library(readxl)
library(dplyr)
library(forcats)
library(purrr)
library(jsonlite)

matvis_vars <- jsonlite::read_json("assets/matvis_vars.json", simplifyVector = TRUE)

getFlatsheetData <- function(levels = 1, region = NA, simplify = TRUE) {
  rtn <- purrr::map(levels, function(level) {
    file_path <- paste0("data/Regional_Flatsheets_Level-", level, ".xlsx")
    sheets <-  readxl::excel_sheets(file_path)
    if (!is.na(region)) {
      sheet <- paste(region, paste("Level", level, sep = "-"), sep = "_")
    } else {
      sheet <- sheets[1]
    }
    skip <- ifelse(as.integer(level) == 1, 1, 0)
    tll <- "Traffic light co-impact"
    df <- readxl::read_excel(file_path, sheet = sheet, skip = skip) %>%
      dplyr::mutate(Level = level) %>%
      tidyr::fill(Transition, `Ad/Mit`, Intervention) %>%
      dplyr::mutate(!!tll :=
                      forcats::fct_recode(factor(as.integer(!!as.symbol(tll))),
                                          !!!unlist(matvis_vars$traffic_light_co_impact)),
                    `Traffic light confidence` =
                      forcats::fct_recode(factor(as.integer(`Traffic light confidence`)),
                                          !!!unlist(matvis_vars$traffic_light_confidence)),
                    `Ad/Mit` =
                      forcats::fct_recode(factor(`Ad/Mit`),
                                          !!!unlist(matvis_vars$ad_mit)))
    cs_levels = unlist(matvis_vars$context_sensitivity)
    if (as.integer(level) == 2) {
      # Workaround because SCA_Level-2 sheet uses "Context dependence"
      cs_label = ifelse ("Context sensitivity" %in% names(df),
                         "Context sensitivity",
                         "Context dependence")
      df <- try(dplyr::mutate(df,
                              "Context sensitivity" =
                                forcats::fct_recode(factor(as.integer(!!as.symbol(cs_label))),
                                                    !!!cs_levels)))
      # Handle different naming across level 1 and level 2 flatsheets
      df <- dplyr::mutate(df,
                          Transition = case_when(
                            Transition == "Energy System" ~ "Energy Systems",
                            Transition == "Urban and Infrastructure System" ~
                              "Urban and Infrastructure Systems",
                            TRUE ~ as.character(Transition)
                          ),
                          Intervention = case_when(
                            Intervention == "Energy supply" ~ "Energy supply / distribution",
                            Intervention == "Energy capture/storage" ~ "Carbon capture technologies",
                            Intervention == "Water storage, supply and water use management" ~ "Water storage, supply, and use management",
                            Intervention == "Urban green infrastructure, land use and planning" ~ "Urban green infrastructure, land use, and planning",
                            Intervention == "Urban green planning and land use" ~ "Urban green infrastructure, land use, and planning",
                            Intervention == "Energy efficient infrastructure/buildings" ~ "Energy efficient infrastructure / buildings",
                            Intervention == "Transportation" ~ "Active and electric transportation",
                            Intervention == "Disaster risk reduction / Early warning systems" ~ "Disaster risk reduction / Early warning and response systems",
                            TRUE ~ as.character(Intervention)
                          ))
    }
    return(df)
  })
  if (simplify) {
    return(bind_rows(rtn))
  }
  return(rtn)
}

getOptions <- function(col_name) {
  level = ifelse(col_name == "Context sensitivity", 2, 1)
  fs_data <- getFlatsheetData(level)
  if (col_name %in% names(fs_data)) {
    options <- dplyr::select(fs_data, all_of(col_name)) %>% unlist() %>% unique() %>% sort()
  } else if (col_name == "Region") {
    file_path <- paste0("data/Regional_Flatsheets_Level-1.xlsx")
    sheets <- readxl::excel_sheets(file_path)
    options <- stringr::str_split(sheets, "_", simplify = TRUE)[,1]
    options <- matvis_vars$region[matvis_vars$region %in% options]
  }
  return(options)
}

filterData <- function(level, input) {
  # Get flatsheet data and filter to match inputs
  fdata <- getFlatsheetData(level, input$region) %>%
    dplyr::filter(Transition %in% input$transition,
                  `Ad/Mit` %in% input$ad_mit,
                  `Traffic light co-impact` %in% input$tll,
                  `Traffic light confidence` %in% input$tlc,
                  Level == 1 || `Context sensitivity` %in% input$cs)
  return(fdata)
}

getGroupedData <- function(level, input) {

  # Get flatsheet data and filter to match inputs
  fs_data <- filterData(level, input)

  # Main grouping columns
  group_cols <- c(
    "Transition",
    "Ad/Mit",
    "Intervention",
    "Level"
  )
  if ("Option" %in% names(fs_data)) {
    group_cols <- c(group_cols,
                    "Option")
  }

  # Filter out rows where evidence is lacking across all co-benefit categories
  evidence_summary <- dplyr::group_by(fs_data, !!!syms(group_cols)) %>%
    dplyr::filter(`Traffic light co-impact` != "Limited or no evidence") %>%
    dplyr::summarise(evidence_count = n())

  fs_data <- dplyr::right_join(fs_data, evidence_summary) %>%
    dplyr::select(-evidence_count)

  # Add co-impact categories to grouping
  group_cols_ext <- c(group_cols, "Co-impact category")

  # Nest co-impact info then pivot to produce co-impact columns, with a
  # specified order
  nested_data <- dplyr::nest_by(fs_data, !!!syms(group_cols_ext),
                                .key = "Co-benefits") %>%
    tidyr::pivot_wider(names_from = `Co-impact category`,
                       values_from = `Co-benefits`) %>%
    dplyr::relocate(any_of(matvis_vars$co_benefits),
                    .after = matches(group_cols[-1]))

  # Combine intervention information
  nested_data <- dplyr::group_by(nested_data,
                                 Transition,
                                 Intervention,
                                 Level,
                                 `Ad/Mit`) %>%
    dplyr::mutate(Intervention = ifelse(Level == 1, Intervention, Option)) %>%
    dplyr::select(-any_of(c("Option"))) %>%
    tidyr::unite("Intervention",
                 `Ad/Mit`,
                 `Intervention`,
                 sep = ";")

  list(title = names(matvis_vars$region[matvis_vars$region == input$region]),
       data = nested_data, groups = group_cols_ext)
}
