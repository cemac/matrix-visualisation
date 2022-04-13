library(readxl)
library(dplyr)
library(forcats)
library(purrr)
library(jsonlite)

file_path <- "data/Copy of flatsheet of matrix for visualisation v.1.xlsx"

matvis_vars <- jsonlite::read_json("assets/matvis_vars.json", simplifyVector = TRUE)

getFlatsheetData <- function(levels = 1, region = NA, simplify = TRUE) {
  rtn <- purrr::map(levels, function(level) {
    sheet = paste("Flatsheet level", level)
    df <- readxl::read_excel(file_path, sheet = sheet) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(`Cell code` = as.integer(`Cell code`),
                    `Traffic light level` = 
                      forcats::fct_recode(factor(as.integer(`Traffic light level`)),
                                          !!!unlist(matvis_vars$traffic_light_level)),
                    `Traffic light confidence` = 
                      forcats::fct_recode(factor(as.integer(`Traffic light confidence`)),
                                          !!!unlist(matvis_vars$traffic_light_confidence)))
    cd_levels = unlist(matvis_vars$context_dependence)
    if (as.integer(level) == 2) {
      df <- dplyr::mutate(df, `Context dependence` =
                            forcats::fct_recode(factor(as.integer(`Context dependence`)),
                                                !!!cd_levels),
                          Reference = NA)
    }
    if (!is.na(region)) {
      df <- dplyr::filter(df, Region == region)
    }
    return(df)
  })
  if (simplify && length(rtn) == 1) {
    return(rtn[[1]])
  }
  return(rtn)
}

getOptions <- function(col_name) {
  getFlatsheetData() %>%
    dplyr::select(all_of(col_name)) %>% unlist() %>% unique() %>% sort()
}
