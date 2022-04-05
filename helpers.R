library(readxl)
library(dplyr)
library(forcats)
library(purrr)

file_path <- "~/Projects/LBF2/data/Copy of flatsheet of matrix for visualisation v.1.xlsx"

traffic_light_levels = c(
  "Limited or no evidence" = "0",
  "Trade-offs" = "1",
  "Mixed" = "2",
  "Benefits" = "3"
)

traffic_light_confidence <- c(
  "Limited or no evidence" = "0",
  "Very low confidence" = "1",
  "Low confidence" = "2",
  "Medium confidence" = "3",
  "High confidence" = "4"
)

context_dependence <- c(
  "Unknown or no evidence" = "0",
  "Low context dependence" = "1",
  "Moderate context dependence" = "2",
  "High context dependence" = "3"
)

getFlatsheetData <- function(levels = 1, region = NA, simplify = TRUE) {
  rtn <- purrr::map(levels, function(level) {
    sheet = paste("Flatsheet level", level)
    df <- readxl::read_excel(file_path, sheet = sheet) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(`Cell code` = as.integer(`Cell code`),
                    `Traffic light level` = 
                      forcats::fct_recode(factor(as.integer(`Traffic light level`)),
                                          !!!traffic_light_levels),
                    `Traffic light confidence` = 
                      forcats::fct_recode(factor(as.integer(`Traffic light confidence`)),
                                          !!!traffic_light_confidence))
    if (as.integer(level) == 2) {
      df <- dplyr::mutate(df, `Context dependence` =
                            forcats::fct_recode(factor(as.integer(`Context dependence`)),
                                                !!!context_dependence),
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
    dplyr::select(col_name) %>% unlist() %>% unique() %>% sort()
}
