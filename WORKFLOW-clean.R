library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(readxl)
library(readr)
library(janitor)
library(glue)
library(purrr)
library(jsonlite)
library(here)
library(paletteer)
library(lubridate)


# data files ----------------------------------------------------

make_path <- function(type, dstamp) {
  fname <- glue(type, dstamp, ".xlsx")
  path <- here("data", fname)
  return(path)
}

dstamp <- "20240122"

definitions <- make_path('MerFPs_DefinitionFP_', dstamp)
searches <- make_path('MerFPs_Search_', dstamp)
criteria <- make_path('MerFPs_Criteria_', dstamp)
screening <- make_path('MerFPs_Screening_', dstamp)
metric_harmonized <- here('data/MerFPs_Impacts_20231004_metrics harmonisation.xlsx')
impacts <-  make_path('MerFPs_Impacts_', dstamp)

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>%
  walk(source)

# clean all data ------------------------------------------------

clean_identifiers(
  definitions
)

clean_search(
  searches
)

clean_criteria(
  criteria
)

clean_screening(
  screening
)

clean_extract_standard_metric(
  df = metric_harmonized,
  sheet = 'harmonisation'
)

clean_extract_factors(
  impacts
)

clean_impacts(
  impacts
)

