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

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>%
  walk(source)

# clean all data ------------------------------------------------

clean_identifiers(
  'data/MerFPs_DefinitionFP_20231017.xlsx'
)

clean_search(
  'data/MerFPs_Search_20231017.xlsx'
)

clean_criteria(
  'data/MerFPs_Criteria_20231017.xlsx'
)

clean_screening(
  'data/MerFPs_Screening_20231017.xlsx'
)

clean_extract_standard_metric(
  df = 'data/MerFPs_Impacts_20231004_metrics harmonisation.xlsx',
  sheet = 'harmonisation'
)

clean_impacts(
  'data/MerFPs_Impacts_20231017.xlsx'
)
