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

definitions <- here('data/MerFPs_DefinitionFP_20231017.xlsx')
searches <- here('data/MerFPs_Search_20231017.xlsx')
criteria <- here('data/MerFPs_Criteria_20231017.xlsx')
screening <- here('data/MerFPs_Screening_20231017.xlsx')
metric_harmonized <- here('data/MerFPs_Impacts_20231004_metrics harmonisation.xlsx')
impacts <- here("data/MerFPs_Impacts_20231017.xlsx")

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
