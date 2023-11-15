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
library(skimr)
library(here)

# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# Read selected paper data ---------------------------------------

impacts <-
  read_excel(
    'data/MerFPs_Impacts_20231017.xlsx'
  )

impacts %>% 
  slice(
    impacts %>%
      problems() %>% 
      pull(row)
  )

impacts <- 
  impacts %>%   
  clean_names()


# clean geo -----------------------------------------------------

geo_dict <- 
  read_csv(
    "data/metadata/geo-clean.csv"
  )

impacts <- 
  impacts %>% 
  mutate(scale = scale %>% str_to_lower()) %>% 
  left_join(geo_dict) 

# impacts %>% 
#   count(scale, geo_coverage)


# clean metrics -------------------------------------------------

metric_dict <- 
  read_csv(
    "data/metadata/metrics_standardized.csv"
  )

impacts <- 
  impacts %>% 
  left_join(metric_dict,
            by = "metric")

# read column metadata ------------------------------------------

metadata <- 
  read_json(
    'data/metadata/impacts-metadata.json',
    simplifyVector = T,
  ) %>%
  as_tibble() %>% 
  rename(line_type =info_type)


# improve column name -------------------------------------------

impacts <- 
  impacts %>% 
  rename(selection_criteria_reported = selection_criteria_2)

# clean all data and check types---------------------------------

metadata %>% 
  filter(
    name %in% c(
      "05_ma_list",
      "06_ma_quality_assessment",
      "07_ma_synthesis",
      "08_pico_combinations",
      "10_pico_cat_results"
    )
  ) %>% 
mutate(
  df = impacts %>% 
    list()
) %>% 
  pwalk(
    clean_imap
  )
