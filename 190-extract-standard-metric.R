library(tidyverse)
library(readxl)
library(janitor)

mt_sd <-
  read_excel(
    "data/MerFPs_Impacts_20231004_metrics harmonisation.xlsx", 
    sheet = "harmonisation"
  ) %>%
  clean_names() %>% 
  select(metric, metric_std) %>% 
  distinct() %>% 
  drop_na()


mt_sd %>% 
  write_csv(
    "data/metadata/metrics_standardized.csv"
  )