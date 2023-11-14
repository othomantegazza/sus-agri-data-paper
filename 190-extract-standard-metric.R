library(tidyverse)
library(readxl)
library(janitor)

mt_std <-
  read_excel(
    "data/MerFPs_Impacts_20231004_metrics harmonisation.xlsx", 
    sheet = "harmonisation"
  ) %>%
  clean_names() %>% 
  filter(metric_matrix != "(blank)") %>% 
  select(metric, metric_std) %>% 
  distinct() %>% 
  drop_na()


mt_std %>% 
  filter(
    metric %in% {
      mt_std %>% 
        count(metric) %>% 
        filter(n > 1) %>% 
        pull(metric)
    }) %>% 
  arrange(metric) %>% 
  write_csv(
    "data/output/diagnostics/metric_standard_duplicated.csv"
  )

mt_std %>% 
  distinct(metric, .keep_all = T) %>% 
  write_csv(
    "data/metadata/metrics_standardized.csv"
  )

