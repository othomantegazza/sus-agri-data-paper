library(tidyverse)
library(readxl)

read_tsv("data/output/02_search_eq - Sheet1.tsv") %>% 
  write_excel_csv2("data/output/imap/02_search_eq.csv", escape = "double")

factors <- read_csv(file = "data/output/imap/09_factors.csv")

factors_andrea <-
  read_excel(
  "data/09_factors_Classified.xlsx"
) %>% 
  janitor::clean_names() %>% 
  mutate(factor_nb = factor_nb %>% as.numeric())

factors_andrea <- 
  factors_andrea %>% 
  select(doi,
         fpid,
         impact_matrix,population_variable_tier1,
         factor_nb,
         population_variable_tier2,
         population_variable_tier3)

factors_out <- 
  factors %>% 
  left_join(factors_andrea)


factors_out %>% 
  write_excel_csv2("data/factors-classified.csv")
