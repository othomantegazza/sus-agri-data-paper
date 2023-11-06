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
library(paletteer)
library(grid)
library(gtable)
library(gridtext)

screening <-   
  read_csv(
    'data/output/4-systematic-screening.csv'
  ) %>% 
  clean_names()

synthesis <- 
  read_csv(
    'data/output/7_ma_synthesis.csv'
  ) %>% 
  select(doi, fpid) %>% 
  distinct()

pico <- 
  read_csv(
    "data/output/9_pico_cat_result.csv"
  )

# select ma selected without blue status ------------------------

screening_blue <- 
  screening %>% 
  select(-status_details) %>% 
  filter(status == "B")

missing_screening <- 
  synthesis %>% 
  left_join(
    screening_blue
  ) %>% 
  filter(is.na(status)) %>% 
  select(-status, -year)

missing_screening %>% 
  write_csv(
    "data/output/missing-in-screening.csv"
  )

missing_impacts <- 
  screening_blue %>% 
  anti_join(
    synthesis
  )

missing_impacts %>% 
  write_csv(
    "data/output/missing-in-impacts.csv"
  )

# missing g lines -----------------------------------------------

missing_in_synthesis <- 
  pico %>% 
  anti_join(
    synthesis,
    by = c("doi", "fpid")
  ) %>% 
  select("doi", "fpid")

missing_in_synthesis %>% 
  write_csv(
    "data/output/missing-G-rows.csv"
  )
