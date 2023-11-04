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

# select ma selected without blue status ------------------------

screening_blue <- 
  screening %>% 
  select(-status_details) %>% 
  filter(status == "B")

synthesis %>% 
  left_join(
    screening_blue
  ) %>% 
  filter(is.na(status))

screening_blue %>% 
  anti_join(
    synthesis
  ) 
