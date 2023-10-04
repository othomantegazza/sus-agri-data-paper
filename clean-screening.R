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

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# read data -----------------------------------------------------

screening <- 
  read_excel(
    'data/MerFPs_Screening_20230810.xlsx'
  )



screening %>% colnames()

screening_status <- read_json(
  'data/metadata/screening-status.json',
  simplifyVector = T
) %>% 
  {set_names(
    x = unlist(.),
    nm = names(.)
  )}

# make figures --------------------------------------------------

selection <- 
  screening %>% 
  select(
    Search,
    Source_of_search,
    DOI,
    FPID,
    Paper.reviewer,
    Status
  ) %>% 
  summarise(
    n = n(),
    .by = c(FPID, Status)
  ) %>% view()
