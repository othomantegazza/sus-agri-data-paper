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
    'data/MerFPs_Screening_20231017.xlsx'
  )

screening %>% colnames()

screening_status <-
  read_json(
  'data/metadata/screening-status.json',
  simplifyVector = T
) %>% 
  {set_names(
    x = unlist(.),
    nm = names(.)
  )}

# make figures --------------------------------------------------

systematic_screening <- 
  screening %>% 
  select(
    # Search,
    # Source_of_search,
    DOI,
    FPID,
    Status
  ) %>% 
  mutate(Status = Status %>% toupper()) %>% 
  mutate(Status = Status %>% {
    case_when(
      . == "LB" ~ "O",
      . == "LB NOT IN EU" ~ "O",
      . == "Y" ~ "O",
      . == "G" ~ "R",
      TRUE ~ .
    )
  })

systematic_screening_DUPL <- 
  systematic_screening %>% 
  extract_duplicated_rows(id_cols = c('DOI', 'FPID')) %>% 
  filter(! is.na(DOI)) %>%
  write_csv('data/output/4-systematic-screening-DUPL.csv')

warning(
  "Dropping randomly ", 
  nrow(systematic_screening_DUPL) - length(unique(systematic_screening_DUPL$DOI)), 
  " duplicated rows in Screening Results"
)

systematic_screening_clean <- 
  systematic_screening %>% 
  distinct(DOI, FPID, .keep_all = T) %>% 
  mutate(status_details = screening_status[Status]) 
  
systematic_screening_clean %>% 
  write_csv('data/output/4-systematic-screening.csv')

systematic_screening_clean %>% 
  filter_all(
    any_vars(
      is.na(.)
    )
  ) %>% view()
  write_csv('data/output/4-systematic-screening-MISSING-DATA.csv')


status_by_fpid_clean %>%  
  write_csv(
    'data/output/status-by-fpid.csv'
  )

