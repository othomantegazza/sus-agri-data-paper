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
  mutate(Status = Status %>% {
    case_when(
      . == "LB" ~ "O",
      . == "Y" ~ "O",
      . == "G" ~ "R",
      TRUE ~ .
    )
  })

systematic_screening_clean <- 
  systematic_screening %>% 
  distinct(DOI, FPID, .keep_all = T) 

systematic_screening_clean %>% 
  write_csv('data/output/4-systematic-screening.csv')

systematic_screening_DUPL <- 
  systematic_screening %>% 
  extract_duplicated_rows(id_cols = c('DOI', 'FPID')) %>% 
  filter(! is.na(DOI)) %>%
  write_csv('data/output/4-systematic-screening-DUPL.csv')

warning("Dropping randomly ", nrow(status_by_fpid_DUPL), " duplicated rows in Screening Results")

status_by_fpid_clean <- 
  status_by_fpid %>% 
  # distinct(DOI, FPID, .keep_all = T) %>% 
  mutate(Status = Status %>% toupper()) %>% 
  summarise(
    n = n(),
    .by = c(FPID, Status)
  ) %>% 
  filter(Status %in% names(screening_status)) %>% 
  mutate(status_details = screening_status[Status],
         .before = 'n') 

status_by_fpid_clean %>%  
  write_csv(
    'data/output/status-by-fpid.csv'
  )

