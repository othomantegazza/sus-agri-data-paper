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

status_by_fpid <- 
  screening %>% 
  select(
    Search,
    Source_of_search,
    DOI,
    FPID,
    Paper.reviewer,
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

status_by_fpid_DUPL <- 
  status_by_fpid %>% 
  extract_duplicated_rows(id_cols = c('DOI', 'FPID'))

warning("Removing randomly duplicated rows in Screening Results")

status_by_fpid_clean <- 
  status_by_fpid %>% 
  distinct(DOI, FPID, .keep_all = T) %>% 
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
