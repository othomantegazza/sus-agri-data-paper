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
library(lubridate)

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)


# read data -----------------------------------------------------

criteria <- 
  read_excel(
    'data/MerFPs_Criteria_20231017.xlsx'
  ) %>% 
  clean_names()

# metadata ------------------------------------------------------

metadata <- 
  read_json(
    'data/metadata/impacts-metadata.json',
    simplifyVector = T,
  ) %>%
  as_tibble() %>% 
  rename(line_type = info_type) %>% 
  filter(name == "03_search_criteria")

# clean ---------------------------------------------------------

criteria <- 
  criteria %>% 
  select(
    fpid,
    criteria_for_exclusion,
    criteria_for_inclusion
  ) %>% 
  pivot_longer(-fpid,
               names_to = "used_for",
               values_to = "criteria") %>% 
  drop_na(criteria) %>% 
  mutate(used_for = used_for %>% str_remove("criteria_for_")) %>% 
  arrange(fpid, used_for)

metadata %>% 
  mutate(df = criteria %>% list()) %>% 
  pwalk(
    clean_imap
  )

