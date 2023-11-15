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

# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# data ----------------------------------------------------------

defs <- 
  read_excel("data/MerFPs_DefinitionFP_20231017.xlsx") %>% 
  clean_names() %>% 
  drop_na(text) %>% 
  rename(type = x1)

# metadata ------------------------------------------------------

metadata <- 
  read_json(
    'data/metadata/impacts-metadata.json',
    simplifyVector = T,
  ) %>%
  as_tibble() %>% 
  rename(line_type = info_type) %>% 
  filter(name == "01_fp_definitions")

# clean ---------------------------------------------------------

metadata %>% 
  mutate(df = defs %>% list()) %>% 
  pwalk(
    clean_imap
  )


