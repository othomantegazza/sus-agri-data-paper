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

defs <- 
  read_excel("data/MerFPs_DefinitionFP_20231017.xlsx") %>% 
  clean_names() %>% 
  drop_na(text)

metadata <- 
  read_json(
    'data/metadata/impacts-metadata.json',
    simplifyVector = T,
  ) %>%
  as_tibble() %>% 
  rename(line_type = info_type) %>% 
  filter(name == "01_fp_definitions") 

metadata %>% 
  clean_impacts()


