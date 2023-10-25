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

search_tab <- 
  read_excel(
    'data/MerFPs_Search_20231017.xlsx'
  ) 

search_tab <-
  search_tab %>% 
  mutate(
    Date.of.search.WOS = Date.of.search.WOS %>% 
      as.numeric() %>% 
      as.Date(origin = "1899-12-30")
    )

