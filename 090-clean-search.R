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
  ) %>% 
  clean_names()
  

search_tab <-
  search_tab %>% 
  mutate(
    across(
      .cols = c("date_of_search_wos",
                "date_of_search_scopus"),
      .fns = ~as.numeric(.) %>% 
        as.Date(origin = "1899-12-30")
    )
  ) 

search_tab <- 
  search_tab %>% 
  select(
    fpid,
    x1,
    nb_papers_scopus,
    date_of_search_scopus,
    nb_papers_wos,
    date_of_search_wos,
    nb_papers_after_merging
  ) %>% 
  summarise(
    across(
      .cols = c(
        nb_papers_scopus,
        date_of_search_scopus,
        nb_papers_wos,
        date_of_search_wos,
        nb_papers_after_merging
      ),
      .fns = ~max(., na.rm = T)
    ),
    .by = fpid
  )

search_tab %>% 
  write_csv(
    "data/output/03-search.csv"
  )

search_dates <-
  search_tab %>% 
  select(fpid, date_of_search_wos, date_of_search_scopus) %>% 
  pivot_longer(cols = !fpid) %>% 
  select(-name) %>% 
  group_by(fpid) %>% 
  summarise(
    across(
      .cols = everything(),
      .fns = ~max(., na.rm = T)
    )
  ) %>% 
  arrange(value) %>% 
  rename(date_of_search = value)

search_dates %>% 
  write_csv(
    "data/output/03-search-dates.csv"
  )
