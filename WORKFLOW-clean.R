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
library(here)
library(lubridate)

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>%
  walk(source)
