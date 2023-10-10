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
library(paletteer)
library(grid)
library(gtable)
library(gridtext)

# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# DATA ----------------------------------------------------------

synthesis <- 
  read_csv(
    'data/output/selected-paper-synthesis.csv'
  )

pico_cat_results <- 
  read_csv(
    'data/output/pico-cat-results.csv'
  )

status_by_fpid <-   
  read_csv(
    'data/output/status-by-fpid.csv'
    )


# VIZ -----------------------------------------------------------

# 

p1 <- build_p1(status_by_fpid)

grid.newpage(); p1 %>% grid.draw()

p2 <- build_p2(synthesis)

grid.newpage(); p2 %>% grid.draw()
