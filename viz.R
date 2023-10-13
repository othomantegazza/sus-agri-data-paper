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
    'data/output/7_ma_synthesis.csv'
  )

pico_cat_results <- 
  read_csv(
    'data/output/8_pico_combinations.csv'
  )

status_by_fpid <-   
  read_csv(
    'data/output/status-by-fpid.csv'
    )

# VIZ -----------------------------------------------------------

p1 <- build_p1(status_by_fpid)

jpeg(filename = "viz/p1.jpeg", 
     width = a4_width, 
     height = a4_height*.75, 
     units = unit_type,
     res = 200)
grid.newpage(); p1 %>% grid.draw()
dev.off()

p2 <- build_p2(synthesis)

jpeg(filename = "viz/p2.jpeg", 
     height  = a4_width, 
     width = a4_height, 
     units = unit_type,
     res = 200)
grid.newpage(); p2 %>% grid.draw()
dev.off()
