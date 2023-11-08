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


screening <-   
  read_csv(
    'data/output/4-systematic-screening.csv'
  )

ma_list <- 
  read_csv(
    'data/output/5_ma_list.csv'
  )

synthesis <- 
  read_csv(
    'data/output/7_ma_synthesis.csv'
  )

pico_combinations <- 
  read_csv(
    'data/output/8_pico_combinations.csv'
  )

pico_results <- 
  read_csv(
    'data/output/9_pico_cat_result.csv'
  )


# VIZ -----------------------------------------------------------

p_geo <- build_p_geo(pico_combinations)

p1 <- build_p1(screening)

jpeg(filename = "viz/p1.jpeg", 
     width = a4_width, 
     height = a4_height*.75, 
     units = unit_type,
     res = 200)
grid.newpage(); p1 %>% grid.draw()
dev.off()

p2 <- build_p2(synthesis, pico_combinations)

jpeg(filename = "viz/p2.jpeg", 
     height  = a4_width, 
     width = a4_height, 
     units = unit_type,
     res = 200)
grid.newpage(); p2 %>% grid.draw()
dev.off()

p3 <- build_p3(ma_list,
               cutoff_year = 2021)

jpeg(filename = "viz/p3.jpeg", 
     height  = a4_height*.25, 
     width = a4_width*.5, 
     units = unit_type,
     res = 200)
p3
dev.off()

p4 <- build_p4(synthesis, pico_results)
