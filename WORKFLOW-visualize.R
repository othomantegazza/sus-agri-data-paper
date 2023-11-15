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


# metadata ------------------------------------------------------

statuses <- 
  read_json(
    "data/metadata/screening-status.json"
  ) %>% 
  {
    tibble(
      status = names(.),
      status_details = unlist(.)
    )
  }

# DATA ----------------------------------------------------------

search_tab <- 
  read_csv(
    "data/output/03-search-summarised.csv"
  )

screening <-   
  read_csv(
    'data/output/imap/04_screeening.csv'
  ) %>% 
  left_join(
    statuses
  ) 

ma_list <- 
  read_csv(
    'data/output/imap/05_ma_list.csv'
  )

synthesis <- 
  read_csv(
    'data/output/imap/07_ma_synthesis.csv'
  )

pico_combinations <- 
  read_csv(
    'data/output/imap/08_pico_combinations.csv'
  )

pico_results <- 
  read_csv(
    'data/output/imap/10_pico_cat_results.csv'
  )

# VIZ -----------------------------------------------------------


# |- plot geographic coverage -----------------------------------

p_geo <- 
  build_p_geo(
    pico_combinations, 
    fill_color = fill_color
  )

jpeg(filename = "viz/p-geo.jpeg", 
     width = a4_width/2, 
     height = a4_height/2, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_geo %>% grid.draw()
dev.off()

# |- Prisma Statements for each FPID ----------------------------

p_prisma <- 
  build_p_prisma(
    search_tab = search_tab, 
    screening = screening
  )

pdf("viz/prisma.pdf",
    width = a4_width*mm_to_in,
    height = a4_height*mm_to_in)
for(p in p_prisma) {
  grid.newpage()
  p %>% grid.draw()
}
dev.off()

# |- Plot Screening Results -------------------------------------

p_screening <- 
  build_p_screening(
    screening,
    fill_color = fill_color
  )

jpeg(filename = "viz/p-screening.jpeg", 
     width = a4_width, 
     height = a4_height*.65, 
     units = unit_type,
     res = ppi)
grid.newpage()
p_screening %>% grid.draw()
dev.off()


# |- plot pico combinations ----------------------------------------


p2 <- build_p2(synthesis, pico_combinations)

jpeg(filename = "viz/p2.jpeg", 
     height  = a4_width, 
     width = a4_height, 
     units = unit_type,
     res = ppi)
grid.newpage(); p2 %>% grid.draw()
dev.off()

p3 <- build_p3(ma_list,
               cutoff_year = 2021)

jpeg(filename = "viz/p3.jpeg", 
     height  = a4_height*.25, 
     width = a4_width*.5, 
     units = unit_type,
     res = ppi)
p3
dev.off()

p4 <- build_p4(synthesis, pico_results)