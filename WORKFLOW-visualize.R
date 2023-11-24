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
library(lubridate)
library(ggnewscale)
library(cowplot)
library(santoku)

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
  ) %>% 
  mutate(year)

screening_dates <-
  read_csv("data/output/03-search-dates.csv") %>% 
  mutate(
    across(
      date_of_search,
      lubridate::as_date
    )
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

# |- plot papers screened by search and publ year ---------------

p_screening_by_year <- build_p_screening_by_year(
  screening = screening,
  screening_dates = screening_dates
)

jpeg(filename = "viz/p-screening-years.jpeg", 
     width = a4_width, 
     height = a4_height*.6, 
     units = unit_type,
     res = ppi)
p_screening_by_year %>% grid.draw()
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

# |- plot year of selected papers ------------------------------

p_selected_years <- 
  build_selected_years(
    ma_list,
    screening_dates = screening_dates
  )

jpeg(filename = "viz/p-selected-years.jpeg", 
     height  = a4_height*.3, 
     width = a4_width*.5, 
     units = unit_type,
     res = ppi)
p_selected_years
dev.off()


# |- plot papers by meta analyses -------------------------------

p_papers_by_ma <- 
  build_papers_by_ma(
    synthesis = synthesis
  )

jpeg(filename = "viz/p-papers-by-ma.jpeg", 
     height  = a4_height*.3, 
     width = a4_width*.5, 
     units = unit_type,
     res = ppi)
p_papers_by_ma
dev.off()

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

# |- plot pico overview --------------------------------------------

p_pico_overview <- 
  build_pico_overview(pico_results)

jpeg(filename = "viz/p-pico-overview.jpeg", 
     height  = a4_height*.6, 
     width = a4_width, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_pico_overview %>% grid.draw()
dev.off()

# |- plot pico overview by impact ---------------------------------

p_pico_impact_overview <- 
  build_pico_impact_overview(pico_results)

jpeg(filename = "viz/p-pico-impact-overview.jpeg", 
     height  = a4_height*.6, 
     width = a4_width, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_pico_impact_overview %>% grid.draw()
dev.off()


# |- plot impacts by fpid -------------------------------------

p_impacts_fpid <- 
  build_p_impacts_fpid(
    synthesis,
    pico_combinations
  )

jpeg(filename = "viz/p-impacts-by-fpid.jpeg", 
     height  = a4_width, 
     width = a4_height, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_impacts_fpid %>% grid.draw()
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


# p4 <- build_p4(synthesis, pico_results)
