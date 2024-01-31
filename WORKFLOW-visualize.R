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

delim <- ";"

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
  read_delim(
    "data/output/imap/02_search_eq.csv",
    delim = delim
  ) %>% 
  summarise(
    nb_papers_wos = nb_papers_wos %>%
      sum(na.rm = T),
    nb_papers_scopus = nb_papers_scopus %>%
      sum(na.rm = T),
    nb_papers_after_merging = nb_papers_scopus %>%
      sum(na.rm = T),
    date_of_search_wos = date_of_search_wos %>%
      as_date() %>% max(na.rm = T),
    date_of_search_scopus = date_of_search_scopus %>%
      as_date() %>% max(na.rm = T),
    .by = fpid
  )

screening <-   
  read_delim(
    'data/output/imap/04_screeening.csv',
    delim = delim
  ) %>% 
  left_join(
    statuses
  ) %>% 
  mutate(year)

screening_dates <-
  # read_csv("data/output/03-search-dates.csv") %>% 
  # mutate(
  #   across(
  #     date_of_search,
  #     lubridate::as_date
  #   )
  # )
  search_tab %>% 
  select(fpid, date_of_search = date_of_search_wos)

ma_list <- 
  read_delim(
    'data/output/imap/05_ma_list.csv',
    delim = delim
  )

synthesis <- 
  read_delim(
    'data/output/imap/07_ma_synthesis.csv',
    delim = delim,
    quote = "\""
  )

pico_combinations <- 
  read_delim(
    'data/output/imap/08_pico_combinations.csv',
    delim = delim
  )

pico_results <- 
  read_delim(
    'data/output/imap/10_pico_cat_results.csv',
    delim = delim
  )

review_log <- 
  read_delim(
    "data/output/imap/11-review-log.csv",
    delim = delim
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
  ) %>% 
  strip_plot()

# |- plot papers by meta analyses -------------------------------

p_papers_by_ma <- 
  build_papers_by_ma(
    synthesis = synthesis
  ) %>% 
  strip_plot()

# |- plot geographic coverage -----------------------------------

p_geo <- 
  build_p_geo(
    pico_combinations, 
    fill_color = fill_color
  ) %>% 
  strip_plot()

# put together the three bar charts -----------------------------

bars_inner <- 
  gtable(
    widths = unit(1, "null"),
    heights = unit(c(1, 1), "null")
  ) %>% 
  gtable_add_grob(
    p_selected_years, 
    t = 1, l = 1
  ) %>% 
  gtable_add_grob(
    p_papers_by_ma, 
    t = 2, l = 1
  ) %>% 
  gtable_add_row_space(padding)

# check_plot(bars_inner)

bars_outer <- 
  gtable(
    widths = unit(c(1, 1), "null"),
    heights = unit(1, "null")
  ) %>% 
  gtable_add_grob(
    p_geo, 
    t = 1, l = 1
  ) %>% 
  gtable_add_grob(
    bars_inner, 
    t = 1, l = 2
  ) %>% 
  gtable_add_col_space(width = padding) %>% 
  gtable_add_padding(padding)
  
# check_plot(bars_outer)

jpeg(filename = "viz/p-bars.jpeg", 
     width = a4_width, 
     height = a4_height*.6, 
     units = unit_type,
     res = ppi)
grid.newpage(); bars_outer %>% grid.draw()
dev.off()



# |- plot pico overview --------------------------------------------

p_pico_overview <- 
  build_pico_overview(
    pico_results
  )

jpeg(filename = "viz/p-pico-overview.jpeg", 
     height  = a4_height*.6, 
     width = a4_width, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_pico_overview %>% grid.draw()
dev.off()

# |- plot pico overview by impact ---------------------------------

p_pico_impact_overview <- 
  build_pico_overview(
    pico_results,
    id_col = impact_matrix,
    ylab = "Impact categories",
    split_at = "(LCA)"
  )

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


# |- plot review log --------------------------------------------

p_review_log <- 
  build_p_review_log(
    review_log = review_log,
  ) 

jpeg(filename = "viz/p-review-log.jpeg", 
     height  = a4_height*.6, 
     width = a4_width*.75, 
     units = unit_type,
     res = ppi)
grid.newpage(); p_review_log %>% grid.draw()
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

