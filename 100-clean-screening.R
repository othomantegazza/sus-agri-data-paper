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
# library(Diderot)

# functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# read data -----------------------------------------------------

screening <- 
  read_excel(
    'data/MerFPs_Screening_20231017.xlsx'
  )

screening %>% colnames()

screening_status <-
  read_json(
  'data/metadata/screening-status.json',
  simplifyVector = T
) %>% 
  {set_names(
    x = unlist(.),
    nm = names(.)
  )}

# extract screening statuses ------------------------------------

systematic_screening <- 
  screening %>% 
  select(
    DOI,
    FPID,
    Status,
    Year
  ) %>% 
  mutate(Status = Status %>% toupper()) %>% 
  mutate(Status = Status %>% {
    case_when(
      . == "LB" ~ "O",
      . == "LB NOT IN EU" ~ "O",
      . == "Y" ~ "O",
      . == "G" ~ "R",
      TRUE ~ .
    )
  }) %>% 
  mutate(Year = Year %>% as.numeric()) %>% 
  filter(Year > 1950)

systematic_screening_DUPL <- 
  systematic_screening %>% 
  extract_duplicated_rows(id_cols = c('DOI', 'FPID')) %>% 
  filter(! is.na(DOI)) %>%
  write_csv('data/output/4-systematic-screening-DUPL.csv')

warning(
  "Dropping randomly ", 
  nrow(systematic_screening_DUPL) - length(unique(systematic_screening_DUPL$DOI)), 
  " duplicated rows in Screening Results"
)

systematic_screening_clean <- 
  systematic_screening %>% 
  distinct(DOI, FPID, .keep_all = T) %>% 
  mutate(status_details = screening_status[Status]) 
  
systematic_screening_clean %>% 
  write_csv('data/output/4-systematic-screening.csv')

systematic_screening_clean %>% 
  filter_all(
    any_vars(
      is.na(.)
    )
  ) %>% 
  write_csv('data/output/4-systematic-screening-MISSING-DATA.csv')



# screening date ------------------------------------------------

screening_dates <- 
  screening %>% 
  select(FPID, 
         Source_of_search, 
         Data_search) %>% 
  mutate(
    Data_search = Data_search %>%
      as.numeric() %>% 
      as.Date(origin = "1899-12-30")
  ) %>% 
  group_by(FPID) %>% 
  summarise(Data_search = Data_search %>% max(na.rm = T)) %>% 
  mutate(Data_search = Data_search %>% {
    case_when(
      is.infinite(.) ~ NA,
      TRUE ~ .
    )
  }) %>% 
  arrange(Data_search) %>% 
  mutate(part = rep(1:2, each = n()/2)) %>% 
  nest(.by = part)



# viz -----------------------------------------------------------

systematic_screening_clean %>% 
  mutate(
    Year = Year %>% {
      case_when(. <= 1998 ~ "1998 or before",
                TRUE ~ as.character(.))
    }
  ) %>% 
  filter(FPID %in% screening_dates$data[[2]]$FPID) %>% 
  left_join(screening_dates$data[[2]]) %>% 
  mutate(
    FPID = paste(
      Data_search, 
      FPID %>% str_wrap(width = 20),
      sep = "\n"
    )
  ) %>% 
  count(FPID, Year) %>%
  ggplot() +
  aes(x = Year,
      y = n) +
  geom_col(
    fill = 'white',
    colour = 'black',
    size = line_width/2,
    width = 1
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(
    facets = "FPID",
    ncol = 1, 
    strip.position = "left"
    # labeller = label_wrap_gen(width = 15)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.line.y = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.line.x = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
      ),
    panel.spacing = unit(0, 'mm'),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      vjust = 0
      )
  )
