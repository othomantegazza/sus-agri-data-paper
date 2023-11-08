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
library(gtable)
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
  read_csv("data/output/3-search-dates.csv")


screening_dates <- 
  screening_dates %>% 
  mutate(date_of_search = date_of_search %>% {
    case_when(
      is.infinite(.) ~ NA,
      TRUE ~ .
    )
  }) %>% 
  arrange(date_of_search) %>% 
  mutate(part = rep(1:2, each = n()/2)) %>% 
  nest(.by = part)

systematic_screening_clean <- 
  systematic_screening_clean %>% 
  mutate(
    Year = Year %>% {
      case_when(. <= 2004 ~ "< 2004",
                TRUE ~ as.character(.))
    }
  ) %>% 
  mutate(Year = as.factor(Year))

# viz -----------------------------------------------------------

plot_screening_year <- function(part) {
  p <-
    systematic_screening_clean %>% 
    filter(
      FPID %in% screening_dates$data[[part]]$fpid
    ) %>% 
    left_join(
      screening_dates$data[[part]],
      by = c("FPID" = "fpid")
    ) %>% 
    mutate(
      FPID = paste(
        date_of_search, 
        FPID %>% str_wrap(width = 20),
        sep = "\n"
      )
    ) %>% 
    count(FPID, Year) %>%
    complete(FPID, Year, fill = list(n = 0)) %>% 
    ggplot() +
    aes(x = Year,
        y = n) +
    geom_col(
      fill = 'black',
      colour = 'black',
      size = line_width/2,
      width = 1
    ) +
    geom_hline(yintercept = 0, 
               # linewidth = 0,
               colour = "white") +
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
    scale_y_continuous(position = "right",
                       breaks = c(0, 50, 100), 
                       limits = c(0, NA)) +
    theme(
      axis.line.y = element_blank(),
      # panel.grid.major.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = .5
      ),
      axis.text.y = element_text(
        vjust = 0
      ),
      panel.spacing = unit(0, 'mm'),
      strip.text.y.left = element_text(
        angle = 0,
        hjust = 1,
        vjust = 0
      )
    ) 
  return(p)
}

p1 <- plot_screening_year(1) %>% ggplotGrob()
p2 <- plot_screening_year(2) %>% ggplotGrob()

p <- 
  gtable() %>% 
  gtable_add_rows(heights = unit(25, "cm")) %>% 
  gtable_add_cols(widths = unit(c(10, 10), "cm")) %>% 
  gtable_add_grob(grobs = p1, l = 1, t = 1) %>%
  gtable_add_grob(grobs = p2, l = 2, t = 1)

p %>% gtable_show_layout()

grid.newpage();grid.draw(p)


# 2nd version ---------------------------------------------------

systematic_screening_clean %>% 
  left_join(
    screening_dates %>% unnest(cols = c(data)),
    by = c("FPID" = "fpid")
  ) %>% 
  count(FPID, date_of_search, Year) %>% 
  arrange(desc(date_of_search)) %>% 
  mutate(
    FPID = FPID %>% 
      paste(date_of_search, sep = " - ") %>%
      as_factor()
  ) %>% 
  ggplot() +
  aes(x = Year,
      y = FPID,
      fill = n) +
  geom_tile() +
  geom_text(
    aes(label = n,
        colour = case_when(
          n > 30 ~ 'white',
          TRUE ~ 'black'
        )
    ),
    size = text_size_plot,
    show.legend = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    fill = 'none'
  ) +
  scale_x_discrete(
    position = 'top'
  ) +
  scale_fill_viridis_c(
    direction = -1,
    option = 'G',
    na.value = '#00000000',
  ) +
  scale_colour_manual(
    values = c('white',
               'black') %>% 
      set_names()
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x.top = element_text(
      angle = 90,
      hjust = 0, 
      vjust = .5
    ),
    axis.ticks = element_line(
      size = line_width*.2
    ),
    panel.grid = element_line(
      colour = 'black',
      size = line_width*.1,
      linetype = '11'
    )
  )
