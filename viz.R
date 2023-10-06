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


# GRAPH SETUP ---------------------------------------------------

unit_type <- 'mm'
line_width <- 1.2
width <- 150
height <- width
base_size <- 10
categorical_palette <- paletteer_d("futurevisions::atomic_blue")
colours_dia <- categorical_palette[2:3]
# colour_base <- categorical_palette[2]
colour_base <- '#3982a4'
size_scale <- 2.8455
text_size_plot <- base_size/size_scale

theme_set(
  theme_minimal(
    base_size = base_size,
  ) +
    theme(
      plot.background = element_rect(fill = 'white',
                                     colour = 'white'),
      plot.margin = margin(20,5,5,5),
      axis.text.x = element_text(hjust = .5,
                                 vjust = .5,
                                 colour = "black",
                                 size = base_size),
      axis.text.y = element_text(vjust = 0.5,
                                 colour = "black",
                                 size = base_size),
      axis.ticks = element_line(),
      axis.line = element_line(linewidth = line_width),
      legend.position = "bottom",
      panel.grid = element_line(
        colour = 'black',
        size = line_width*.1,
        linetype = '11'
      ),
      panel.grid.minor = element_blank()
    )
)


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

fpid_ordered <-  
  synthesis %>% 
  count(fpid,
        sort = T) %>% 
  pull(fpid)

matrix_ordered <- 
  synthesis %>% 
  count(impact_matrix,
        sort = T) %>% 
  pull(impact_matrix)


synthesis %>% 
  count(fpid, impact_matrix) %>% 
  ggplot() +
  aes(x = impact_matrix %>% 
        factor(
          levels = matrix_ordered
        ),
      y = fpid %>% factor(
        levels = fpid_ordered) %>%
        fct_rev(),
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
  labs(x = NULL,
       y = NULL) +
  guides(fill = 'none') +
  scale_fill_viridis_c(
    direction = -1,
    option = 'G',
    na.value = '#00000000',
  ) +
  scale_colour_manual(values = c(white = 'white',
                                 black = 'black')) +
  theme(
    axis.text.x = element_text(
      angle = 90, hjust = 1, vjust = .5
    ),
    panel.grid = element_line(
      colour = 'black',
      size = line_width*.1,
      linetype = '11'
    )
  )

pico_cat_results %>% 
  count(fpid, sort = T) %>% 
  ggplot() +
  aes(x = n,
      y = fpid) +
  geom_col()


# VIZ SCREEN ----------------------------------------------------

fpid_ordered_screening <- 
  status_by_fpid %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    .by = FPID
  ) %>% 
  arrange(desc(n)) %>% 
  pull(FPID) 

screening_levels <- 
  c(
    'After Web Search',
    'After Reading Abstract',
    'After Reading Full Text'
  )

status_by_fpid <- 
  status_by_fpid %>% 
  mutate(
    short_description = Status %>% {
      case_when(. == 'R' ~ screening_levels[1],
                . == 'O' ~ screening_levels[2],
                . == 'B' ~ screening_levels[3]
      )
    } %>% 
      factor(levels = screening_levels)
  )

status_by_fpid %>% 
  arrange(FPID, Status) %>% 
  mutate(tot_before_step = n %>% cumsum(),
         .by = FPID) %>% 
  ggplot() +
  aes(
    x = tot_before_step,
    y = FPID %>%
      factor(levels = fpid_ordered_screening) %>%
      fct_rev()) +
  geom_col(
    fill = 'white',
    colour = 'black',
    size = line_width/2,
    width = 1
  ) +
  geom_text(
    aes(
      x = tot_before_step + 20,
      label = tot_before_step
    ),
    size = text_size_plot,
    hjust = 0
  ) +
  geom_vline(
    xintercept = 0,
    size = line_width
    ) +
  facet_grid(
    . ~ short_description,
    scales = 'free_x',
    space = 'free_x',
    labeller = label_wrap_gen(width = 12)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0, 100)),
    # breaks = ~seq(0, .[2], by = 200),
    limits = ~c(0, ifelse(.[2] > 150, .[2], 150))
  ) +
  theme(
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.spacing = unit(0, 'mm'),
    strip.text = element_text(size = base_size,
                              hjust = 0)
  )
  