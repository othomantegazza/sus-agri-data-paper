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

p1 <- build_p1(status_by_fpid)

grid.newpage(); p1 %>% grid.draw()

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


impact_matrix_by_fpid <- 
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
    # axis.line = element_line(
    #   size = line_width*.2
    # ),
    axis.line = element_blank(),
    axis.text.x = element_text(
      # angle = 300,
      angle = 270,
      hjust = 1, 
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

size_by_fpid <- 
  pico_cat_results %>% 
  count(
    fpid,
    sort = T) %>% 
  mutate(
    type = 'Effect Sizes'
  ) %>% 
  bind_rows(
    synthesis %>% 
      count(fpid,
            sort = T) %>% 
      mutate(
        type = 'MAs by FPID'
      )
  ) %>% 
  ggplot() +
  aes(x = n,
      y = fpid %>% factor(
        levels = fpid_ordered) %>%
        fct_rev()) +
  geom_col(
    fill = 'white',
    colour = 'black',
    size = line_width/2,
    width = 1
  ) +
  geom_text(
    aes(
      x = n + 10,
      label = n
    ),
    size = text_size_plot,
    hjust = 0
  ) +
  geom_vline(
    xintercept = 0,
    size = line_width
  ) + 
  facet_grid(
    . ~ type,
    scales = 'free_x',
    space = 'free_x',
    # labeller = label_wrap_gen(width = 12)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0, 50)),
    limits = ~c(0, ifelse(.[2] > 150, .[2], 150))
  ) +
  theme(
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.spacing = unit(0, 'mm'),
    strip.text = element_blank()
  )

fpid_grob <- 
  impact_matrix_by_fpid %>% 
  ggplotGrob() %>% 
  gtable_filter('axis-l')

impact_by_fpid_grob <- 
  impact_matrix_by_fpid %>% 
  ggplotGrob() %>% 
  gtable_filter('panel')

impact_id_grob <- 
  impact_matrix_by_fpid %>% 
  ggplotGrob() %>% 
  gtable_filter('axis-t')

mas_by_fpid_grob <- 
  size_by_fpid %>% 
  ggplotGrob() %>% 
  gtable_filter('panel-1-2')

lay_head <- function(head_text,
                     gp = gpar(fontsize = 10)) {
  hline_grob <- 
    linesGrob(
      x = unit(c(.05, .95), 'npc'),
      y = unit(rep(.5, 2), 'npc'),
      gp = gpar(
        lty = '11'
      )
    )
  
  head_grob <- 
    richtext_grob(
      x = unit(.5, 'npc'),
      y = unit(.5, 'npc'),
      text = head_text,
      margin = unit(c(0, 5, 0, 5), "pt"),
      gp = gp
    )
  
  diag <- 
    gtable() %>% 
    gtable_add_cols(
      widths = unit(1, 'null')
    ) %>% 
    gtable_add_cols(
      widths = unit(
        widthDetails(head_grob), 'points'
      )
    ) %>% 
    gtable_add_cols(
      widths = unit(1, 'null')
    ) %>% 
    gtable_add_rows(
      heights = unit(1, 'null')
    ) %>% 
    gtable_add_grob(
      grobs = gList(hline_grob, head_grob, hline_grob),
      t = rep(1, 3),
      l = 1:3)
    
  return(diag)
}

es_by_fpid_grob <- 
  size_by_fpid %>% 
  ggplotGrob() %>% 
  gtable_filter('panel-1-1')

viz2_gtable <- 
  gtable() %>% 
  # padding
  gtable_add_rows(
    heights = unit(.2, 'cm')
  ) %>% 
  gtable_add_rows(
    heights = unit(.5, 'cm')
  ) %>% 
  gtable_add_rows(
    heights = impact_id_grob %>% 
      gtable_height() %>% .[1]
  ) %>% 
  gtable_add_rows(
    heights = unit(1, 'null')
  ) %>% 
  # padding
  gtable_add_rows(
    heights = unit(.2, 'cm')
  ) %>% 
  # padding
  gtable_add_cols(
    widths = unit(.2, 'cm')
  ) %>% 
  gtable_add_cols(
    widths = fpid_grob %>% 
      gtable_width()
  ) %>% 
  gtable_add_cols(
    widths =  mas_by_fpid_grob %>% 
      gtable_width()
  ) %>% 
  gtable_add_cols(
    widths =  unit(1200, 'null')
  ) %>% 
  gtable_add_cols(
    widths =  es_by_fpid_grob %>% 
      gtable_width()
  ) %>% 
  # padding
  gtable_add_cols(
    widths = unit(.2, 'cm')
  ) 
  
viz2_gtable %>%
  gtable_show_layout()

viz2 <- 
  viz2_gtable %>% 
  gtable_add_grob(
    grobs = gList(
      fpid_grob,
      mas_by_fpid_grob,
      impact_by_fpid_grob,
      es_by_fpid_grob
      ),
    t = rep(4, 4),
    l = 2:5
  ) %>%
  gtable_add_grob(
    grobs = impact_id_grob,
    t = 3,
    l = 4
  ) %>% 
  gtable_add_grob(
    grobs = gList(
      lay_head(
        'MAs [n]',
      ),
      lay_head(
        'Impacts [n]'
      ),
      lay_head(
        'Effect Sizes [n]'
      )
    ),
    t = rep(2, 3),
    l = 3:5
 ) 


grid.newpage()
viz2 %>% grid.draw()

