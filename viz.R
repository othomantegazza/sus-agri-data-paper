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

tst <- lay_head(head_text = 'MAs [n]') 

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
    grobs = fpid_grob,
    t = 4,
    l = 2
  ) %>% 
  gtable_add_grob(
    grobs = mas_by_fpid_grob,
    t = 4,
    l = 3
  ) %>% 
  gtable_add_grob(
    grobs = impact_by_fpid_grob,
    t = 4,
    l = 4
  ) %>% 
  gtable_add_grob(
    grobs = impact_id_grob,
    t = 3,
    l = 4
  ) %>% 
  gtable_add_grob(
    grobs = es_by_fpid_grob,
    t = 4,
    l = 5
  ) %>% 
  gtable_add_grob(
    grobs = lay_head(
      'MAs [n]'
    ),
    t = 2,
    l = 3
  ) %>% 
  gtable_add_grob(
    grobs = lay_head(
      'Impacts [n]'
    ),
    t = 2,
    l = 4
  ) %>% 
  gtable_add_grob(
    grobs = lay_head(
      'Effect Sizes [n]'
    ),
    t = 2,
    l = 5
  ) 


grid.newpage()
viz2 %>% grid.draw()

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

p_screening <- 
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

p_screening

statuses <- 
  status_by_fpid %>% 
  group_by(short_description) %>% 
  summarise(n = n %>% sum()) %>% 
  mutate(discarded = c(NA, n[1:{n() - 1}]) - n) %>% 
  # transmute(res = paste(
  #   short_description, ': retained ', n, ' MAs', sep = '')
  # ) %>% 
  # pull(res) 
  mutate(
    top_arrow = c(F, T, T),
    right_arrow = c(T, T, F),
    top_text = top_arrow
  )

gp <- gpar(
  fontsize = base_size
)


make_diagram <- function(
    short_description,
    n,
    discarded,
    top_arrow = T,
    right_arrow = T,
    top_text = T,
    ...
) {
  hs <- c(2, .2, 1, 2.2, .2)
  ws <- c(2, 1, .5)
  
  diagr <- 
    gtable() %>% 
    gtable_add_rows(
      heights = unit(hs[1], "cm")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[2], "cm")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[3], "null")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[4], "cm")
    ) %>%
    gtable_add_rows(
      heights = unit(hs[5], "cm")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[1], "cm")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[2], "null")
    ) %>%
    gtable_add_cols(
      widths = unit(ws[3], "cm")
    )
  
  # bottom text
  btxt <- 
    textbox_grob(
      text = paste(
        short_description, ': retained ', n, ' MAs', sep = ''
      ),
      x = unit(0, "npc"),
      y = unit(0, "npc"),
      width = unit(1, "npc"),
      hjust = 0,
      vjust = 0,
      gp = gp
    )
  
  diagr <- 
    diagr %>% 
    gtable_add_grob(
      grobs = btxt,
      t = 4,
      l = 1
    ) 
  
  if(top_text) {
    ttxt <- 
      textbox_grob(
        text = ,paste(
          'Discarded:', ' ', discarded, ' ', 'MAs', sep = ''
        ),
        x = unit(0, "npc"),
        y = unit(0, "npc"),
        width = unit(1, "npc"),
        hjust = 0,
        vjust = 0,
        gp = gp,
        box_gp = gpar(fill = 'white',
                      col = '#00000000')
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = ttxt,
        t = 1,
        l = 1
      )
  }
  
  if(right_arrow) {
    harr <- 
      linesGrob(
        x = unit(c(0, 1), 'npc'),
        y = unit(rep(base_size, 2), 'points'),
        arrow = arrow(
          length = unit(.2, 'cm'),
          type = "open"
        ),
        gp = gpar(
          lty = '11'
        )
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = harr,
        t = 4,
        l = 2
      )
  }
  
  if(top_arrow) {  
    varr <- 
      linesGrob(
        x = unit(rep(base_size, 2), 'points'),
        y = unit(c(0, 1), 'npc'),
        arrow = arrow(
          length = unit(.2, 'cm'),
          type = "open"
        ),
        gp = gpar(
          lty = '11'
        )
      )
    
    diagr <- 
      diagr %>% 
      gtable_add_grob(
        grobs = varr,
        t = 3,
        l = 1
      )
  }
  return(diagr)
}

status_tables <- 
  statuses %>% pmap(make_diagram)

# 
# grid.newpage()
# diagr %>% grid.draw()

p_screening_augmented <- 
  p_screening %>%
  ggplotGrob() %>% 
  # keep only panels and the active axis
  gtable_filter(
    pattern = c('panel|axis-l-1')
    ) %>% 
  # add padding
  gtable_add_cols(
    widths = unit(.2, 'cm'),
    pos = 0
  ) %>% 
  gtable_add_cols(
    widths = unit(.5, 'cm')
    ) %>% 
  gtable_add_rows(
    heights = unit(.5, 'cm'),
    pos = 0
  ) %>% 
  gtable_add_rows(
    heights = unit(.5, 'cm')
  )  %>% 
  gtable_add_rows(
    heights = unit(5.5, "cm"),
    pos = 1
  ) %>% 
  gtable_add_grob(
    grobs = status_tables[[1]],
    t = 2,
    l = 3
  ) %>% gtable_add_grob(
    grobs = status_tables[[2]],
    t = 2,
    l = 5
  ) %>% gtable_add_grob(
    grobs = status_tables[[3]],
    t = 2,
    l = 7
  ) 

grid.newpage()
p_screening_augmented %>% grid.draw()

