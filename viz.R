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
    axis.line = element_line(
      size = line_width*.2
    ),
    axis.text.x = element_text(
      angle = 300, 
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
  # add back padding
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

# p_screening_augmented %>% gtable_show_layout()
# p_screening %>% ggplotGrob %>% gtable_show_layout()
grid.newpage()
p_screening_augmented %>% grid.draw()

# p_screening_augmented %>% .$layout

# grid.ls()
# 
# pushViewport(viewport(layout.pos.col = 2:3, layout.pos.row = 3))
# grid.rect(gp = gpar(col = "grey"))


  
# .$layout %>% %>% 
  # filter(name %>% str_detect('^strip-t'))



gtable_add_grob(
  grobs = textGrob(
    label = 'Ciao',
    gp = gpar(fontsize = base_size)
  ),
  t = 7,
  l = 5
  ) %>% # .$layout
  grid.draw()








p_scr_t <- 
  p_screening %>%
  ggplotGrob() %>% 
  gtable_add_rows(heights = unit(1, 'cm'), pos = 6) %>%
  gtable_add_grob(
    rect,
    t = 1,
    b = 13,
    l = 1,
    r = 9,
    z = Inf
  )
  # gtable_add_grob(
  #   textGrob('ciao', gp = gpar(col = 'red', fontsize = 30)),
  #   t = 7,
  #   l = 8,
  #   # b = 7,
  #   # r = 5, 
  #   z = Inf)

grid.draw(p_scr_t)

p_scr_t$layout


rect <- rectGrob(gp = gpar(fill = "#00000080"))
tab <- gtable(unit(rep(1, 3), "null"), unit(rep(1, 3), "null"))
tab <- gtable_add_grob(tab, rect, t = 1, l = 1, r = 3)
tab <- gtable_add_grob(tab, rect, t = 1, b = 3, l = 1)
tab <- gtable_add_grob(tab, rect, t = 1, b = 3, l = 3)
