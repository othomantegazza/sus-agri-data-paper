# GRAPH SETUP ---------------------------------------------------

unit_type <- 'mm'
mm_to_in <- 0.0393701
line_width <- 1.2
a4_width <- 210
a4_height <- 297
width <- 150
height <- 150
base_size <- 10
categorical_palette <- paletteer_d("futurevisions::atomic_blue")
fill_color <- "#fcfcea"
colours_dia <- categorical_palette[2:3]
# colour_base <- categorical_palette[2]
colour_base <- '#3982a4'
size_scale <- 2.8455
text_size_plot <- base_size/size_scale
ppi <- 300
lineheight <- .8
padding <- unit(2, unit_type)

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

check_plot <- function(p) {
  grid.newpage(); gtable_show_layout(p)
  grid.newpage(); grid.draw(p)
}
