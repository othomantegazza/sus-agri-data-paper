grid.newpage()
x <- stats::runif(20)
y <- stats::runif(20)
rot <- stats::runif(20, 0, 360)
grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
          gp=gpar(fontsize=20, col="grey"))
grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
          gp=gpar(fontsize=20), check=TRUE)
grid.newpage()
draw.text <- function(just, i, j) {
  grid.text("ABCD", x=x[j], y=y[i], just=just)
  grid.text(deparse(substitute(just)), x=x[j], y=y[i] + unit(2, "lines"),
            gp=gpar(col="grey", fontsize=8))
}
x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")
grid.grill(h=y, v=x, gp=gpar(col="grey"))
draw.text(c("bottom"), 1, 1)
draw.text(c("left", "bottom"), 2, 1)
draw.text(c("right", "bottom"), 3, 1)
draw.text(c("centre", "bottom"), 4, 1)
draw.text(c("centre"), 1, 2)
draw.text(c("left", "centre"), 2, 2)
draw.text(c("right", "centre"), 3, 2)
draw.text(c("centre", "centre"), 4, 2)
draw.text(c("top"), 1, 3)
draw.text(c("left", "top"), 2, 3)
draw.text(c("right", "top"), 3, 3)
draw.text(c("centre", "top"), 4, 3)
draw.text(c(), 1, 4)
draw.text(c("left"), 2, 4)
draw.text(c("right"), 3, 4)
draw.text(c("centre"), 4, 4)


grid.rect(gp = gpar(lty = "dashed"))
vp <- viewport(width = 0.5, height = 0.5)
pushViewport(vp)
grid.rect(gp = gpar(col = "grey"))
grid.text("quarter of the page", y = 0.85)
pushViewport(vp)
grid.rect()

grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 5)))
grid.ls()
grid.rect(gp = gpar(col = "grey"))
grid.ls()
grid.segments(
  c(
    1:4/5,
    rep(0, 3)
  ),
  c(
    rep(0, 4),
    1:3/4
    ),
  c(
    1:4/5, 
    rep(1, 3)
  ), 
  c(
    rep(1, 4),
    1:3/4
  ),
  gp = gpar(
    col = "grey")
  )
grid.ls()
pushViewport(viewport(layout.pos.col = 2:3, layout.pos.row = 3))
grid.rect(gp = gpar(lwd = 3))             
grid.ls()

candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)


?gtable
library(grid)
a <- gtable(
  unit(1:3, c("cm")),
  unit(5, "cm")
)
a
gtable_show_layout(a)
b <- gtable(unit(c(2, 2, 2), "cm"), unit(c(2, 2, 2), "cm"))
b %>% gtable_show_layout()
b <- gtable_add_grob(b, rect, 2, 2)

g <- 
  grid.layout(
    4, 4,
    widths = unit(
      c(3, 1, 1, 1),
      c("lines", "null", "null", "cm")
    ),
    heights = c(1, 1, 2, 3),
    c("cm", "null", "null", "lines")
  )

diagr <- 
  gtable() %>% 
  gtable_add_rows(
    heights = unit(2, "cm")
  ) %>%
  gtable_add_rows(
    heights = unit(.5, "cm")
  ) %>%
  gtable_add_rows(
    heights = unit(1, "null")
  ) %>%
  gtable_add_rows(
    heights = unit(2, "cm")
  ) %>%
  gtable_add_cols(
    widths = unit(2, "cm")
  ) %>%
  gtable_add_cols(
    widths = unit(1, "null")
  ) %>%
  gtable_add_cols(
    widths = unit(.5, "cm")
  ) 

diagr %>% gtable_show_layout()  

g
class(g)
class(a)
gtable_show_layout(a)

grid.show.layout(g)
