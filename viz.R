# VIZ -----------------------------------------------------------

unit_type <- 'mm'
line_width <- 1.2
width <- 150
height <- width
base_size <- 16
categorical_palette <- paletteer_d("futurevisions::atomic_blue")
colours_dia <- categorical_palette[2:3]
# colour_base <- categorical_palette[2]
colour_base <- '#3982a4'
size_scale <- 2.8455

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
  theme(
    axis.text.x = element_text(
      angle = 90, hjust = 1, vjust = 1
    )
  )

pico_cat_results %>% 
  count(fpid, sort = T) %>% 
  ggplot() +
  aes(x = n,
      y = fpid) +
  geom_col()