impacts %>% 
  count(scale, geo_coverage) %>% 
  mutate(geo_coverage = geo_coverage %>% str_split(pattern = ";")) %>% 
  unnest(geo_coverage) %>% 
  summarise(
    n = n %>% sum(na.rm = T),
    .by = geo_coverage
  ) %>% 
  arrange(desc(n))
