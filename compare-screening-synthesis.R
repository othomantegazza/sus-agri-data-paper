library(tidyverse)

synthesis <- 
  read_csv(
    'data/output/selected-paper-synthesis.csv'
  )

status_by_fpid <-   
  read_csv(
    'data/output/4-systematic-screening.csv'
  ) %>% 
  filter(Status == 'B')


anti_join(
  synthesis %>% 
    select(doi, fpid),
  status_by_fpid,
  by = c('doi' = 'DOI')
) %>%
  distinct() %>% 
  arrange(fpid, doi)

anti_join(
  status_by_fpid,
  synthesis %>% 
    select(doi, fpid),
  by = c('DOI' = 'doi')
) %>% 
  distinct() %>% 
  arrange(FPID, DOI)
