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

# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)

# Read selected paper data ---------------------------------------

impacts <-
  read_csv(
    'data/MerFPs_Impacts.csv'
  ) %>%
  clean_names() %>% 
  filter()

papers <- 
  impacts %>% 
  filter(info_type == 'G')

paper_details <- 
  impacts %>% 
  filter(info_type == 'I')


# read column metadata ------------------------------------------

column_metadata <- 
  read_json(
    'data/metadata/selected-ma-coltypes.json',
    simplifyVector = T
  )

# paper metadata ------------------------------------------------


metadata <- 
  papers %>% 
  select(
    all_of(column_metadata$metadata$colname)
  ) %>% 
  distinct() %>% 
  arrange(doi)

metadata %>% 
  write_csv(
    'data/output/selected-paper-metadata.csv'
  )

metadata %>% 
  glimpse()

metadata %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/selected-paper-metadata-DUPL.csv'
  )

coltypes_metadata <- 
  metadata %>% {
    tibble(colname = colnames(.),
           type = map_chr(., class))  
  }


# paper primary keys ----------------------------------------


# ┣ farming practices ---------------------------------------

farming_practices <- 
  read_json(
    'data/metadata/farming-practices.json',
    simplifyVector = T
  )

stopifnot(
  all(
    papers$fpid %in% farming_practices
  )
)

# ┣ impact matrices --------------------------------------------

impact_matrices <- 
  read_json('data/metadata/impact_matrices.json',
            simplifyVector = T)

stopifnot(
  all(
    papers$impact_matrix %in% impact_matrices
  )
)


# ┣ extract json with unique keys ---------------------------------

selected_paper_fpid_im <- 
  papers %>% 
  select(
    doi,
    fpid,
    impact_matrix
  ) %>% 
  select(doi, fpid, impact_matrix) %>% 
  distinct() %>% 
  arrange(doi, fpid, impact_matrix)
  
selected_paper_fpid_im %>% 
  glimpse()

selected_paper_fpid_im %>% 
  write_csv(
    'data/output/selected-paper-primary-keys.csv'
  )

# paper selection criteria --------------------------------------

criteria <- 
  papers %>% 
  select(
    all_of(column_metadata$criteria$colname)
  ) %>% 
  distinct() %>% 
  mutate(data_in_europe = data_in_europe %>% {
    case_when(. == 'Y' ~ TRUE,
              . == 'N' ~ FALSE,
              TRUE ~ NA)
  }) %>% 
  mutate(
    across(
      .cols = c(
        'selection_criteria_2',
        'selection_criteria_3'
      ),
      .fns = ~as.numeric(.) %>% as.logical() 
    )
  ) %>% 
  arrange(doi)
    
criteria %>% 
  glimpse()

criteria %>% 
  write_csv(
    'data/output/paper-selection-criteria.csv' 
  )

criteria %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/paper-selection-criteria-DUPL.csv' 
  )

coltypes_criteria <- 
  criteria %>% {
    tibble(colname = colnames(.),
           type = map_chr(., class))  
  }


# paper quality score -------------------------------------------

quality_metrics <- 
  papers %>% 
  select(
    all_of(column_metadata$quality_metrics$colname)
  ) %>% 
  distinct() %>% 
  mutate(
    across(
      .cols = !doi,
      .fns = ~as.numeric(.) %>% as.logical()
    )
  ) %>% 
  arrange(doi)

quality_metrics %>% 
  glimpse()

quality_metrics %>% 
  write_csv(
    'data/output/paper_quality_metrics.csv' 
  )

quality_metrics %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/paper_quality_metrics-DUPL.csv' 
  )

quality_metrics %>% skim()

coltypes_quality_metrics <- 
  quality_metrics %>% {
    tibble(colname = colnames(.),
           type = map_chr(., class))  
  }
# 
# coltypes <- 
#   list(metadata = coltypes_metadata,
#        criteria = coltypes_criteria,
#        quality_metrics = coltypes_quality_metrics)
# 
# coltypes %>% 
#   write_json(
#     'data/metadata/selecte-ma-coltypes.json',
#     pretty = T
#   )
#   

# g lines PICO ----------------------------------------------------

population_colnames <- 
  read_json('data/metadata/paper-PICO.json',
            simplifyVector = T)

impact_matrices <- 
  read_json('data/metadata/impact_matrices.json',
            simplifyVector = T)

stopifnot(
  all(
    papers$impact_matrix %in% impact_matrices
  )
)

paper_population <- 
  papers %>% 
  select(
    all_of(population_colnames)
  ) %>% 
  distinct() %>% 
  relocate(
    doi,
    impact_matrix
  ) %>% 
  arrange(
    doi,
    impact_matrix
  )

paper_population %>% 
  glimpse()

paper_population_DUPL <- 
  paper_population %>% 
  extract_duplicated_rows(id_cols = c("doi", "impact_matrix"))

paper_population %>% 
  nest(results = matches('result_*'),
       factors = matches('factor*_*'))

paper_population %>% 
  write_csv(
    'data/output/paper-population.csv' 
  )

paper_population %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/paper-population-DUPL.csv' 
  )



