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

impacts_primary_keys <- c('doi', 'fpid', 'impact_matrix')

impacts <-
  read_csv(
    'data/MerFPs_Impacts.csv'
  ) %>%
  clean_names() %>% 
  group_by(
    across(
      all_of(impacts_primary_keys)
    )
  )  %>% 
  fill(
    scale,
    data_in_europe,
    nb_of_papers,
    .direction = "downup"
  ) %>%
  ungroup()

papers <- 
  impacts %>% 
  filter(info_type == 'G')

paper_details <- 
  impacts %>% 
  filter(info_type %in% c('I', 'S'))


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

primary_keys_combinations <- 
  papers %>% 
  select(all_of(impacts_primary_keys)) %>% 
  distinct() %>% 
  arrange(
    across(
      all_of(
        impacts_primary_keys
      )
    )
  )
  
primary_keys_combinations %>% 
  glimpse()

primary_keys_combinations %>% 
  write_csv(
    'data/output/selected-paper-primary-keys.csv'
  )

# paper synthesis --------------------------------------


synthesis_colnames <-
  read_json(
    'data/metadata/selected-ma-synthesis.json',
    simplifyVector = T
  )

synthesis <- 
  papers %>% 
  select(
    all_of(synthesis_colnames)
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
  arrange(
    across(
      all_of(
        impacts_primary_keys
      )
    )
  )

synthesis %>% 
  glimpse()

synthesis %>% 
  extract_duplicated_rows(id_cols = impacts_primary_keys) %>% 
  write_csv(
    'data/output/paper-synthesis-DUPL.csv' 
  )
  
synthesis %>% 
  write_csv(
    'data/output/paper-synthesis.csv' 
  )

coltypes_synthesis <- 
  synthesis %>% {
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

# I and S lines PICO ----------------------------------------------------

columns_pico <-
  read_json('data/metadata/selected-ma-pico.json',
            simplifyVector = T)

pico_details <- 
  paper_details %>%
  select(
    all_of(columns_pico)
  )

stopifnot(
  all(
    pico_details$fpid %in% farming_practices
  )
)

stopifnot(
  all(
    pico_details$impact_matrix %in% impact_matrices
  )
)

if(
  !all(
    pico_details$doi %in% primary_keys_combinations$doi
  )
) {
  warning('Some doi in I or S rows is not represented in the G rows')
  
  pico_details %>% 
    filter( ! doi %in% primary_keys_combinations$doi ) %>% 
    write_csv("data/output/selected-paper-PICO-extra-DOI.csv")
  
} 

pico_details <- 
  pico_details %>% 
  mutate(data_in_europe = data_in_europe %>% {
    case_when(. == 'Y' ~ TRUE,
              . == 'N' ~ FALSE,
              TRUE ~ NA)
  }) %>% 
  mutate(
    across(
    .cols =   c(positive, negative, no_effect, uncertain),
    .fns = ~as.numeric(.) %>% as.logical()
    )
  ) %>% 
  mutate(nb_of_papers = nb_of_papers %>% as.numeric())


pico_details %>% skim()

pico_details %>% glimpse()

pico_details %>% 
  write_csv("data/output/selected-paper-PICO.csv")

coltypes_pico <- 
  pico_details %>% {
    tibble(colname = colnames(.),
           type = map_chr(., class))  
  }
 
# paper_population <- 
#   papers %>% 
#   select(
#     all_of(population_colnames)
#   ) %>% 
#   distinct() %>% 
#   relocate(
#     doi,
#     impact_matrix
#   ) %>% 
#   arrange(
#     doi,
#     impact_matrix
#   )
# 
# paper_population %>% 
#   glimpse()
# 
# paper_population_DUPL <- 
#   paper_population %>% 
#   extract_duplicated_rows(id_cols = c("doi", "impact_matrix"))
# 
# paper_population %>% 
#   nest(results = matches('result_*'),
#        factors = matches('factor*_*'))
# 
# paper_population %>% 
#   write_csv(
#     'data/output/paper-population.csv' 
#   )
# 
# paper_population %>% 
#   extract_duplicated_rows() %>% 
#   write_csv(
#     'data/output/paper-population-DUPL.csv' 
#   )
# 

# write data types ----------------------------------------------



coltypes <-
  list(metadata = coltypes_metadata,
       synthesis = coltypes_synthesis,
       pico = coltypes_pico)

coltypes %>%
  write_json(
    'data/metadata/selecte-ma-coltypes.json',
    pretty = T
  )


