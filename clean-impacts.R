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

# impacts_primary_keys <- c('doi', 'fpid', 'impact_matrix')

impacts <-
  read_csv(
    'data/MerFPs_Impacts.csv'
  ) 

problems(impacts)
  
impacts <- 
  impacts %>%   
  clean_names() # %>% 
  # group_by(
  #   across(
  #     all_of(impacts_primary_keys)
  #   )
  # )  %>% 
  # fill(
  #   scale,
  #   data_in_europe,
  #   nb_of_papers,
  #   .direction = "downup"
  # ) %>%
  # ungroup()

# papers <- 
#   impacts %>% 
#   filter(info_type == 'G')
# 
# paper_details <- 
#   impacts %>% 
#   filter(info_type %in% c('I', 'S'))


# read column metadata ------------------------------------------

metadata <- 
  read_json(
    'data/metadata/selected-ma-coltypes.json',
    simplifyVector = T
  )

# paper metadata ------------------------------------------------

ma_metadata <- 
  impacts %>% 
  filter(info_type == "G") %>% 
  select(
    all_of(metadata$G$ma_metadata$columns$colname)
  ) %>% 
  distinct() %>% 
  arrange(doi)

ma_metadata %>% 
  write_csv(
    'data/output/selected-paper-metadata.csv'
  )

ma_metadata %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/selected-paper-metadata-DUPL.csv'
  )


# paper primary keys ----------------------------------------


# ┣ farming practices ---------------------------------------

farming_practices <- 
  read_json(
    'data/metadata/farming-practices.json',
    simplifyVector = T
  )

stopifnot(
  all(
    impacts$fpid %in% farming_practices
  )
)

# ┣ impact matrices --------------------------------------------

impact_matrices <- 
  read_json('data/metadata/impact_matrices.json',
            simplifyVector = T)

stopifnot(
  all(
    impacts$impact_matrix %in% impact_matrices
  )
)

# ┣ extract json with unique keys ---------------------------------

primary_keys_combinations <- 
  impacts %>% 
  select(all_of(metadata$G$synthesis$unique_identifiers)) %>% 
  distinct() %>% 
  arrange(
    across(
      all_of(
        metadata$G$synthesis$unique_identifiers
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

# synthesis_colnames <-
#   read_json(
#     'data/metadata/selected-ma-synthesis.json',
#     simplifyVector = T
#   )

synthesis_coerced_cols <- 
  c(
    'selection_criteria_2',
    'selection_criteria_3'
  )

synthesis <- 
  impacts %>% 
  filter(info_type == "G") %>% 
  select(
    all_of(metadata$G$synthesis$column$colname)
  ) %>% 
  distinct() %>% 
  mutate( 
    across(
      .cols = all_of(synthesis_coerced_cols),
      .fns = ~as.numeric(.) %>% as.logical() 
    )
  ) %>%
  mutate(data_in_europe = data_in_europe %>% {
    case_when(. == 'Y' ~ TRUE,
              . == 'N' ~ FALSE,
              TRUE ~ NA)
  }) %>%
  mutate(nb_of_papers = nb_of_papers %>% as.numeric()) %>% 
  arrange(
    across(
      all_of(
        metadata$G$synthesis$unique_identifiers
      )
    )
  )

synthesis %>% 
  extract_duplicated_rows(
    id_cols = metadata$G$synthesis$unique_identifiers
  ) %>%
  write_csv(
    'data/output/selected-paper-synthesis-DUPL.csv' 
  )

synthesis %>% 
  write_csv(
    'data/output/selected-paper-synthesis.csv' 
  )


# paper quality score -------------------------------------------

quality_metrics <- 
  impacts %>% 
  filter(info_type == "G") %>% 
  select(
    all_of(metadata$G$quality_metrics$columns$colname)
  ) %>% 
  distinct() %>% 
  arrange(doi)

quality_metrics_clean <- 
  quality_metrics %>% 
  mutate(
    across(
      .cols = !c(doi, fpid),
      .fns = ~as.numeric(.) %>% as.logical()
    )
  )

quality_metrics_not_clean <- 
  quality_metrics %>%
  anti_join(quality_metrics_clean %>%
              drop_na() %>%
              select(doi))



quality_metrics_clean %>% 
  write_csv(
    'data/output/selected-paper-quality-metrics.csv' 
  )

quality_metrics_clean %>% 
  extract_duplicated_rows(
    id_cols = metadata$G$quality_metrics$unique_identifiers
  ) %>% 
  write_csv(
    'data/output/selected-paper-quality-metrics-DUPL.csv' 
  )

quality_metrics_not_clean %>% 
  write_csv(
    'data/output/selected-paper-quality-metrics-NEED-FIX.csv'
  )
  

# quality metrics with more than one vote -----------------------

quality_metrics_clean %>% 
  group_by(
    across(
      all_of(
        metadata$G$quality_metrics$unique_identifiers
      )
    )
  ) %>% 
  summarise(
    across(
      everything(),
      ~sum(., na.rm = T)/n()
    )
  ) %>% 
  write_csv(
    'data/output/selected-paper-quality-metrics-VOTED-MORE-THAN-ONE.csv' 
  )

# MA Level Data -----------------------------------------------

# ma_level_data <-
#   impacts %>% 
#   
#   select(
#     all_of(
#       column_metadata$ma_level_data$colname
#     )
#   ) %>% 
#   distinct()
# 
# ma_level_data_clean <-
#   ma_level_data %>% 
#   mutate(data_in_europe = data_in_europe %>% {
#     case_when(. == 'Y' ~ TRUE,
#               . == 'N' ~ FALSE,
#               TRUE ~ NA)
#   }) %>% 
#   mutate(nb_of_papers = nb_of_papers %>% as.numeric())
# 
# ma_level_data_not_clean <- 
#   ma_level_data %>% 
#   anti_join(ma_level_data_clean %>% drop_na() %>% select(doi))
# 
# ma_level_data_not_clean %>% 
#   write_csv(
#     'data/output/ma-level-data-need-fix.csv'
#   )
# 
# ma_level_data_clean %>% 
#   extract_duplicated_rows(id_cols = c("doi", "fpid")) %>%
#   write_csv(
#     "data/output/ma-level-data-DUPL.csv"
#   )
# 
# ma_level_data_clean %>% 
#   write_csv(
#     "data/output/ma-level-data.csv"
#   )


# PICO combinations ---------------------------------------------

pico_combinations <- 
  impacts %>% 
  filter(info_type == "G") %>% 
  select(
    all_of(
      metadata$G$pico_combinations$columns$colname
      )
  ) %>% 
  distinct() %>% 
  arrange(
    across(
      all_of(
        metadata$G$pico_combinations$unique_identifiers
      )
    )
  )

pico_combinations %>% 
  write_csv(
    'data/output/pico-combinations.csv' 
  )

pico_combinations %>% 
  extract_duplicated_rows(
    id_cols = metadata$G$pico_combinations$unique_identifiers
  ) %>% 
  write_csv(
    'data/output/pico-combinations-DUPL.csv' 
  )

# pico_combinations %>% 
#   {tibble(
#     colname = colnames(.),
#     type = map_chr(., class)
#   )} %>% 
#   toJSON()
#   write_json()

# I and S lines PICO --------------------------------------------

# pico <- 
#   paper_details %>%
#   select(
#     all_of(column_metadata$pico$colname)
#   )
# 
# stopifnot(
#   all(
#     pico$fpid %in% farming_practices
#   )
# )
# 
# stopifnot(
#   all(
#     pico$impact_matrix %in% impact_matrices
#   )
# )
# 
# if(
#   !all(
#     pico$doi %in% primary_keys_combinations$doi
#   )
# ) {
#   warning('Some doi in I or S rows is not represented in the G rows')
#   
#   pico %>% 
#     filter( ! doi %in% primary_keys_combinations$doi ) %>% 
#     write_csv("data/output/selected-paper-PICO-extra-DOI.csv")
#   
# } 
# 
# pico <- 
#   pico %>% 
#   mutate(
#     across(
#     .cols =   c(positive, negative, no_effect, uncertain),
#     .fns = ~as.numeric(.) %>% as.logical()
#     )
#   )
# 
# pico %>% 
#   write_csv("data/output/selected-paper-PICO.csv")
# 
# coltypes_pico <- 
#   pico %>% {
#     tibble(colname = colnames(.),
#            type = map_chr(., class))  
#   }
#  

# write data types ----------------------------------------------

# coltypes <-
#   list(metadata = coltypes_metadata,
#        synthesis = coltypes_synthesis,
#        quality_metrics = coltypes_quality_metrics,
#        pico = coltypes_pico)

# coltypes %>%
#   write_json(
#     'data/metadata/selected-ma-coltypes.json',
#     pretty = T
#   )
# 

impacts %>% 
  count(control_matrix) %>% view()
