library(dplyr)
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

# Parameters ----------------------------------------------------

unit_type <- 'mm'
line_width <- 1.2
width <- 150
height <- width
base_size <- 16
# categorical_palette <- paletteer_d("futurevisions::atomic_blue")
# colours_dia <- categorical_palette[2:3]
# colour_base <- categorical_palette[2]
colour_base <- '#3982a4'
size_scale <- 2.8455

theme_set(
  theme_minimal(
    base_size = base_size,
  ) +
    theme(
      plot.background = element_rect(fill = 'white',
                                     colour = 'white'),
      plot.margin = margin(20,5,5,5),
      axis.text.x = element_text(hjust = .5,
                                 colour = "black",
                                 size = base_size),
      axis.text.y = element_text(vjust = 0,
                                 colour = "black",
                                 size = base_size),
      axis.ticks = element_line(),
      axis.line = element_line(linewidth = line_width),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
)


# Functions -----------------------------------------------------

list.files(
  'R',
  full.names = T
) %>% 
  walk(source)


# Read metadata -------------------------------------------------

dataset_quality_metrics <- 
  read_json('data/metadata/dataset-quality-metrics.json',
            simplifyVector = T)

# Read data -----------------------------------------------------

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

# Clean and check integrity -------------------------------------

# impacts %>% 
#   clean_impacts()


# paper metadata ------------------------------------------------

paper_metadata_colnames <- 
  read_json('data/metadata/paper-metadata-column-name.json',
            simplifyVector = T)


selected_paper_metadata <- 
  papers %>% 
  select(
    all_of(paper_metadata_colnames)
  ) %>% 
  distinct()

selected_paper_metadata %>% 
  write_csv(
    'data/output/selected-paper-metadata.csv'
  )

selected_paper_metadata %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/selected-paper-metadata-DUPL.csv'
  )

# paper farming practice ----------------------------------------

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

selected_paper_fpid <- 
  papers %>% 
  select(
    doi,
    fpid
  ) %>% 
  group_by(doi) %>% 
  summarise(fpid = list(unique(fpid)))

selected_paper_fpid %>% 
  write_json(
    'data/output/selected-paper-fpid.json',
    pretty = TRUE
  )

# paper selection criteria --------------------------------------

selection_criteria_colnames <- 
  read_json('data/metadata/paper-selection-criteria.json',
            simplifyVector = T)

selection_criteria <- 
  papers %>% 
  select(
    all_of(selection_criteria_colnames)
  ) %>% 
  distinct()

selection_criteria %>% 
  write_csv(
    'data/output/paper-selection-criteria.csv' 
  )

selection_criteria %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/paper-selection-criteria-DUPL.csv' 
  )

# paper quality score -------------------------------------------

paper_quality_metrics_colnames <- 
  read_json('data/metadata/paper-quality-metrics.json',
            simplifyVector = T)

paper_quality_metrics <- 
  papers %>% 
  select(
    all_of(paper_quality_metrics_colnames)
  ) %>% 
  distinct()

paper_quality_metrics %>% 
  write_csv(
    'data/output/paper_quality_metrics.csv' 
  )

paper_quality_metrics %>% 
  extract_duplicated_rows() %>% 
  write_csv(
    'data/output/paper_quality_metrics-DUPL.csv' 
  )
