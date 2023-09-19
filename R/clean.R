count_dois <- function(impacts, message = "") {
  cat(
    "detected",
    impacts %>% pull(doi) %>% unique() %>% length(),
    "unique dois",
    message,
    ".\n"
  )
}

clean_impacts <- function(impacts) {
  
  # → Unique Dois ------------------------------------------------
  
  unique_dois <-
    impacts %>%
    pull(doi) %>%
    unique()
  
  count_dois(impacts, "in original dataset")
  
  # → Row types G -------------------------------------------------
  
  impacts <-
    impacts %>% 
    filter(info_type == "G")
  
  count_dois(impacts, "after keeping only G lines")
  
  # → Keep only necessary columns ----------------------------------
  
  impacts <- 
    impacts %>% 
    select(
      doi,
      year,
      scale,
      nb_of_papers,
      all_of(ma_quality_metrics)
    )
  
  
  # → Quality metrics to number -----------------------------------
  
  impacts <- 
    impacts %>% 
    mutate(
      across(
        .cols = all_of(ma_quality_metrics),
        .fns = as.numeric
      )
    )
  
  # → Year to number ----------------------------------------------
  
  cat("Converting year into number\n")
  
  impacts <- 
    impacts %>% 
    mutate(year = year %>% as.numeric())
  
  # → Clean number of papers --------------------------------------
  
  cat("Extracting number of papers per metanalysis\n")
  
  undefined_number <- 
    impacts %>% 
    filter(nb_of_papers %>% as.numeric() %>% is.na()) %>% 
    pull(nb_of_papers)
  
  cat("Detected", length(undefined_number), "metanalysis with these undefined
    number of studies:\n-",
      undefined_number %>% unique() %>% paste(collapse = "\n-"),
      "\nSetting those values to NA\n")
  
  impacts <- 
    impacts %>% 
    mutate(nb_of_papers = nb_of_papers %>% as.numeric())
  
  # count_dois(impacts, "After removing rows with missing number of papers")
  
  # → Clean Scale of the analysis -----------------------------------
  
  impacts <- 
    impacts %>% 
    mutate(scale = scale %>% {
      case_when(
        str_detect(., "^US") ~ "United States",
        str_detect(., "^United_stat") ~ "United States",
        str_detect(., "^Tropic") ~ "Tropical  pedo-climates",
        str_detect(., "^temperate") ~ "Temperate pedo-climates",
        str_detect(., "^Subsah") ~ "Subsaharan Africa",
        str_detect(., "^Northamer") ~ "North America",
        str_detect(., "^Newzeal") ~ "New Zealand",
        str_detect(., "^Mediterran") ~ "Mediterranean pedo-climates",
        str_detect(., "^India") ~ "India",
        str_detect(., "^Glob") ~ "Global",
        str_detect(., "^EU") ~ "European Union",
        str_detect(., "^continental") ~ "Continental pedo-climates",
        str_detect(., "^Chin") ~ "China",
        str_detect(., "^Canad") ~ "Canada",
        str_detect(., "^Bra") ~ "Brasil",
        str_detect(., "^Australia") ~ "Australia",
        str_detect(., "^Asia") ~ "Asia",
        str_detect(., "^artic") ~ "Artic regions",
        str_detect(., "^Amer") ~ "Americas",
        str_detect(., "^Africa") ~ "Africa",
        TRUE ~ .
      )
    })
  
  impacts %>% 
    count(scale, sort = T)
  
  # → Remove fully duplicated records -----------------------------
  
  cat("Removing fully duplicated records\n")
  
  impacts <- 
    impacts %>% 
    distinct()
  
  count_dois(impacts, "After removing fully duplicated records")
  
  # → Report ------------------------------------------------------
  
  valid_dois_n <- impacts %>% pull(doi) %>% unique() %>% length()
  
  cat(
    "Out of",
    unique_dois %>% length(),
    "unique metanayses dois,\n",
    valid_dois_n,
    "dois are still valid after data cleaning\n",
    "in", impacts %>% nrow(), "records:\n",
    {impacts %>% nrow()} - {valid_dois_n},
    "records still have duplicated dois.\n"
  )
  
  # → Arbitrarily keep unique dois --------------------------------
  
  warning(
    'Removing ',
    {impacts %>% nrow()} - {valid_dois_n},
    ' duplicated  doi records arbitrarily'
  )
  
  impacts <-
    impacts %>% 
    distinct(doi, 
             .keep_all = T)
  
  
  return(impacts)
  
}

clean_dataset_quality <- function(dataset_quality) {
  dataset_quality <- 
    dataset_quality %>% 
    select(doi, all_of(dataset_quality_metrics)) %>% 
    mutate(
      across(
        .cols = all_of(dataset_quality_metrics),
        .fns = ~as.numeric(.)
      )
    )
  
  n_dois <- nrow(dataset_quality)
  unique_dois <- 
    dataset_quality %>% 
    pull(doi) %>% 
    unique() %>% 
    length()
  
  warning('found ', n_dois - unique_dois, ' duplicated dois, removing arbitrarliy')
  
  dataset_quality <- 
    dataset_quality %>% 
    distinct(doi, .keep_all = T)
  
  return(dataset_quality)
}