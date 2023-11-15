clean_search <- function(df = 'data/MerFPs_Search_20231017.xlsx') {
  # read data -----------------------------------------------------
  
  search_tab <- 
    read_excel(
      df
    ) %>% 
    clean_names()
  
  
  # read metadata -------------------------------------------------
  
  metadata <- 
    read_json(
      'data/metadata/impacts-metadata.json',
      simplifyVector = T,
    ) %>%
    as_tibble() %>% 
    rename(line_type = info_type) %>% 
    filter(name == "02_search_eq")
  
  
  # clean ---------------------------------------------------------
  
  search_tab <-
    search_tab %>% 
    rename(type = x1) %>% 
    mutate(
      across(
        .cols = c("date_of_search_wos",
                  "date_of_search_scopus"),
        .fns = ~as.numeric(.) %>% 
          as.Date(origin = "1899-12-30")
      )
    ) %>% 
    mutate(
      across(
        .cols = c(
          "nb_papers_scopus",
          "nb_papers_wos",
          "nb_papers_after_merging"
        ),
        .fns = ~as.numeric(.)
      )
    )
  
  metadata %>% 
    mutate(df = search_tab %>% list()) %>% 
    pwalk(
      clean_imap
    )
  
  
  # save additional assets ----------------------------------------
  
  
  search_tab <-
    search_tab %>% 
    select(
      fpid,
      type,
      date_of_search_scopus,
      date_of_search_wos,
      nb_papers_scopus,
      nb_papers_wos,
      nb_papers_after_merging
    ) %>% 
    summarise(
      across(
        .cols = c(
          nb_papers_scopus,
          date_of_search_scopus,
          nb_papers_wos,
          date_of_search_wos,
          nb_papers_after_merging
        ),
        .fns = ~max(., na.rm = T)
      ),
      .by = fpid
    )
  
  search_tab %>% 
    write_csv(
      "data/output/03-search-summarised.csv"
    )
  
  search_dates <-
    search_tab %>% 
    select(fpid, date_of_search_wos, date_of_search_scopus) %>% 
    pivot_longer(cols = !fpid) %>% 
    select(-name) %>% 
    group_by(fpid) %>% 
    summarise(
      across(
        .cols = everything(),
        .fns = ~max(., na.rm = T)
      )
    ) %>% 
    arrange(value) %>% 
    rename(date_of_search = value)
  
  search_dates %>% 
    write_csv(
      "data/output/03-search-dates.csv"
    )
}