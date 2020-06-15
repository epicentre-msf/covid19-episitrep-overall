

# Load tool for this script
source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_get_data.R")  , encoding = "UTF-8")


# === === === === === === === 
# ECDC data
# === === === === === === === 

# Option to update ECDC data
get_updated_data <- ifelse(!file.exists(file.path(path.local.worldwide.data, 'last_save_date.RDS')), TRUE, 
                           ifelse(readRDS(file.path(path.local.worldwide.data, 'last_save_date.RDS')) != Sys.Date(),
                                  TRUE, FALSE))

# Get the ECDC data
if (get_updated_data | update_data_anyways) {
  
  sf_world     <- get_world_sf(scale = 'small', proj = 'robinson')
  df_ecdc      <- get_ecdc_data()
  df_countries     <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(continent, region, iso_a3, country))
  df_pop_country   <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(iso_a3, country, pop = population_2018))
  df_pop_region    <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(region)    %>% summarise(pop = sum(population_2018, na.rm = TRUE))
  df_pop_continent <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(continent) %>% summarise(pop = sum(population_2018, na.rm = TRUE))
  
  saveRDS(sf_world        , file = file.path(path.local.worldwide.data, paste0('sf_world','.RDS')))
  saveRDS(df_countries    , file = file.path(path.local.worldwide.data, paste0('df_countries','.RDS')))
  saveRDS(df_pop_country  , file = file.path(path.local.worldwide.data, paste0('df_pop_country','.RDS')))
  saveRDS(df_pop_region   , file = file.path(path.local.worldwide.data, paste0('df_pop_region','.RDS')))
  saveRDS(df_pop_continent, file = file.path(path.local.worldwide.data, paste0('df_pop_continent','.RDS')))
  
  # ECDC data until date_max_report
  dfc_ecdc <- df_ecdc %>% 
    tidyr::drop_na(iso_a3) %>% 
    filter(between(date, left = min(df_ecdc$date, na.rm = TRUE), right = date_max_report))
  
  # ECDC split data in a list of countries with iso_a3 as key
  lst_ecdc <- dfc_ecdc %>% 
    multisplit("iso_a3")
  
  save(df_ecdc, 
       dfc_ecdc, 
       lst_ecdc, 
       file = file.path(path.local.worldwide.data, 'dta_ECDC.RData'))
  
  saveRDS(Sys.Date(), file = file.path(path.local.worldwide.data, 'last_save_date.RDS'))
  
} else {
  
  sf_world         <- readRDS(file.path(path.local.worldwide.data, paste0('sf_world','.RDS')))
  df_countries     <- readRDS(file.path(path.local.worldwide.data, paste0('df_countries','.RDS')))
  df_pop_country   <- readRDS(file.path(path.local.worldwide.data, paste0('df_pop_country','.RDS')))
  df_pop_region    <- readRDS(file.path(path.local.worldwide.data, paste0('df_pop_region','.RDS')))
  df_pop_continent <- readRDS(file.path(path.local.worldwide.data, paste0('df_pop_continent','.RDS')))
  
  load(file.path(path.local.worldwide.data, 'dta_ECDC.RData'))
  
}


# --- --- --- --- --- ---
# TESTS dataset from FIND
# --- --- --- --- --- ---

df_tests <- read.csv(file.path(path.data, 'COVID-19 cases and tests tracker.csv'), stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

iso_tests <- read.csv(file.path(path.data, 'iso-a3_for_tests.csv'), stringsAsFactors = FALSE)

lst_tests <- df_tests %>% 
  select(country, date, new_tests) %>% 
  filter(date <= date_max_report) %>% 
  mutate(
    country = case_when(
      country == "N/A" ~ NA_character_,
      TRUE ~ country)
  ) %>% 
  tidyr::drop_na(country) %>% 
  full_join(iso_tests, by = 'country') %>% 
  arrange(iso_a3, date) %>% 
  multisplit("iso_a3")


# Fill possible gap in tests time-series
for (i in names(lst_tests)) {
  
  tests_dta <- lst_tests[[i]]
  if (nrow(tests_dta) > 1) {
    
    tests_dta <- tests_dta %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      date_max_report, by = 1), 
                      fill = list(new_tests = NA_real_))
  }
  lst_tests[[i]] <- tests_dta
}

save(df_tests, 
     lst_tests, 
     file = file.path(path.local.worldwide.data, paste0('lst_tests','.RData')))


