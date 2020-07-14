

# Load tool for this script
source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_get_data.R")  , encoding = "UTF-8")


# === === === === === === === 
# Geo and population data
# === === === === === === === 

sf_world         <- readRDS(file.path(path.data, paste0('sf_world','.RDS')))
df_countries     <- readRDS(file.path(path.data, paste0('df_countries','.RDS')))
df_pop_country   <- readRDS(file.path(path.data, paste0('df_pop_country','.RDS')))
df_pop_region    <- readRDS(file.path(path.data, paste0('df_pop_region','.RDS')))
df_pop_continent <- readRDS(file.path(path.data, paste0('df_pop_continent','.RDS')))



# === === === === === === === 
# ECDC data
# === === === === === === === 

# Get the ECDC data
if (get_updated_world_data) {
  
  df_ecdc <- get_ecdc_data()
  
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
  
  load(file.path(path.local.worldwide.data, 'dta_ECDC.RData'))
  
}


# --- --- --- --- --- ---
# TESTS dataset from FIND
# --- --- --- --- --- ---

df_tests <- read.csv(file.path(path.data, 'cv_tests_download.csv'), stringsAsFactors = FALSE) %>% 
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


