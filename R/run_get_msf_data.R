



# The path for msf data
path.data.sharepoint <- file.path('D:', 'MSF/GRP-EPI-COVID-19 - NCoVEpi', 'data', 'linelist', 'world')


# Get the MSF linelist dataset
if (get_updated_msf_data) {
  dta <- readRDS(file.path(path.data.sharepoint, 'msf_covid19_linelist_global_2020-06-10.rds'))
  
  # Get the MSF aggregated data
  dta_aggregated <- read.xlsx(file.path(path.data.sharepoint, 'msf_covid19_aggregated_data.xlsx'), sheetIndex = 1, stringsAsFactors = FALSE, encoding = 'UTF-8') %>% 
    filter(!is.na(country)) %>% 
    as_tibble()
  
  
  # Prepare the linelist dataset
  dta <- prepare_msf_dta(dta)
  
  # Prepare the aggregated dataset
  dta_expanded <- dta_aggregated %>% 
    select(-c(date_adm_first, date_adm_last)) %>% 
    pivot_longer(cols = c("Confirmed", "Probable", "Suspected", "Not.a.case", "Unknown"),
                 names_to = "covid_status") %>% 
    mutate(obs = purrr::map(value, ~rep_len(1, .x))) %>%
    unnest(cols = c(obs)) %>%
    select(-c(value, obs)) %>% 
    mutate(
      covid_status = case_when(
        covid_status == 'Not.a.case' ~ 'Not a case', 
        TRUE ~ covid_status), 
      country = paste(country, '(*)'))
  
  dta_expanded_dates <- dta_aggregated %>% 
    select(continent, country, site_name, date_adm_first, date_adm_last) %>% 
    mutate(
      date_adm_first = as.Date(date_adm_first, "%d/%m/%y"), 
      date_adm_last = as.Date(date_adm_last, "%d/%m/%y"), 
      country = paste(country, '(*)')) %>% 
    pivot_longer(cols = c(date_adm_first, date_adm_last), names_to = "type_date", values_to = 'date_consultation')
  
  save(dta, 
       dta_aggregated, 
       dta_expanded, 
       dta_expanded_dates, 
       file = file.path(path.local.msf.data, 'dta_MSF.RData'))
  
  } else {
    
    load(file.path(path.local.msf.data, 'dta_MSF.RData'))
  }



