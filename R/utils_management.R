

# Set the terms of dates 
# This is also used to create folder where to save output files
set_date_frame <- function(date_min = NULL, date_max = NULL, week_suffix = NULL, create_folders = FALSE){
  
  
  if (!is.null(date_min)) {
    date_min <- lubridate::floor_date(as.Date(date_min), unit = "week", week_start = 1)
  }
  
  if (!is.null(date_max)) {
    date_max <- as.Date(date_max)
  } else {
    date_max <- lubridate::floor_date(Sys.Date(), unit = "week", week_start = 7)
  }
  
  # Week based on date_max (with option to add a suffix)
  week_max <- ISOweek::ISOweek(date_max) %>% gsub("W","w", .)
  
  if (!is.null(week_suffix)) {
    week_max <- paste(week_max, week_suffix, sep = '_')
  }
    
  # Create paths
  path.local.week   <<- file.path(path.local, week_max)
  
  path.local.worldwide        <<- file.path(path.local.week, 'worldwide')
  path.local.worldwide.data   <<- file.path(path.local.worldwide, 'data')
  path.local.worldwide.graphs <<- file.path(path.local.worldwide, 'graphs')
  path.local.worldwide.tables <<- file.path(path.local.worldwide, 'tables')
  
  path.local.msf        <<- file.path(path.local.week, 'msf')
  path.local.msf.data   <<- file.path(path.local.msf, 'data')
  path.local.msf.graphs <<- file.path(path.local.msf, 'graphs')
  path.local.msf.tables <<- file.path(path.local.msf, 'tables')
  
  path.local.week.oc    <<- file.path(path.local.week, "oc")

  
  # path.local.msf.oc        <<- file.path(path.local.week.oc, 'msf')
  # path.local.msf.data.oc   <<- file.path(path.local.week.oc, 'data')
  # path.local.msf.graphs.oc <<- file.path(path.local.week.oc, 'graphs')
  # path.local.msf.tables.oc <<- file.path(path.local.week.oc, 'tables')
  
  # Create folders based on the paths
  if (create_folders) {
    
    dir.create(path.local.week, showWarnings = FALSE, recursive = TRUE) 
    
    dir.create(path.local.worldwide.data  , showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.worldwide.graphs, showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.worldwide.tables, showWarnings = FALSE, recursive = TRUE) 
    
    dir.create(path.local.msf.data  , showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.msf.graphs, showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.msf.tables, showWarnings = FALSE, recursive = TRUE) 
    
    dir.create(path.local.week.oc,       showWarnings = FALSE, recursive = TRUE) 
    # dir.create(path.local.msf.oc,        showWarnings = FALSE, recursive = TRUE) 
    # dir.create(path.local.msf.data.oc,   showWarnings = FALSE, recursive = TRUE) 
    # dir.create(path.local.msf.graphs.oc, showWarnings = FALSE, recursive = TRUE) 
    # dir.create(path.local.msf.tables.oc, showWarnings = FALSE, recursive = TRUE) 
  }
  
  return(list(date_min = date_min, date_max = date_max, week_max = week_max))

}


set_paths <- function(path_local, folder_name, create_folders = FALSE) {
  # Create paths
  path.local.week   <<- file.path(path_local, folder_name)
  
  path.local.worldwide        <<- file.path(path.local.week, 'worldwide')
  path.local.worldwide.data   <<- file.path(path.local.worldwide, 'data')
  path.local.worldwide.graphs <<- file.path(path.local.worldwide, 'graphs')
  path.local.worldwide.tables <<- file.path(path.local.worldwide, 'tables')
  
  path.local.msf        <<- file.path(path.local.week, 'msf')
  path.local.msf.data   <<- file.path(path.local.msf, 'data')
  path.local.msf.graphs <<- file.path(path.local.msf, 'graphs')
  path.local.msf.tables <<- file.path(path.local.msf, 'tables')
  
  # Create folders based on the paths
  if (create_folders) {
    
    dir.create(path.local.week, showWarnings = FALSE, recursive = TRUE) 
    
    dir.create(path.local.worldwide.data  , showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.worldwide.graphs, showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.worldwide.tables, showWarnings = FALSE, recursive = TRUE) 
    
    dir.create(path.local.msf.data  , showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.msf.graphs, showWarnings = FALSE, recursive = TRUE) 
    dir.create(path.local.msf.tables, showWarnings = FALSE, recursive = TRUE) 
  }
}



# set date to the Monday of the ISO week
make_epiweek_date <- function(date) {
  lubridate::wday(date, week_start = 1) <- 1
  return(date)
}


max_2 <- function(x) {
  # 2 most recent epi weeks to remove from charts
  x <- unique(x) %>% purrr::discard(is.na) %>% sort()
  n <- length(x)
  c(nth(x, n-1), nth(x, n))
}


# Formatting Confidence Intervals
combine_ci <- function(lwr, upr, digits = 1) {
  sprintf(glue("[%.{digits}f - %.{digits}f]"), 
        round(lwr, digits = digits),
        round(upr, digits = digits))
}


format_ci <- function(tbl) {
  tbl <- tbl %>% mutate(
    l_cnt_ci = case_when(
      !is.na(l_cnt_est) ~ combine_ci(l_cnt_lwr, l_cnt_upr), 
      TRUE ~NA_character_), 
    p_cnt_ci = case_when(
      !is.na(p_cnt_est) ~ combine_ci(p_cnt_lwr, p_cnt_upr), 
      TRUE ~NA_character_), 
    l_cml_ci = case_when(
      !is.na(l_cml_est) ~ combine_ci(l_cml_lwr, l_cml_upr), 
      TRUE ~NA_character_), 
    p_cml_ci = case_when(
      !is.na(p_cml_est) ~ combine_ci(p_cml_lwr, p_cml_upr), 
      TRUE ~NA_character_), 
    l_cnt_est = round(l_cnt_est, digits = 1), 
    p_cnt_est = round(p_cnt_est, digits = 1), 
    p_cml_est = round(p_cml_est, digits = 1), 
    l_cml_est = round(l_cml_est, digits = 1))
  
  return(tbl)
}



vars_trends <- function(model){
  
  stub_vars <- c('coeff', 'lwr', 'upr')
  
  lst_vars <- switch(model, 
                     linear       = c(paste0('l_cnt_', stub_vars), 'trend_linear'), 
                     quasipoisson = c(paste0('p_cnt_', stub_vars), 'trend_quasipoisson'))
  
  names(lst_vars) <- c(stub_vars, 'trend')
  
  return(lst_vars)
  
}


vars_doubling_time <- function(model, series){
  
  stub_vars <- c('est', 'lwr', 'upr')
  
  lst_vars <- switch(model, 
                     linear       = paste0('l_cnt_', stub_vars), 
                     quasipoisson = paste0('p_cnt_', stub_vars))
  
  names(lst_vars) <- switch(series, 
                     cases  = paste0('cases_' , stub_vars), 
                     deaths = paste0('deaths_', stub_vars))
  
  return(lst_vars)
  
}



attach_prefix <- function(var_in, suffix_var_out) {
  setNames(as.vector(var_in), paste0(suffix_var_out, var_in))
}





# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Specific functions for worldwide ECDC dataset
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

prepare_ecdc_dta <- function(dta){
  
  dta <- dta %>% 
    dplyr::mutate(date = lubridate::make_date(year, month, day)-1) %>% 
    dplyr::rename(geoid = geoId, country_ecdc = countriesAndTerritories, iso_a3 = countryterritoryCode, population_2019 = popData2019) %>% 
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # Complete missing infos
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country == "Congo - Brazzaville" ~ "Republic of Congo", 
        country_ecdc == 'Cases_on_an_international_conveyance_Japan' ~ 'Cruise Ship', 
        is.na(country) ~ gsub('_', ' ', country_ecdc), 
        TRUE ~ country), 
      iso_a3 = dplyr::case_when(
        country == 'Kosovo' ~ 'XKX', 
        country == 'Anguilla' ~ 'AIA', 
        country == 'Bonaire, Saint Eustatius and Saba' ~ 'BES', 
        country == 'Falkland Islands (Malvinas)' ~ 'FLK', 
        country == 'Montserrat' ~ 'MSR',  
        country == 'Taiwan' ~ 'TWN', 
        country == 'Western Sahara' ~ 'ESH', 
        country == 'Cruise Ship' ~ NA_character_, 
        TRUE ~ iso_a3), 
      continent = countrycode::countrycode(iso_a3, origin = 'iso3c', destination = 'continent'), 
      continent = dplyr::case_when(
        country == 'Kosovo' ~ 'Europe', 
        country == 'Cruise Ship' ~ 'Undefined', 
        is.na(country) ~ "Unknown", 
        TRUE ~ continent), 
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"), 
      region = dplyr::case_when(
        country == 'Kosovo' ~ 'Southern Europe', 
        country == 'Cruise Ship' ~ 'Undefined', 
        is.na(country) ~ "Unknown", 
        TRUE ~ region), 
      source = "ECDC"
    ) %>% 
    dplyr::select(dateRep, date, country_ecdc:geoid, country:region, iso_a3, cases, deaths, population_2019, source)
  
  return(dta)
  
}





# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Specific functions for MSF linelist dataset
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

clean_msf_dta <- function(dta) {
  
  dta <- dta %>% 
    mutate(
        age_in_years = case_when(
          age_in_years > 110 ~ NA_real_, 
          TRUE ~ age_in_years), 
      Comcond_immuno = case_when(
        grepl('Positive', MSF_hiv_status) ~ 'Yes', 
        TRUE ~ Comcond_immuno), 
      Comcond_cardi = case_when(
        MSF_hypertension == 'Yes' ~ 'Yes', 
        TRUE ~ Comcond_cardi))
  
  return(dta)
  
}


prepare_msf_dta <- function(dta, shorten_var_names = FALSE){
  
  # Create variable levels
  levels_covid_status <- c('Confirmed', 'Probable', 'Suspected', 'Not a case', '(Unknown)')
  levels_outcome_status <- c('Cured', 'Died', 'Left against medical advice', 'Transferred', 'Sent back home', 'Other')
  levels_ynu <- c('Yes', 'No', '(Unknown)')
  
  
  # Covid status
  dta <- dta %>% 
    mutate(
      MSF_covid_status = factor(MSF_covid_status, levels = levels_covid_status) %>% forcats::fct_explicit_na(na_level = '(Unknown)'))
  
  
  # Dates (and weeks)
  dta <- dta %>% 
    mutate(
      MSF_date_consultation = as.Date(MSF_date_consultation), 
      outcome_patcourse_status = factor(outcome_patcourse_status, levels = levels_outcome_status) %>% forcats::fct_explicit_na(na_level = '(Pending/Unknown)'),
      epi_week_report = make_epiweek_date(report_date),
      epi_week_consultation = make_epiweek_date(MSF_date_consultation),
      epi_week_admission = make_epiweek_date(patcourse_presHCF),
      epi_week_onset = make_epiweek_date(patcourse_dateonset)
    )
  
  
  # Age
  age_breaks_5 <- c(0, 5, 15, 45, 65, Inf)
  age_labels_5 <- label_breaks(age_breaks_5, exclusive = TRUE)
  
  age_breaks_9 <- c(seq(0, 80, 10), Inf)
  age_labels_9 <- label_breaks(age_breaks_9, exclusive = TRUE)
  
  dta <- dta %>% 
    mutate(
      age_in_years = floor(age_in_years), 
      age_5gp = cut(age_in_years, breaks = age_breaks_5, labels = age_labels_5, include.lowest = TRUE, right = FALSE),
      age_9gp = cut(age_in_years, breaks = age_breaks_9, labels = age_labels_9, include.lowest = TRUE, right = FALSE)
    )
  
  
  # Recoding Comorbidities as Yes/No
  dta <- dta %>% 
    mutate(
      MSF_malaria = case_when(
        MSF_malaria == 'Positive' ~ 'Yes', 
        MSF_malaria == 'Negative' ~ 'No', 
        MSF_malaria %in% c('Inconclusive', 'Not done') ~ 'Unknown', 
        TRUE ~ MSF_malaria), 
      MSF_hiv_status = case_when(
        MSF_hiv_status %in% c('Positive (no ARV)', 'Positive (on ART)', 'Positive (unknown)') ~ 'Yes', 
        MSF_hiv_status == 'Negative' ~ 'No', 
        TRUE ~ MSF_hiv_status), 
      MSF_tb_active = case_when(
        MSF_tb_active %in% c('Yes (currently no treatment)', 'Yes (currently on treatment)', 'Yes (unknown)') ~ 'Yes', 
        TRUE ~ MSF_tb_active), 
      MSF_smoking = case_when(
        MSF_smoking %in% c('Yes (current)', 'Yes (former)') ~ 'Yes', 
        TRUE ~ MSF_smoking))
  
  
  # Recode presence of comorbidities including the MSF ones
  Comcond_count <- rowSums(select(dta, starts_with('Comcond_'), MSF_hiv_status, MSF_hypertension, MSF_tb_active, MSF_malaria, MSF_malnutrition, MSF_smoking) == "Yes", na.rm = TRUE)
  
  Comcond_01 <- ifelse(Comcond_count > 0, 1, 0)
  
  dta <- cbind(dta, Comcond_count, Comcond_01)
  
  
  # Patients' care variables
  dta <- dta %>% 
    mutate(
      patcourse_admit = factor(patcourse_admit, levels = levels_ynu) %>% forcats::fct_explicit_na(na_level = levels_ynu[3]), 
      outcome_patcourse_admit = factor(outcome_patcourse_admit, levels = levels_ynu) %>% forcats::fct_explicit_na(na_level = '(Unknown)'), 
      merge_admit = case_when(
        patcourse_admit == 'Yes' ~ levels_ynu[1], 
        outcome_patcourse_admit == 'Yes' ~ levels_ynu[1], 
        is.na(outcome_patcourse_admit) ~ levels_ynu[3], 
        TRUE ~ levels_ynu[2]) %>% factor(levels = levels_ynu), 
      merge_oxygen = recode_care(MSF_received_oxygen, MSF_outcome_received_oxygen), 
      merge_icu    = recode_care(patcourse_icu , outcome_patcourse_icu), 
      merge_vent   = recode_care(patcourse_vent, outcome_patcourse_vent), 
      merge_ecmo   = recode_care(patcourse_ecmo, outcome_patcourse_ecmo)) 
  

  # Shorten variables names to make easy to handle them
  if (shorten_var_names) {
    
    var_names_stub <- c('^patinfo_', '^patcourse_', '^MSF_', '_patcourse')
    
    for (i in var_names_stub) {
      names(dta) <- gsub(i, '', names(dta))
    }
    
  }
  
  return(as_tibble(dta))
}


prepare_msf_dta_comcond <- function(dta){
  
  dta <- dta %>% 
    select(continent, covid_status, outcome_status, starts_with('Comcond_'), hiv_status, hypertension, tb_active, malaria, malnutrition, smoking) %>% 
    select(-c(Comcond_present, Comcond_pregt))
  
  return(dta)
}


df_labels_comcond <- data.frame(
  levels = c('Comcond_cardi', 'Comcond_diabetes', 'Comcond_immuno', 'Comcond_liver', 'Comcond_lung', 'Comcond_malig', 'Comcond_neuro', 'Comcond_other', 'Comcond_partum', 'Comcond_preg', 'Comcond_renal', 'hiv_status', 'hypertension', 'malaria', 'malnutrition', 'smoking', 'tb_active'),
  labels = c('Cardiovascular (including hypertention)', 'Diabetes', 'Immunological (including HIV)','Hepatic', 'Respiratory (including chronic lung diseases)', 'Cancer', 'Neurological', 'Other condition', 'Post-partum', 'Pregnancy', 'Renal', 'HIV', 'Hypertension', 'Malaria', 'Malnutrition', 'Smoking', 'TB'))




recode_care <- function(var1, var2){
  case_when(
    var1 == 'Yes' ~ 'Yes', 
    var2 == 'Yes' ~ 'Yes', 
    var1 == 'No' & (var2 == 'Unknown' | is.na(var2)) ~ 'No at admission then not reported', 
    (var1 == 'No' | is.na(var1)) & var2 == 'No' ~ 'No at any time', 
    TRUE ~ 'Not reported') %>% 
    factor(levels = c('Yes', 'No at admission then not reported', 'No at any time', 'Not reported'))
}



