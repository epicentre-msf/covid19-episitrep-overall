

source(file.path(path.R, "utils_modelling.R") , encoding = "UTF-8")



if (update_models) {
  
  # <!-- Modelling Cases Trends -->
  
  model_cnt_cases_linear  <- linear_model_cnt(series = 'cases', 
                                              lst_dta = lst_ecdc, 
                                              last_date = date_max_report, 
                                              time_unit_extent = period_trends, 
                                              min_sum = 30)
  
  model_cnt_cases_linear_30d  <- linear_model_cnt(series = 'cases', 
                                                  lst_dta = lst_ecdc, 
                                                  last_date = date_max_report, 
                                                  time_unit_extent = 30, 
                                                  min_sum = 30)
  
  
  model_cml_cases_linear  <- linear_model_cml(series = 'cases', 
                                              lst_dta = lst_ecdc, 
                                              last_date = date_max_report, 
                                              time_unit_extent = period_trends, 
                                              min_sum = 50)
  
  model_cnt_cases_quasipoisson <- suppressMessages(
    quasipoisson_model_cnt(series = 'cases', 
                           lst_dta = lst_ecdc, 
                           last_date = date_max_report, 
                           time_unit_extent = period_trends, 
                           min_sum = 30))
  
  model_cml_cases_quasipoisson <- suppressMessages(
    quasipoisson_model_cml(series = 'cases', 
                           lst_dta = lst_ecdc, 
                           last_date = date_max_report, 
                           time_unit_extent = period_trends, 
                           min_sum = 50))
  
  # <!-- Modelling Deaths Trends -->
  
  model_cnt_deaths_linear  <- linear_model_cnt(series = 'deaths', 
                                               lst_dta = lst_ecdc, 
                                               last_date = date_max_report, 
                                               time_unit_extent = period_trends, 
                                               min_sum = 30)
  
  model_cnt_deaths_linear_30d  <- linear_model_cnt(series = 'deaths', 
                                                   lst_dta = lst_ecdc, 
                                                   last_date = date_max_report, 
                                                   time_unit_extent = 30, 
                                                   min_sum = 30)
  
  model_cml_deaths_linear  <- linear_model_cml(series = 'deaths', 
                                               lst_dta = lst_ecdc, 
                                               last_date = date_max_report, 
                                               time_unit_extent = period_trends, 
                                               min_sum = 50)
  
  model_cnt_deaths_quasipoisson <- suppressMessages(
    quasipoisson_model_cnt(series = 'deaths',
                           lst_dta = lst_ecdc, 
                           last_date = date_max_report, 
                           time_unit_extent = period_trends, 
                           min_sum = 30))
  
  model_cml_deaths_quasipoisson <- suppressMessages(
    quasipoisson_model_cml(series = 'deaths', 
                           lst_dta = lst_ecdc, 
                           last_date = date_max_report, 
                           time_unit_extent = period_trends, 
                           min_sum = 50))
  
  # <!-- Save models in a unique RData -->
  save(model_cnt_cases_linear, 
       model_cnt_cases_linear_30d, 
       model_cml_cases_linear,
       model_cnt_deaths_linear_30d, 
       model_cnt_deaths_linear,
       model_cml_deaths_linear,
       model_cnt_cases_quasipoisson, 
       model_cml_cases_quasipoisson, 
       model_cnt_deaths_quasipoisson, 
       model_cml_deaths_quasipoisson, 
       file = file.path(path.local.worldwide.data, 'models_trends.RData'))
  
} else {
  
    load(file = file.path(path.local.worldwide.data, 'models_trends.RData'))

}






# === === === === === === === === === 
# Upload or create daily coefficients
# === === === === === === === === === 


if (!file.exists(file.path(path.local.worldwide.data, 'lst_coeffs_cases.RDS')) | update_models) {
  lst_coeffs_cases <- ts_coeff(series = 'cases' , lst_dta = lst_ecdc, time_unit_extent = 5, ma_window = 3, min_sum = 30)
  saveRDS(lst_coeffs_cases, file = file.path(path.local.worldwide.data, paste0('lst_coeffs_cases','.RDS'))) 
} else {
  lst_coeffs_cases <- readRDS(file = file.path(path.local.worldwide.data, paste0('lst_coeffs_cases','.RDS')))
}


if (!file.exists(file.path(path.local.worldwide.data, 'lst_coeffs_deaths.RDS')) | update_models) {
  lst_coeffs_deaths <- ts_coeff(series = 'deaths', lst_dta = lst_ecdc, time_unit_extent = 5, ma_window = 3, min_sum = 30)
  saveRDS(lst_coeffs_deaths, file = file.path(path.local.worldwide.data, paste0('lst_coeffs_deaths','.RDS')))
} else {
  lst_coeffs_deaths <- readRDS(file = file.path(path.local.worldwide.data, paste0('lst_coeffs_deaths','.RDS')))
}

