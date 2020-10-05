
# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- LOOPS to plot graphs for each country 
# --- --- --- --- --- --- --- --- --- --- --- --- 

rm(list = ls())

source(here::here('R', 'setup.R'), encoding = 'UTF-8')
source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")
source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")


# Get ECDC data either from the web or from the saved RDS file
rds_ecdc <- readRDS(file.path(path.local.worldwide.data, 'dta_ECDC.RDS'))

lst_dta_ecdc <- rds_ecdc[[1]] %>% 
  prepare_ecdc_dta() %>% 
  tidyr::drop_na(iso_a3) %>% 
  filter(between(date, left = NULL, right = date_max_report)) %>% 
  multisplit("iso_a3")

# Get trends
trend_models <- readRDS(file.path(path.local.worldwide.data, 'trends_models.RDS'))

model_cnt_cases_linear_short <- trend_models$model_cnt_cases_linear_short
model_cnt_cases_linear_long  <- trend_models$model_cnt_cases_linear_long

model_cnt_deaths_linear_short <- trend_models$model_cnt_deaths_linear_short
model_cnt_deaths_linear_long  <- trend_models$model_cnt_deaths_linear_long

lst_coeffs_cases  <- trend_models$lst_coeffs_cases
lst_coeffs_deaths <- trend_models$lst_coeffs_deaths

# Countries list dataset
df_countries <- readRDS(file.path(path.local.data, paste0('df_countries','.RDS')))


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# --- Plot SIX graphs with counts of cases and deaths and trend with two different period lengths 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 


# Create a dedicated folder (there might be too many plots)
path.local.worldwide.graphs.country_trends <- file.path(path.local.worldwide.graphs, 'country_trends')
dir.create(path.local.worldwide.graphs.country_trends, showWarnings = FALSE, recursive = TRUE) 

# To filter which countries to plot 
country_list <- df_countries

# Loop of plots
for (i in country_list$iso_a3){
  name_country <- country_list %>% filter(iso_a3 == i) %>% pull(country) %>% gsub(" ", "_", .)
  plots <- country_six_plots(country_iso = i)
  ggsave(file.path(path.local.worldwide.graphs.country_trends, glue("trends_{name_country}_{week_report}.png")), 
         plot = plots, 
         scale = 1, 
         width = 9,
         dpi = 320)
}



# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- Plot growth rate curves by country (LOOP) 
# --- --- --- --- --- --- --- --- --- --- --- --- 

# Create a dedicated folder (there might be too many plots)
path.local.worldwide.graphs.country_growth_rates <- file.path(path.local.worldwide.graphs, 'country_growth_rates')
dir.create(path.local.worldwide.graphs.country_growth_rates, showWarnings = FALSE, recursive = TRUE) 

# To filter which countries to plot 
country_list <- df_countries


for (i in country_list$iso_a3){
  name_country <- country_list %>% filter(iso_a3 == i) %>% pull(country) %>% gsub(" ", "_", .)
  temp_plot <- country_plot_coeff('cases' , i)
  ggsave(file.path(path.local.worldwide.graphs.country_growth_rates, glue("growth_rates_{name_country}_{week_report}.png")), 
         plot = temp_plot, 
         scale = 1, 
         dpi = 320)
}

