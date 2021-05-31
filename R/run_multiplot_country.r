
# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- LOOPS to plot graphs for each country 
# --- --- --- --- --- --- --- --- --- --- --- --- 


# Setup environment -------------------------------------------------------

# source(here::here('R',   'setup.R'),            encoding = 'UTF-8')
# source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")

# dates_and_week <- set_date_frame(create_folders = FALSE)
# date_min_report <- dates_and_week[[1]]
# date_max_report <- dates_and_week[[2]]
# week_report     <- dates_and_week[[3]]


library(patchwork)


##### Data #####

# Get jhu data either from the web or from the saved RDS file
rds_jhu <- readRDS(file.path(path.local.worldwide.data, 'dta_jhu.RDS'))

# Get trends
trend_models     <- readRDS(file.path(path.local.worldwide.data, 'trends_models.RDS'))
trend_models_new <- readRDS(file.path(path.local.worldwide.data, 'trends_models_new.RDS'))



# Countries list dataset
df_countries <- readRDS(file.path(path.local.data, paste0('df_countries','.RDS')))



# Create dedicated folders (there might be too many plots)
path.local.worldwide.graphs.country_trends <- file.path(path.local.worldwide.graphs, 'country_trends')

path.local.worldwide.graphs.country_case_fatality <- file.path(path.local.worldwide.graphs, 'country_case_fatality')

path.local.worldwide.graphs.country_growth_rates <- file.path(path.local.worldwide.graphs, 'country_growth_rates')


dir.create(path.local.worldwide.graphs.country_trends, 
           showWarnings = FALSE, recursive = TRUE) 
dir.create(path.local.worldwide.graphs.country_case_fatality, 
           showWarnings = FALSE, recursive = TRUE) 
dir.create(path.local.worldwide.graphs.country_growth_rates, 
           showWarnings = FALSE, recursive = TRUE) 



# Calculate trends --------------------------------------------------------

lst_dta_jhu <- rds_jhu %>% 
  tidyr::drop_na(iso_a3) %>% 
  filter(between(date, 
                 left = date_min_report, 
                 right = date_max_report)) %>% 
  multisplit("iso_a3")


df_countries <- df_countries %>% 
  inner_join(rds_jhu %>% select(iso_a3) %>% 
               distinct(),
             by = "iso_a3")


# model_cnt_cases_linear_short <- trend_models$model_cnt_cases_linear_short
# model_cnt_cases_linear_long  <- trend_models$model_cnt_cases_linear_long
# 
# model_cnt_deaths_linear_short <- trend_models$model_cnt_deaths_linear_short
# model_cnt_deaths_linear_long  <- trend_models$model_cnt_deaths_linear_long

lst_coeffs_cases  <- trend_models$lst_coeffs_cases
lst_coeffs_deaths <- trend_models$lst_coeffs_deaths




# Plot cases and trends ---------------------------------------------------

# --- Plot and save SIX graphs with counts of cases and deaths and trend with two different period lengths 

# To filter which countries to plot 
country_list <- df_countries %>% 
  filter(iso_a3 != "HKG",
         # iso_a3 != "FSM",
         # iso_a3 != "MHL",
         # iso_a3 != "TJK",
         # iso_a3 != "TZA",
         # iso_a3 != "VAT",
         # iso_a3 != "VUT"
         ) %>% 
  arrange(iso_a3)


rds_jhu %>% 
  filter(iso_a3 %in% country_list$iso_a3) %>% 
  pivot_longer(cases:deaths, 
               names_to = 'obs', 
               values_to = 'count') %>% 
  left_join(trend_models_new$tbl_preds_all, by = c("country", "date", "obs")) %>% 
  multisplit("iso_a3") %>% 
  map(country_six_plots)



# Loop of plots
# for (i in country_list$iso_a3){
#   
#   # name_country <- country_list %>% 
#   #   filter(iso_a3 == i) %>%
#   #   pull(country) %>% 
#   #   gsub(" ", "_", .) %>% 
#   #   gsub("_\\(country\\)", "", .)
#   
#   # print(paste(i, name_country))
# 
#     plots <- country_six_plots(country_iso = i,
#                                lst_dta     = lst_dta_jhu, 
#                                countries   = df_countries)
#     
#     ggsave(file.path(path.local.worldwide.graphs.country_trends,
#                      glue("trends_{name_country}_{week_report}.png")),
#            plot = plots,
#            scale = 1,
#            width = 9,
#            dpi = 320)
#   
# }


# Plot growth rates -------------------------------------------------------

# --- Plot growth rate curves by country (LOOP) 

# To filter which countries to plot 
country_list <- df_countries %>% filter(iso_a3 != "HKG")


for (i in country_list$iso_a3){
  name_country <- country_list %>% 
    filter(iso_a3 == i) %>% 
    pull(country) %>% 
    gsub(" ", "_", .)
  
  temp_plot <- country_plot_coeff('cases', i)
  
  ggsave(file.path(path.local.worldwide.graphs.country_growth_rates, 
                   glue("growth_rates_{name_country}_{week_report}.png")), 
         plot = temp_plot, 
         scale = 1, 
         dpi = 320)
}



# Plot CFR ----------------------------------------------------------------

# --- Plot case fatality curve by country

for (i in names(lst_dta_jhu)) {
  
  dta <- lst_dta_jhu[[i]]
  
  name_country <- unique(dta$country) 
  last_date <- max(dta$date)
  
  temp_plot <- plot_cfr_ma(dta) + 
    plot_annotation(title = glue("Evolution of the Covid-19 case fatality in {name_country}"), 
                    subtitle = glue("update until {format(last_date, '%d %B %Y')}"))
  
  ggsave(file.path(path.local.worldwide.graphs.country_case_fatality, 
                   glue("case_fatality_{name_country}_{week_report}.png")), 
         plot = temp_plot, 
         scale = 1, 
         height = 5, 
         width = 9,
         dpi = 320)
}

