
# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- LOOPS to plot graphs for each country 
# --- --- --- --- --- --- --- --- --- --- --- --- 

rm(list = ls())


# Setup environment -------------------------------------------------------

# source(here::here('R',   'setup.R'),            encoding = 'UTF-8')
# source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
# source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")



dates_and_week <- set_date_frame(create_folders = FALSE)

date_min_report <- dates_and_week[[1]]
date_max_report <- dates_and_week[[2]]
week_report     <- dates_and_week[[3]]



# Get ECDC data either from the web or from the saved RDS file
rds_ecdc <- readRDS(file.path(path.local.worldwide.data, 'dta_jhu.RDS'))

lst_dta_ecdc <- rds_ecdc %>% 
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
country_list <- df_countries %>% filter(iso_a3 != "HKG")

# Loop of plots
for (i in country_list$iso_a3){
  print(i)
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
country_list <- df_countries %>% filter(iso_a3 != "HKG")


for (i in country_list$iso_a3){
  name_country <- country_list %>% 
    filter(iso_a3 == i) %>% 
    pull(country) %>% 
    gsub(" ", "_", .)
  
  temp_plot <- country_plot_coeff('cases' , i)
  
  ggsave(file.path(path.local.worldwide.graphs.country_growth_rates, glue("growth_rates_{name_country}_{week_report}.png")), 
         plot = temp_plot, 
         scale = 1, 
         dpi = 320)
}




# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- Plot case fatality curve by country
# --- --- --- --- --- --- --- --- --- --- --- --- 

library(patchwork)

# Create a dedicated folder
path.local.worldwide.graphs.country_case_fatality <- file.path(path.local.worldwide.graphs, 'country_case_fatality')
dir.create(path.local.worldwide.graphs.country_case_fatality, showWarnings = FALSE, recursive = TRUE) 


plot_cfr_ma <- function(dta){
  
  dta_cfr <- dta %>% 
    select(date, continent, region, country, iso_a3, cases, deaths) %>% 
    tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                    max(date, na.rm = TRUE), by = 1), 
                    fill = list(cases = 0, deaths = 0)) %>% 
    mutate(
      cases_lagged = dplyr::lag(cases, 8), 
      cases_ma = forecast::ma(cases_lagged, order = 5), 
      deaths_ma = forecast::ma(deaths, order = 5), 
      cfr = deaths_ma / cases_ma) %>% 
    mutate(
      cfr = as.double(cfr), 
      cfr = case_when(
        cfr > 1 ~ NA_real_, 
        TRUE ~ cfr))
  
  dta_cfr %>% as.data.frame()
  
  
  # Parameters
  main_colour  <- c(cases = '#1A62A3', deaths = '#e10000')
  name_country <- unique(dta$country)
  date_min     <- dta_cfr %>% filter(cases != 0) %>% pull(date) %>% min()
  
  n_max <- max(dta_cfr$deaths, na.rm = TRUE)
  p_max <- rounder(max(dta_cfr$cfr, na.rm = TRUE), .1)
  p_max <- ifelse(p_max > 1, 1, p_max)
  scaling_factor <- p_max / n_max # sets the y limit of the proportion rounded up to nearest 10% above the max
  
  
  p1 <- ggplot(dta_cfr, aes(x = date, y = cases)) + 
    geom_col(fill = '#1A62A3', width = 1) + 
    scale_x_date(name = NULL, date_breaks = "2 months", date_labels = "%b\n%Y") + 
    scale_y_continuous(name = 'Cases') + 
    theme_light() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  p2 <- ggplot(dta_cfr, aes(x = date, y = deaths)) + 
    geom_col(fill = '#de2d26', width = 1) + 
    geom_line(data = dta_cfr,
              aes(y = cfr / scaling_factor),
              key_glyph = "timeseries", size = 1) + 
    scale_x_date(name = NULL, date_breaks = "2 months", date_labels = "%b\n%Y") + 
    scale_y_continuous(name = 'Deaths', sec.axis = ggplot2::sec_axis(~ . * scaling_factor, name = "Case fatality (lag 8 days)", labels = scales::percent_format(accuracy = 1))) + 
    labs(fill = NULL, colour = NULL) + 
    theme_light() + 
    theme(legend.position = "top", legend.direction = 'vertical', legend.text = element_text(size = 11))
  
  p1 + p2 + plot_layout(ncol = 1)
  
}


for (i in names(lst_dta_ecdc)) {
  
  dta <- lst_dta_ecdc[[i]]
  
  name_country <- unique(dta$country) 
  last_date <- max(dta$date)
  
  temp_plot <- plot_cfr_ma(dta) + plot_annotation(title = glue("Evolution of the Covid-19 case fatality in {name_country}"), subtitle = glue("update until {format(last_date, '%d %B %Y')}"))
  
  ggsave(file.path(path.local.worldwide.graphs.country_case_fatality, glue("case_fatality_{name_country}_{week_report}.png")), 
         plot = temp_plot, 
         scale = 1, 
         height = 5, 
         width = 9,
         dpi = 320)
}

