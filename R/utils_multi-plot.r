
# --- --- --- --- --- --- --- --- --- --- --- --- 
# --- Plot trends all country (LOOP) 
# --- --- --- --- --- --- --- --- --- --- --- --- 

rm(list = ls())

source(here::here('R', 'setup.R'), encoding = 'UTF-8')
source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")
source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")


load(file.path(path.local.worldwide.data, glue('episitrep_worldwide_analyses_{week_report}.RData')))


# Create a dedicated folder (there might be too many plots)
path.local.worldwide.graphs.country_trends <- file.path(path.local.worldwide.graphs, 'country_trends')
dir.create(path.local.worldwide.graphs.country_trends, showWarnings = FALSE, recursive = TRUE) 

# To filter which countries to plot 
country_list <- df_countries

# Loop of plots
for (i in country_list$iso_a3[!is.na(country_list$iso_a3)]){
  name_country <- country_list %>% filter(iso_a3 == i) %>% pull(country) %>% gsub(" ", "_", .)
  plots <- country_multi_plots(i)
  ggsave(file.path(path.local.worldwide.graphs.country_trends, glue("trends_{name_country}_{week_report}.png")), 
         plot = plots, 
         scale = 1, 
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

