# ---- Prepare environment ----

# source(file.path(path.R, "setup.R")  , encoding = "UTF-8")
# source(file.path(path.R, 'utils_management.R'), encoding = 'UTF-8')
# source(file.path(path.R, 'utils_vis.R')       , encoding = 'UTF-8')
source(file.path(path.R, 'utils_modelling.R') , encoding = 'UTF-8')

path_world_continent_growth_rates <- file.path(path.local.worldwide.graphs,
                                               'world_continent_growth_rates')

dir.create(path_world_continent_growth_rates, 
           showWarnings = FALSE, recursive = TRUE) 

# Load data
load(file.path(path.local.worldwide.data, glue('episitrep_worldwide_analyses_{week_report}.RData')))



# Get coeff ---------------------------------------------------------------

world_coeffs <- dta_jhu_right_censored %>% 
  count(date, wt = cases, name = "cases") %>% 
  arrange(date) %>% 
  ts_coeff_single(series = "cases")

continent_coeffs <- 
  c("Asia", "Africa", "Oceania", "Europe", "Americas") %>% 
  setNames(nm = .) %>% 
  purrr::map(~{
    dta_jhu_right_censored %>% 
      filter(continent == .x) %>% 
      count(date, wt = cases, name = "cases") %>% 
      arrange(date) %>% 
      ts_coeff_single(series = "cases")
  })

all_coeffs <- c(list("World" = world_coeffs), continent_coeffs)



# Plot --------------------------------------------------------------------

plot_list <- c("World", "Asia", "Africa", "Oceania", "Europe", "Americas")

all_plots <- plot_list %>% 
  setNames(nm = .) %>% 
  purrr::map(~{
    df <- all_coeffs[[.x]]
    plot_coeff(df, .x, series = "cases")
  })



# Save plots --------------------------------------------------------------

plot_list %>% 
  purrr::walk(
    ~ggsave(
      fs::path(path_world_continent_growth_rates, glue("growth_rates_{.x}_{week_report}"), ext = "png"), 
      plot = all_plots[[.x]], scale = 1, dpi = 320, width = 10, height = 6
    )
  )
