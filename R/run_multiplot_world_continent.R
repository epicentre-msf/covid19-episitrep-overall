
source(file.path(path.R, "setup.R")  , encoding = "UTF-8")

# Upload functions
source(file.path(path.R, 'utils_management.R'), encoding = 'UTF-8')
source(file.path(path.R, 'utils_vis.R')       , encoding = 'UTF-8')

source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")

load(file.path(path.local.worldwide.data, glue('episitrep_worldwide_analyses_{week_report}.RData')))


ts_coeff_single <- function(dta, series = "cases", time_unit_extent = 5, ma_window = 3, min_sum = 30){
  
  dta <- dta %>% 
    tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                    max(date, na.rm = TRUE), by = 1), 
                    fill = list(series = NA_real_))
  
  seq_dates <- if(length(dta$date) < time_unit_extent) {
    min(dta$date) 
  } else {
    seq.Date(min(dta$date, na.rm = TRUE) + (time_unit_extent - 1) / 2, 
             max(dta$date, na.rm = TRUE) - (time_unit_extent - 1) / 2, 
             by = 1)
  }
  
  dta <- dta %>% 
    mutate(
      mov_av = as.double(forecast::ma(dta[series], order = ma_window)) %>% 
        na_if(., 0)) # Replace 0 values with NA
  
  tbl_coeffs <- tibble(date  = as.character(), 
                       coeff = numeric(), 
                       lwr   = numeric(), 
                       upr   = numeric())
  
  for (i in as.character(seq_dates)) {
    
    temp <- dta %>% 
      filter(between(date, 
                     as.Date(i) - (time_unit_extent - 1) / 2, 
                     as.Date(i) + (time_unit_extent - 1) / 2))
    
    if (sum(!is.na(temp['mov_av'])) > 2 & sum(temp['mov_av'], na.rm = TRUE) > min_sum) {
      mdl  <- lm(log(mov_av) ~ date, data = temp)
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
    } else {
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(date = i, mdl_coeffs)
    
  }
  
  dta_coeff <- left_join(dta,  tbl_coeffs %>% mutate(date = as.Date(date)))
  
  return(dta_coeff)
}



plot_coeff <- function(dta, name, series = "cases") {
  
  name_country <- name
  
  quo_series <- sym(series)
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  date_min <- min(dta$date, na.rm = TRUE)
  date_max <- max(dta$date, na.rm = TRUE)
  
  plot_crv <- ggplot(dta, aes(x = date, y = !!quo_series)) + 
    geom_col(colour = main_colour,  fill = main_colour) + 
    scale_y_continuous(labels = scales::label_number_si()) +
    xlim(c(date_min, date_max)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = glue('Number of {series} reported')) + 
    theme_light()
  
  plot_cff <- ggplot(dta, aes(x = date)) +
    geom_hline(yintercept = 0, colour = "red", alpha = .6) +
    geom_line(aes(y = coeff), colour = '#1B9E77', size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
    xlim(c(date_min, date_max)) + 
    #scale_x_date(date_breaks = "4 weeks", date_labels = "%d %b") + 
    xlab(NULL) + 
    ylab('Slope coefficient') + 
    labs(subtitle = 'Slope coefficient curve') + 
    theme_light()
  
  grid.arrange(rbind(ggplotGrob(plot_crv), ggplotGrob(plot_cff)), 
               top = textGrob(glue('Evolution of the slope cofficient in {name_country}'), 
                              gp = gpar(fontface = 'bold')))
  
}

world_coeffs <- dta_ecdc_right_censored %>% 
  count(date, wt = cases, name = "cases") %>% 
  arrange(date) %>% 
  ts_coeff_single(series = "cases")

continent_coeffs <- 
  c("Asia", "Africa", "Oceania", "Europe", "Americas") %>% 
  setNames(nm = .) %>% 
  purrr::map(~{
    dta_ecdc_right_censored %>% 
      filter(continent == .x) %>% 
      count(date, wt = cases, name = "cases") %>% 
      arrange(date) %>% 
      ts_coeff_single(series = "cases")
  })

all_coeffs <- c(list("World" = world_coeffs), continent_coeffs)

plot_list <- c("World", "Asia", "Africa", "Oceania", "Europe", "Americas")

all_plots <- plot_list %>% 
  setNames(nm = .) %>% 
  purrr::map(~{
    df <- all_coeffs[[.x]]
    plot_coeff(df, .x, series = "cases")
  })



## Save plots
path_world_continent_growth_rates <- file.path(path.local.worldwide.graphs, 'world_continent_growth_rates')
dir.create(path_world_continent_growth_rates, showWarnings = FALSE, recursive = TRUE) 

plot_list %>% 
  purrr::walk(
    ~ggsave(
      fs::path(path_world_continent_growth_rates, glue("growth_rates_{.x}_{week_report}"), ext = "png"), 
      plot = all_plots[[.x]], scale = 1, dpi = 320, width = 10, height = 6
    )
  )
