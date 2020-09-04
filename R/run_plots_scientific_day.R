

## === === === === === === === === 
## Plots for Scientific day
## === === === === === === === === 


source(here::here('R', 'setup.r'), encoding = 'UTF-8')

# Upload functions
source(file.path(path.R, 'utils_management.R'), encoding = 'UTF-8')
source(file.path(path.R, 'utils_vis.R')       , encoding = 'UTF-8')

source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")


library(patchwork)

path.sharepoint.scientific.day.2020 <- file.path(path.sharepoint, 'template', 'scientific-day_2020')



## --- --- --- --- --- --- --- --- --- --- --- --- --- ---
## ---- GRID Epidemic curve and growth rates by continent 
## --- --- --- --- --- --- --- --- --- --- --- --- --- ---


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


## Plot 1 - Epidemic curve with continents

continent_ordered <- c("Asia", "Europe", "Americas", "Africa", "Oceania")

hist_epi_curve_world <- dta_ecdc_right_censored %>% 
  mutate(continent = factor(continent, levels =  continent_ordered)) %>% 
  ggplot(aes(x = date, y = cases, fill = continent)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Number of Cases") + 
  theme_light() + 
  theme(legend.position = "top", legend.title = element_blank())


## Plot 2 - Growth rate as multi-facet by continents

### Calculate growth rate by continent

growth_rate_world <- dta_ecdc_right_censored %>% 
  count(date, wt = cases, name = "cases") %>% 
  arrange(date) %>% 
  ts_coeff_single(series = "cases", time_unit_extent = 12, ma_window = 7) %>% 
  mutate(
    gr  = exp(coeff), 
    lwr = exp(lwr), 
    upr = exp(upr)) %>% 
  select(date, cases, gr, lwr, upr)

growth_rate_continent <- continent_ordered %>% 
  setNames(nm = .) %>% 
  purrr::map(~{
    dta_ecdc_right_censored %>% 
      filter(continent == .x) %>% 
      count(date, wt = cases, name = "cases") %>% 
      arrange(date) %>% 
      ts_coeff_single(series = "cases", time_unit_extent = 12, ma_window = 7) %>% 
      mutate(
        gr  = exp(coeff), 
        lwr = exp(lwr), 
        upr = exp(upr)) %>% 
      select(date, cases, gr, lwr, upr)
  })

growth_rate_all <- c(list("World" = growth_rate_world), growth_rate_continent)

df_growth_rate_all <- growth_rate_all %>% 
  bind_rows(.id = "geo_unit") %>% 
  mutate(geo_unit = factor(geo_unit, levels = c('World', continent_ordered)))


lines_growth_rate <- ggplot(df_growth_rate_all, aes(x = date)) + 
  facet_wrap(~geo_unit, ncol = 3, dir = 'h') + 
  geom_hline(yintercept = 1, colour = "red", alpha = .6) +
  geom_line(aes(y = gr), colour = '#1B9E77', size = 0.75) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = function(x) paste0(round((x - 1) * 100, 1), "%")) + # Transform the rate in percentage increase
  labs(x = NULL, y = 'Growth rate') + 
  theme_light()

graph_epicurve_trends_by_continent <- hist_epi_curve_world + 
  lines_growth_rate + 
  plot_layout(ncol = 1)


hist_epi_curve_world
lines_growth_rate
graph_epicurve_trends_by_continent


ggsave(file.path(path.sharepoint.scientific.day.2020, glue("graph_epicurve_trends_by_continent_{week_report}.png")), 
       plot = graph_epicurve_trends_by_continent, 
       scale = 1, 
       dpi = 720, 
       width = 15, 
       height = 9)


ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('lines_growth_rate', '_', week_report,'.png')), 
       plot = lines_growth_rate, 
       scale = 0.8, 
       dpi = 720, 
       width = 10, 
       height = 6)




## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
## ---- FACET epcurve by continent
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

hist_epicurve_by_continent <- dta_ecdc_right_censored %>% 
  mutate(continent = factor(continent, levels = continent_ordered)) %>% 
  ggplot(aes(x = date, y = cases, fill = continent)) + 
  facet_wrap(vars(continent)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Number of Cases") + 
  theme_light() + 
  theme(legend.position = "none")

hist_epicurve_by_continent

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_epicurve_by_continent', '_', week_report, '.png')), 
       plot = hist_epicurve_by_continent, 
       scale = 0.7, 
       dpi = 720, 
       width = 15, 
       height = 9)



dta_ecdc_count_continents <- dta_ecdc_right_censored %>% 
  select(geo_unit = continent, date, cases, deaths)

dta_ecdc_count_world <- dta_ecdc_right_censored %>% 
  group_by(date) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE), 
    deaths = sum(cases, na.rm = TRUE)) %>% 
  mutate(
    geo_unit = "World")


dta_ecdc_count_continents_plus_world <- dta_ecdc_count_continents %>% 
   add_row(dta_ecdc_count_world)


hist_epicurve_world <- dta_ecdc_count_continents_plus_world %>% 
  mutate(continent = factor(geo_unit, levels = c('World', continent_ordered))) %>% 
  ggplot(aes(x = date, y = cases, fill = geo_unit)) + 
  facet_wrap(vars(geo_unit)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Number of Cases") + 
  theme_light() + 
  theme(legend.position = "none")

hist_epicurve_world

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_epicurve_world', '_', week_report, '.png')), 
       plot = hist_epicurve_world, 
       scale = 0.7, 
       dpi = 720, 
       width = 15, 
       height = 9)




hist_epicurve_by_continent <- dta_ecdc_right_censored %>% 
  mutate(continent = factor(continent, levels = continent_ordered)) %>% 
  ggplot(aes(x = date, y = cases, fill = continent)) + 
  facet_wrap(vars(continent), nrow = 1) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Number of Cases") + 
  theme_light() + 
  theme(legend.position = "none")

df_growth_rate_continent <- growth_rate_continent %>% 
  bind_rows(.id = "continent") %>% 
  mutate(continent = factor(continent, levels = continent_ordered))

lines_growth_rate_continent <- ggplot(df_growth_rate_continent, aes(x = date)) + 
  facet_wrap(vars(continent), nrow = 1) + 
  geom_hline(yintercept = 1, colour = "red", alpha = .6) +
  geom_line(aes(y = gr), colour = '#1B9E77', size = 0.75) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = function(x) paste0(round((x - 1) * 100, 1), "%")) + # Transform the rate in percentage increase
  labs(x = NULL, y = 'Growth rate') + 
  theme_light()

graph_epicurves_trends_continent <- hist_epicurve_by_continent + 
  lines_growth_rate_continent +
  plot_layout(ncol = 1)

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('graph_epicurves_trends_continent', '_', week_report, '.png')), 
       plot = graph_epicurves_trends_continent, 
       scale = 0.7, 
       dpi = 720, 
       width = 21, 
       height = 7)

# === === === === ===


## breaks for cumulative incidence %
breaks_ar <- c(0, 0.01, 0.1, 1, 10, Inf)
labels_ar <- c("0 - 0.01%", "0.01 - 0.1%", "0.1 - 1%", "1 - 10%", "10%+")  


tbl_attack_rates <- df_countries %>% 
  left_join(tbl_cases_count %>% select(iso_a3, cases)) %>% 
  left_join(tbl_deaths_count %>% select(iso_a3, deaths)) %>% 
  left_join(df_pop_country %>% select(iso_a3, pop)) %>% 
  mutate(
    cases_ar  = cases  / pop * 100, 
    deaths_ar = deaths / pop * 100,
    cases_brks  = cut(cases_ar, breaks_ar, labels = labels_ar, include.lowest = TRUE, right = FALSE), 
    deaths_brks = cut(deaths_ar, breaks_ar, labels = labels_ar, include.lowest = TRUE, right = FALSE)
  )

sf_attack_rates <- tbl_attack_rates %>% 
  inner_join(
    select(sf_world, iso_a3),
    by = "iso_a3"
  ) %>% 
  st_as_sf()

map_world_cases_attack_rates <- ggplot(sf_attack_rates) + 
  geom_sf(aes(fill = cases_brks), size = .1) + 
  scale_fill_brewer(
    name = NULL, 
    palette = "Blues", 
    drop = FALSE, 
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(70 / length(labels_ar), units = "mm"),
      title.hjust = 0.5,
      nrow = 1,
      label.position = "bottom",
      title.position = 'top')) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

map_world_cases_attack_rates 

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('map_world_cases_attack_rates', '_', week_report, '.png')), 
       plot = map_world_cases_attack_rates, 
       width = 10 * cm_to_in, 
       height = 6.66 * cm_to_in, 
       scale = 1.5, 
       dpi = 720)


## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
## ---- FACET Weekly number of consultation versus admission by continent 
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
load(file.path(path.local.msf.data, glue('episitrep_msf_level_analyses_{week_report}.RData')))



tbl_epicurve_consultation_continent <- dta %>%
  filter(merge_admit != "Unknown", covid_status == "Confirmed") %>% 
  count(continent, epi_week_consultation, merge_admit) %>%
  drop_na() %>%
  mutate(merge_admit = recode(merge_admit, "Yes" = "Admitted", "No" = "Not Admitted"))

tbl_admit_prop_continent <- dta %>%
  filter(merge_admit != "Unknown", covid_status == "Confirmed") %>% 
  drop_na(epi_week_consultation) %>%
  group_by(continent, epi_week_consultation) %>%
  summarise(
    n = n(),
    total = sum(merge_admit %in% c("Yes", "No")),
    admitted = sum(merge_admit == "Yes"),
    admit_prop = admitted / total
  ) %>%
  ungroup()

n_max <- max(tbl_admit_prop_continent$n, na.rm = TRUE)
cfr_max <- rounder(max(tbl_admit_prop_continent$admit_prop, na.rm = TRUE), .1)
cfr_max <- ifelse(cfr_max > 1, 1, cfr_max)
scaling_factor <- cfr_max / n_max # sets the y limit of CFR rounded up to nearest 10% above cfr max

missing_consultation <- sum(is.na(dta$epi_week_consultation))
missing_admission    <- sum(dta$merge_admit == "Unknown")

hist_confirmed_consult_admit_continent <- tbl_epicurve_consultation_continent %>% 
  ggplot(aes(epi_week_consultation, n)) +
  facet_wrap(~continent) +
  geom_col(aes(fill = merge_admit)) +
  geom_line(data = tbl_admit_prop_continent,
            aes(y = admit_prop / scaling_factor, colour = "Admitted %"),
            key_glyph = "timeseries", size = 0.5) +
  ggthemes::scale_fill_tableau(name = "          ", palette = "Tableau 20") +
  scale_x_date(name = "Week of Consultation", date_breaks = "3 week", date_labels = "%V", 
               sec.axis = ggplot2::sec_axis(trans = ~ .), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * scaling_factor, name = "Admitted", labels = scales::percent_format(accuracy = 1)),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(y = "Patients", colour = NULL, caption = glue::glue("Missing data: Consultation Date {missing_consultation}, Admission: {missing_admission}")) + 
  theme_light() + 
  theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


hist_confirmed_consult_admit_continent


ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_confirmed_consult_admit_continent', '_', week_report, '.png')),
       plot = hist_confirmed_consult_admit_continent,
       width = 6, 
       height = 4, 
       scale = 1.1,
       dpi = 720)




