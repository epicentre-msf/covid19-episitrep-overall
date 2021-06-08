

## === === === === === === === === 
## Plots for Scientific day
## === === === === === === === === 

# ---- Setup ----

source(here::here('R', 'setup.r'), encoding = 'UTF-8')

# Upload functions
source(file.path(path.R, 'utils_management.R'), encoding = 'UTF-8')
source(file.path(path.R, 'utils_vis.R')       , encoding = 'UTF-8')

source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")


library(patchwork)

path.sharepoint.scientific.day.2020 <- file.path(path.sharepoint, 'template', 'scientific-day_2020')



## NEw function

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




# ---- World analysis ----

## ---- Upload data ----

load(file.path(path.local.worldwide.data, glue('episitrep_worldwide_analyses_{week_report}.RData')))

levels_continents_ordered <- c("Asia", "Europe", "Americas", "Africa", "Oceania")


## ---- Epidemic curve by continent ----

hist_epi_curve_world <- dta_jhu_right_censored %>% 
  mutate(continent = factor(continent, levels =  levels_continents_ordered)) %>% 
  ggplot(aes(x = date, y = cases, fill = continent)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Number of Cases") + 
  theme_light() + 
  theme(legend.position = "top", legend.title = element_blank())


hist_epi_curve_world


ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_epi_curve_world', '_', week_report, '.png')), 
       plot = graph_epicurve_trends_by_continent, 
       scale = 1, 
       dpi = 720, 
       width = 15, 
       height = 9)




## ---- FACET epidemic curve by continent ----

hist_epicurve_continent <- dta_jhu_right_censored %>% 
  transform(continent = factor(continent, levels = levels_continents_ordered)) %>% 
  ggplot(aes(x = date, y = cases, fill = continent)) + 
  facet_wrap(vars(continent), nrow = 1) + 
  geom_col(width = 1) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") + 
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = NULL, y = "Case count") + 
  theme_light() + 
  theme(legend.position = "none", 
        axis.text.x = element_blank(), 
        strip.text = element_text(size = 11), 
        strip.background = element_rect(fill = "#969696"))

hist_epicurve_continent

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_epicurve_continent', '_', week_report, '.png')), 
       plot = hist_epicurve_continent, 
       scale = 0.3, 
       dpi = 1200, 
       width = 19, 
       height = 6)



## ---- FACET growth rate by continent ----

tbl_growth_rate_continent <- growth_rate_continent %>% 
  bind_rows(.id = "continent") %>% 
  transform(continent = factor(continent, levels = levels_continents_ordered))

lines_growth_rate_continent <- ggplot(tbl_growth_rate_continent, aes(x = date)) + 
  facet_wrap(vars(continent), nrow = 1) + 
  geom_hline(yintercept = 1, colour = "red", alpha = .6) +
  geom_line(aes(y = gr, colour = continent), size = 0.75) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b") + 
  scale_y_continuous(labels = function(x) paste0(round((x - 1) * 100, 1), "%")) + # Transform the rate in percentage increase
  labs(x = NULL, y = 'Growth rate') + 
  theme_light() + 
  theme(legend.position = "none", 
        strip.background = element_blank(),
        strip.text.x = element_blank())

lines_growth_rate_continent

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('lines_growth_rate_continent', '_', week_report, '.png')), 
       plot = lines_growth_rate_continent, 
       scale = 0.3, 
       dpi = 1200, 
       width = 19, 
       height = 5)


graph_epicurves_trends_continent <- hist_epicurve_continent + 
  lines_growth_rate_continent +
  plot_layout(ncol = 1)

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('graph_epicurves_trends_continent', '_', week_report, '.png')), 
       plot = graph_epicurves_trends_continent, 
       scale = 0.3, 
       dpi = 1200, 
       width = 25, 
       height = 9)



## ---- Map cumulative incidence ----

## breaks for cumulative incidence %
breaks_ar <- c(0, 0.01, 0.1, 1, Inf)
labels_ar <- c("0 - 0.01%", "0.01 - 0.1%", "0.1 - 1%", "1%+")  


tbl_attack_rates <- df_countries %>% 
  left_join(tbl_case_count %>% select(iso_a3, cases)) %>% 
  left_join(tbl_death_count %>% select(iso_a3, deaths)) %>% 
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
      keywidth = unit(80 / length(labels_ar), units = "mm"),
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
       scale = 1, 
       dpi = 1200)



## ---- DOTS cumulative incidences versus predictions ----


dta_survey_estimates <- read.csv(file.path(path.sharepoint.scientific.day.2020, 'figure_3_values.csv'), sep = ";")
lst_survey_pop <- readRDS(file.path(path.sharepoint.scientific.day.2020, 'poplist.RDS'))

dta_survey_pop <- tibble(country = character(), 
                  pop = numeric())

for (c in names(lst_survey_pop)) {
  
  dta_survey_pop <- dta_survey_pop %>% 
    add_row(country = c, 
            pop = sum(lst_survey_pop[[c]][,c(2:3)], na.rm = TRUE))
}


tbl_case_count_may2020 <- dta_jhu %>% 
  filter(date <= as.Date("2020-05-30")) %>% 
  group_by(continent, country) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE)) %>% 
  mutate(
    country = case_when(
      country == 'United States' ~ 'United States of America', 
      country == 'Czechia' ~ 'Czech Republic', 
    TRUE ~ country))


tbl_inc_obs_prd <- left_join(dta_survey_estimates, dta_survey_pop) %>% 
  left_join(tbl_case_count_may2020) %>% 
  drop_na() %>% 
  mutate(
    inc_obs = cases / pop * 100, 
    inc_prd = mean * 100) %>% 
  filter(inc_obs > 0.05)


ablines <- tibble(abline_name = c('Ratio 1:1', 'Ratio 1:10'), 
                  simple_intercept = c(0, 0), 
                  simple_slope = c(1, 10))

dots_inc_obs_prd <- ggplot(tbl_inc_obs_prd) + 
  geom_point(aes(inc_obs, inc_prd, col = continent)) + 
  ggrepel::geom_text_repel(aes(inc_obs, inc_prd, label = country, colour = continent), size = 3.5, show.legend = FALSE) + 
  geom_abline(data = ablines, mapping = aes(slope = simple_slope,
                                            intercept = simple_intercept, 
                                            linetype = as.factor(abline_name)),
              size = 0.5) + 
  scale_x_continuous(limits = c(0, 1), n.breaks = 5) + 
  scale_y_continuous(limits = c(0, 24), n.breaks = 12) + 
  xlab("Cumulative incidence (%) of confirmed cases until 30 May 2020") + 
  ylab("Predicted proportion (%) of population infected (O'Driscoll et al)") + 
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12))

dots_inc_obs_prd

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('dots_inc_obs_prd', '_', week_report, '.png')), 
       plot = dots_inc_obs_prd, 
       width = 16 * cm_to_in, 
       height = 16 * cm_to_in, 
       scale = 1, 
       dpi = 1200)



# ---- MSF data analysis ----

## ---- Upload data ----

load(file.path(path.local.msf.data, glue('episitrep_msf_level_analyses_{week_report}.RData')))



## ---- FACET - MSF consultation versus admission by continent ----


tbl_epicurve_consultation_continent <- dta_linelist %>%
  filter(merge_admit != "Unknown", covid_status == "Confirmed") %>% 
  count(continent, epi_week_consultation, merge_admit) %>%
  drop_na() %>%
  mutate(merge_admit = recode(merge_admit, "Yes" = "Admitted", "No" = "Not Admitted"))

tbl_admit_prop_continent <- dta_linelist %>%
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

missing_consultation <- sum(is.na(dta_linelist$epi_week_consultation))
missing_admission    <- sum(dta_linelist$merge_admit == "Unknown")

hist_confirmed_consult_admit_continent <- tbl_epicurve_consultation_continent %>% 
  ggplot(aes(epi_week_consultation, n)) +
  facet_wrap(~continent) +
  geom_col(aes(fill = merge_admit)) +
  geom_line(data = tbl_admit_prop_continent,
            aes(y = admit_prop / scaling_factor, colour = "Admitted %"),
            key_glyph = "timeseries", size = 0.5) +
  ggthemes::scale_fill_tableau(name = "          ", palette = "Tableau 20") +
  scale_x_date(name = "Week of Consultation", date_breaks = "3 months", date_labels = "%b") +
  scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * scaling_factor, name = "Admitted", labels = scales::percent_format(accuracy = 1)),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(y = "Patients", colour = NULL, caption = glue::glue("Missing data: Consultation Date {missing_consultation}, Admission: {missing_admission}")) + 
  theme_light() + 
  theme(legend.position = "top", 
        legend.text = element_text(size = 11), 
        strip.text = element_text(size = 11), 
        strip.background = element_rect(fill = "#969696"))


hist_confirmed_consult_admit_continent


ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_confirmed_consult_admit_continent', '_', week_report, '.png')),
       plot = hist_confirmed_consult_admit_continent,
       width = 10, 
       height = 7, 
       scale = 0.5,
       dpi = 1200)




## ---- FACET - MSF covid status ----


tbl_epicurve_status_continent <- dta_linelist_with_aggregated %>% 
  transform(covid_status = droplevels(covid_status)) %>% 
  transform(covid_status = factor(covid_status, levels = c('Confirmed', 'Probable', 'Suspected', 'Not a case', 'Unknown'), labels = c('Confirmed', 'Probable', 'Suspected', 'CoViD-19 negative', 'Unknown'))) %>% 
  count(continent, covid_status, epi_week_consultation)


hist_epicurve_status_continent <- tbl_epicurve_status_continent %>% 
  ggplot(aes(epi_week_consultation, n, fill = covid_status)) +
  facet_wrap(vars(continent), scales = 'free_y') +
  geom_col() +
  ggthemes::scale_fill_tableau(name = NULL, palette = "Tableau 20") +
  scale_x_date(name = 'Week of Consultation', date_breaks = '2 months', date_labels = '%b') +
  scale_y_continuous(name = "Patients", expand = expansion(mult = c(0, 0.02))) +
  labs(y = 'Patients', caption = 'NOTE: free y axis scale among graphics') + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  theme_light() +
  theme(legend.position = 'top', 
        legend.direction = 'vertical', 
        legend.text = element_text(size = 11), 
        strip.text = element_text(size = 11), 
        strip.background = element_rect(fill = "#969696"))

hist_epicurve_status_continent

ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('hist_epicurve_status_continent', '_', week_report, '.png')),
       plot = hist_epicurve_status_continent, 
       width = 16, 
       height = 9, 
       scale = 0.5,
       dpi = 1200)





## ---- CFR by age-group ----

age_cut <- c(seq(0, 80, 10), Inf)
age_labs <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

df_severity <- dta_linelist %>% 
  mutate(age_group = cut(age_in_years, breaks = age_cut, labels = age_labs, right = FALSE, include.lowest = TRUE)) %>% 
  filter(!is.na(age_group), covid_status == "Confirmed") %>% 
  group_by(age_group) %>% 
  summarize(
    n = n(),
    n_hosp = sum(admit == "Yes", na.rm = TRUE),
    n_hosp_known = sum(admit %in% c("Yes", "No"), na.rm = TRUE),
    p_hosp = n_hosp / n_hosp_known,
    p_hosp_low95 = as.numeric(binom.test(n_hosp, n_hosp_known)$conf.int)[1],
    p_hosp_upp95 = as.numeric(binom.test(n_hosp, n_hosp_known)$conf.int)[2],
    n_died = sum(outcome_status == "Died", na.rm = TRUE),
    n_died_known = sum(outcome_status %in% c("Died", "Cured"), na.rm = TRUE),
    p_died_low95 = as.numeric(binom.test(n_died, n_died_known)$conf.int)[1],
    p_died_upp95 = as.numeric(binom.test(n_died, n_died_known)$conf.int)[2],
    p_died = n_died / n_died_known,
    # hosp_lab = paste0("(", n_hosp_known, ")"),
    # died_lab = paste0("(", n_died_known, ")"),
    hosp_lab = paste0("(", n_hosp, "/", n_hosp_known, ")"),
    died_lab = paste0("(", n_died, "/", n_died_known, ")")
  ) %>% 
  ungroup()


dots_cfr_agegroup <- ggplot(df_severity, aes(x = age_group)) +
  geom_point(aes(y = p_died), size = 2) +
  geom_linerange(aes(ymin = p_died_low95, ymax = p_died_upp95)) +
  geom_text(aes(label = died_lab, y = p_died_upp95 + 0.02), size = 3.5) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1), labels = scales::percent_format(accuracy = 1), sec.axis = dup_axis(name = NULL)) +  
  labs(x = "Age Group", y = "CFR", caption = "CFR = deaths / known outcomes (cured + died)\nLine range shows binomial 95% confidence intervals") 


dots_cfr_agegroup


ggsave(file.path(path.sharepoint.scientific.day.2020, paste0('dots_cfr_agegroup', '_', week_report, '.png')),
       plot = dots_cfr_agegroup,
       width = 11, 
       height = 7, 
       scale = 0.5,
       dpi = 1200)







