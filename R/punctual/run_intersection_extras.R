
# Setup -----------------------------------------------

if (!exists('path.root')) {
  source(here::here('R', 'setup.r'), encoding = 'UTF-8')
}

if (Sys.getlocale(category = "LC_TIME") == "French_France.1252") {
  Sys.setlocale(category = "LC_TIME", locale = "C")
  Sys.setenv(LANG = "en_GB.UTF-8") 
}

# Upload functions
source(file.path(path.R, 'utils_management.R'), encoding = 'UTF-8')
source(file.path(path.R, 'utils_get_data.R')  , encoding = 'UTF-8')
source(file.path(path.R, 'utils_vis.R')       , encoding = 'UTF-8')


# Set the left and right censoring for date of consultation.
# The right-censoring create also the week value and the folders where to save outputs
dates_and_week <- set_date_frame(create_folders = TRUE)

date_min_report <- dates_and_week[[1]]
date_min_report <- as.Date("2020-01-22")
date_max_report <- dates_and_week[[2]]
week_report     <- dates_and_week[[3]]



### SETUP PATHS ###
# If week folder not created already by overall sitrep, create it.
path.local.msf.extra <- file.path(path.local.msf, "extra")

if (!dir.exists(path.local.msf.extra)) {
  dir.create(path.local.msf.extra,   showWarnings = FALSE, recursive = TRUE)
}

# Get data --------------------------------------------

# === === === === === === ===
# Geo and population data
# === === === === === === ===

df_countries <- readRDS(file.path(path.local.data, 'df_countries.RDS'))


# === === === === === === ===
# Linelist data
# === === === === === === ===


##### Data #####
load(file.path(path.local.msf.data, 
               glue('episitrep_msf_level_analyses_{week_report}.RData')))


# dta_linelist <- covidutils::get_msf_linelist(force = TRUE) %>%
#   covidutils::prepare_msf_dta(shorten_var_names = FALSE) %>%
#   rename(iso_a3 = country) %>%
#   left_join(df_countries %>% select(continent, region, iso_a3, country), by = 'iso_a3') %>%
#   mutate(
#     country   = as.factor(country),
#     continent = as.factor(continent),
#     delay_before_consultation = as.numeric(MSF_date_consultation - patcourse_dateonset)) %>%
#   filter(between(MSF_date_consultation,
#                  left = date_min_report, right = date_max_report) | is.na(MSF_date_consultation)) %>%  # Filter date limits using date of consultation
#   filter(across(
#     .cols = ends_with("OCP"),
#     .fns  = ~ .x == TRUE))


# Data for per project analysis: only the confirmed, probable and suspects
# dta_linelist_project <- dta_linelist %>%
#   filter(ind_MSF_covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
#   mutate(MSF_severity = ifelse(is.na(MSF_severity), "Non available", MSF_severity),
#          MSF_severity = factor(MSF_severity,
#                                levels = c("Mild", "Moderate", "Severe", "Critical",
#                                           "Non available")),
#          country_project = paste(iso_a3, project, sep = "_") )



# === === === === === === ===
# Aggregated data
# === === === === === === ===

# dta_weekly_aggregated        <- get_msf_aggregated(force = TRUE)
# dta_project_aggregated_dates <- get_msf_aggregated_dates(force = TRUE)

### Remove temporarily Quito that grouped the whole activity in a single week
# dta_weekly_aggregated <- dta_weekly_aggregated %>%
#   filter(project == 'Quito-Fixed & mobile brigades')


### Expand the aggregated
# dta_aggregated_expanded <- dta_weekly_aggregated %>%
#   filter(!project %in% c('Lesvos-Moria', 'Lesvos-Moria Ped', "Quito")) %>%
#   select(-week) %>%
#   mutate(
#     date = as.Date(date),
#     epi_week_consultation = make_epiweek_date(date) %>% as.Date(),
#     country = case_when(
#       country == 'DRC' ~ 'Democratic Republic of the Congo',
#       country == "Côte d'Ivoire" ~ 'Côte d’Ivoire',
#       TRUE ~ country)) %>%
#   left_join(df_countries, by = 'country') %>%
#   pivot_longer(cols = c('confirmed', 'probable', 'suspected', 'non_cases', 'unknown'),
#                names_to = 'ind_MSF_covid_status') %>%
#   filter(!is.na(value)) %>%
#   mutate(obs = purrr::map(value, ~rep_len(1, .x))) %>%
#   unnest(cols = c(obs)) %>%
#   select(-c(value, obs)) %>%
#   mutate(
#     ind_MSF_covid_status = factor(ind_MSF_covid_status,
#                                   levels = c('confirmed', 'probable', 'suspected',
#                                              'non_cases', 'unknown'),
#                                   labels = c('Confirmed', 'Probable', 'Suspected',
#                                              'Not a case', 'Unknown'))) %>%
#   rename('OC' = oc,
#          'site_name' = project) %>%
#   filter(between(date, left = date_min_report, right = date_max_report) | is.na(date)) %>%
#   filter(OC == "OCP")


# === === === === === === ===
# Merge datasets
# === === === === === === ===

# The dataset with linelist and the expanded aggregated data together
# dta_linelist_with_aggregated <- dta_linelist %>%
#   select(continent, region, country, site_name, iso_a3, OC,
#          epi_week_consultation, ind_MSF_covid_status, project) %>%
#   add_row(dta_aggregated_expanded %>%
#             select(continent, region, country, site_name, iso_a3,
#                    OC, epi_week_consultation, ind_MSF_covid_status) %>%
#             mutate(
#               country = paste(country, '(*)'),
#               site_name = paste(site_name, '(*)'))) %>%
#   mutate(
#     project = case_when(
#       is.na(project) ~ site_name,
#       TRUE           ~ project),
#     ind_MSF_covid_status = recode(ind_MSF_covid_status, "(Unknown)" = "Unknown")
#   ) %>%
#   mutate(continent = case_when(
#     country == "Cote d'Ivoire (*)" ~ 'Africa',
#     country == "Democratic Republic of Congo (*)" ~ 'Africa',
#     country == "Palestine (*)" ~ "Asia",
#     TRUE ~ continent))


# tbl_aggregated_expanded_with_dates <- dta_aggregated_expanded %>%
#   distinct(sheet, OC, country, site_name) %>%
#   full_join(dta_project_aggregated_dates, by = 'sheet') %>%
#   mutate(
#     country = case_when(
#       country == 'DRC' ~ 'Democratic Republic of the Congo',
#       country == "Côte d'Ivoire" ~ 'Côte d’Ivoire',
#       TRUE ~ country)) %>%
#   pivot_longer(
#     cols = date_first:date_last,
#     names_to = "type_date",
#     values_to = "MSF_date_consultation") %>%
#   left_join(df_countries, by = 'country') %>%
#   mutate(
#     country = paste(country, '(*)'),
#     site_name = paste(site_name, '(*)'))



# Patients under OXYGEN ---------------------------------------------------

DATA <- dta_linelist %>% 
  filter(merge_oxygen == "Yes")


# path.local.msf.extra.graphs <- file.path(path.local.msf.extra, "graphs")
# 
# if (!dir.exists(path.local.msf.extra)) {
#   dir.create(path.local.msf.extra,   showWarnings = FALSE, recursive = TRUE)
# }


##### Pyramide des ages #####

missing_age_sex <- DATA %>%
  summarise(age = sum(is.na(age_in_years)), 
            patinfo_sex = sum(is.na(patinfo_sex)))

tbl_pyramid_continent <- DATA %>%
  drop_na(patinfo_sex, age_in_years) %>%
  count(patinfo_sex, age_9gp) %>%
  mutate(n = if_else(patinfo_sex == "M", -n, n))

pyramid_age_sex_all <- ggplot(tbl_pyramid_continent, 
                              aes(x = age_9gp, 
                                  y = n, 
                                  fill = patinfo_sex)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_text(aes(label = abs(n), hjust = if_else(n >= 0, 1.1, -0.1)), colour = "white", size = 3.5) +
  coord_flip() +
  scale_fill_manual(name = NULL, 
                    values = c("#4E79A7FF", "#A0CBE8FF"), 
                    breaks = c("M", "F"), 
                    labels = c("Males", "Females")) +
  scale_y_continuous(label = abs, limits = pyramid_limits) +
  theme(legend.position = "top") +
  labs(title = "Patients who received oxygen",
       x = "Age Group", 
       y = "Patients consulted/admitted", 
       caption = glue::glue("Missing Data: Age {missing_age_sex$age}, Sex {missing_age_sex$patinfo_sex}"))

ggsave(file.path(path.local.msf.extra, paste0('pyramid_age_sex_all_continent', '_', week_report, '.png')),
       plot = pyramid_age_sex_all,
       width = 8,
       height = 6)




# Sur le dernier mois
missing_age_sex_last_month <- DATA %>%
  filter(MSF_date_consultation >= lubridate::today() - 30,
         MSF_date_consultation <= lubridate::today()) %>% 
  summarise(age = sum(is.na(age_in_years)), 
            patinfo_sex = sum(is.na(patinfo_sex)))


tbl_pyramid_continent_last_month <- DATA %>%
  filter(MSF_date_consultation >= lubridate::today() - 30,
         MSF_date_consultation <= lubridate::today()) %>% 
  drop_na(patinfo_sex, age_in_years) %>%
  count(patinfo_sex, age_9gp) %>%
  mutate(n = if_else(patinfo_sex == "M", -n, n))

pyramid_age_sex_all_last_month <- ggplot(tbl_pyramid_continent_last_month, 
                                         aes(x = age_9gp, 
                                             y = n, 
                                             fill = patinfo_sex)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_text(aes(label = abs(n), hjust = if_else(n >= 0, 1.1, -0.1)), colour = "white", size = 3.5) +
  coord_flip() +
  scale_fill_manual(name = NULL, 
                    values = c("#4E79A7FF", "#A0CBE8FF"), 
                    breaks = c("M", "F"), 
                    labels = c("Males", "Females")) +
  scale_y_continuous(label = abs, limits = pyramid_limits) +
  theme(legend.position = "top") +
  labs(title = "Patients who received oxygen",
       x = "Age Group", 
       y = "Patients consulted/admitted", 
       caption = glue::glue("Missing Data: Age {missing_age_sex$age}, Sex {missing_age_sex$patinfo_sex}"))

ggsave(file.path(path.local.msf.extra, paste0('pyramid_age_sex_all_continent_last_month', '_', week_report, '.png')),
       plot = pyramid_age_sex_all_last_month,
       width = 8,
       height = 6)




##### CFR by age group (dots) #####

age_cut <- c(seq(0, 80, 10), Inf)
age_labs <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

df_MSF_severity <- DATA %>%
  mutate(age_group = cut(age_in_years, 
                         breaks = age_cut,
                         labels = age_labs, 
                         right = FALSE, 
                         include.lowest = TRUE)) %>%
  filter(!is.na(age_group), ind_MSF_covid_status == "Confirmed") %>%
  group_by(age_group) %>%
  summarize(
    n = n(),
    n_hosp = sum(merge_admit == "Yes", na.rm = TRUE),
    n_hosp_known = sum(merge_admit %in% c("Yes", "No"), na.rm = TRUE),
    
    p_hosp = n_hosp / n_hosp_known,
    p_hosp_low95 = as.numeric(binom.test(n_hosp, n_hosp_known)$conf.int)[1],
    p_hosp_upp95 = as.numeric(binom.test(n_hosp, n_hosp_known)$conf.int)[2],
    
    n_died = sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
    n_died_known = sum(ind_outcome_patcourse_status %in% c("Died", "Cured"), na.rm = TRUE),
    
    p_died_low95 = ifelse(n_died_known == 0, 
                          NA,
                          as.numeric(binom.test(n_died, n_died_known)$conf.int)[1]),
    p_died_upp95 = ifelse(n_died_known == 0, 
                          NA,
                          as.numeric(binom.test(n_died, n_died_known)$conf.int)[2]),
    p_died = n_died / n_died_known,
    # hosp_lab = paste0("(", n_hosp_known, ")"),
    # died_lab = paste0("(", n_died_known, ")"),
    hosp_lab = paste0("(", n_hosp, "/", n_hosp_known, ")"),
    died_lab = paste0("(", n_died, "/", n_died_known, ")")
  ) %>%
  ungroup()


dots_cfr_agegroup <- ggplot(df_MSF_severity, 
                            aes(x = age_group)) +
  geom_point(aes(y = p_died), size = 2) +
  geom_linerange(aes(ymin = p_died_low95, 
                     ymax = p_died_upp95)) +
  geom_text(aes(label = died_lab, y = p_died_upp95 + 0.06), size = 4.2) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1.06), 
                     labels = scales::percent_format(accuracy = 1), 
                     sec.axis = dup_axis(name = NULL)) +
  labs(title = "Patients who received oxygen",
       x = "Age Group", 
       y = "CFR", 
       caption = "CFR = deaths / known outcomes (cured + died)\nLine range shows binomial 95% confidence intervals")

ggsave(file.path(path.local.msf.extra, paste0('dots_cfr_agegroup', '_', week_report, '.png')),
       plot = dots_cfr_agegroup,
       scale = 1.1,
       dpi = 320)


dots_cfr_agegroup



##### CFR by status and sex #####

dta_cfr_status <- DATA %>%
  select(continent, ind_MSF_covid_status, ind_outcome_patcourse_status, patinfo_sex) %>%
  filter(ind_outcome_patcourse_status %in% c('Cured', 'Died'))


# Hommes
tbl_cfr_status_continent_M <- levels(dta_cfr_status$ind_MSF_covid_status) %>%
  purrr::map_df(~{
    dta_cfr_status %>%
      filter(ind_MSF_covid_status == .x) %>%
      filter(patinfo_sex == "M") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        ind_MSF_covid_status = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_status_continent_M <- tbl_cfr_status_continent_M %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(ind_MSF_covid_status)) %>%
  cols_label(
    ind_MSF_covid_status = 'Covid status',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(ind_MSF_covid_status)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_status_continent_M,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_status_continent_M','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_status_continent_M,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_status_continent_M','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Femmes
tbl_cfr_status_continent_F <- levels(dta_cfr_status$ind_MSF_covid_status) %>%
  purrr::map_df(~{
    dta_cfr_status %>%
      filter(ind_MSF_covid_status == .x) %>%
      filter(patinfo_sex == "F") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        ind_MSF_covid_status = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_status_continent_F <- tbl_cfr_status_continent_F %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(ind_MSF_covid_status)) %>%
  cols_label(
    ind_MSF_covid_status = 'Covid status',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(ind_MSF_covid_status)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_status_continent_F,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_status_continent_F','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_status_continent_F,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_status_continent_F','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



##### CFR by severity and sex #####
dta_cfr_severity <- DATA %>%
  select(continent, MSF_severity, ind_outcome_patcourse_status, patinfo_sex) %>%
  filter(ind_outcome_patcourse_status %in% c('Cured', 'Died')) %>% 
  mutate(MSF_severity = as.factor(MSF_severity))

# Hommes
tbl_cfr_severity_continent_M <- levels(dta_cfr_severity$MSF_severity) %>% 
  purrr::map_df(~{
    dta_cfr_severity %>%
      filter(MSF_severity == .x) %>%
      filter(patinfo_sex == "M") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        MSF_severity = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_severity_continent_M <- tbl_cfr_severity_continent_M %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(MSF_severity)) %>%
  cols_label(
    MSF_severity = 'Severity',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(MSF_severity)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_severity_continent_M,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_M','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_M,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_M','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Femmes
tbl_cfr_severity_continent_F <- levels(dta_cfr_severity$MSF_severity) %>%
  purrr::map_df(~{
    dta_cfr_severity %>%
      filter(MSF_severity == .x) %>%
      filter(patinfo_sex == "F") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        MSF_severity = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_severity_continent_F <- tbl_cfr_severity_continent_F %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(MSF_severity)) %>%
  cols_label(
    MSF_severity = 'Severity',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(MSF_severity)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_severity_continent_F,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_F','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_F,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_F','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Sur le dernier mois
dta_cfr_severity_last_month <- DATA %>%
  filter(MSF_date_consultation >= lubridate::today() - 30,
         MSF_date_consultation <= lubridate::today() ) %>% 
  select(continent, MSF_severity, ind_outcome_patcourse_status, patinfo_sex) %>%
  filter(ind_outcome_patcourse_status %in% c('Cured', 'Died')) %>% 
  mutate(MSF_severity = as.factor(MSF_severity))

# Hommes
tbl_cfr_severity_continent_M_last_month <- levels(dta_cfr_severity$MSF_severity) %>% 
  purrr::map_df(~{
    dta_cfr_severity_last_month %>%
      filter(MSF_severity == .x) %>%
      filter(patinfo_sex == "M") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        MSF_severity = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_severity_continent_M_last_month <- tbl_cfr_severity_continent_M_last_month %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(MSF_severity)) %>%
  cols_label(
    MSF_severity = 'Severity',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(MSF_severity)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_severity_continent_M_last_month,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_M_last_month','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_M_last_month,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_M_last_month','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Femmes
tbl_cfr_severity_continent_F_last_month <- levels(dta_cfr_severity$MSF_severity) %>%
  purrr::map_df(~{
    dta_cfr_severity_last_month %>%
      filter(MSF_severity == .x) %>%
      filter(patinfo_sex == "F") %>% 
      tbl_prop('ind_outcome_patcourse_status', 'continent', drop_levels = FALSE) %>%
      mutate(
        MSF_severity = .x)
  }) %>%
  filter(ind_outcome_patcourse_status == 'Died')


gtbl_cfr_severity_continent_F_last_month <- tbl_cfr_severity_continent_F_last_month %>%
  gt() %>%
  cols_hide('ind_outcome_patcourse_status') %>%
  cols_move_to_start(columns = vars(MSF_severity)) %>%
  cols_label(
    MSF_severity = 'Severity',
    n_Africa   = 'n',
    n_Americas = 'n',
    n_Asia     = 'n',
    n_Europe   = 'n',
    n_Total    = 'n',
    N_Africa   = 'N',
    N_Americas = 'N',
    N_Asia     = 'N',
    N_Europe   = 'N',
    N_Total    = 'N',
    p_Africa   = '(%)',
    p_Americas = '(%)',
    p_Asia     = '(%)',
    p_Europe   = '(%)',
    p_Total    = '(%)') %>%
  tab_spanner(
    label = 'Africa',
    columns = ends_with('_Africa')) %>%
  tab_spanner(
    label = 'Americas',
    columns = ends_with('_Americas')) %>%
  tab_spanner(
    label = 'Asia',
    columns = ends_with('_Asia')) %>%
  tab_spanner(
    label = 'Europe',
    columns = ends_with('_Europe')) %>%
  tab_spanner(
    label = 'Total',
    columns = ends_with('_Total')) %>%
  fmt_number(
    columns = starts_with('p_'),
    decimals = 1,
    scale_by = 100,
    pattern = '({x})') %>%
  fmt_missing(
    columns = starts_with('n_'),
    missing_text = "0") %>%
  fmt_missing(
    columns = starts_with('p_'),
    missing_text = "---") %>%
  cols_align(
    align = 'left',
    columns = vars(MSF_severity)) %>%
  cols_align(
    align = 'right',
    columns = starts_with('n_')) %>%
  tab_options(
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    row_group.padding = px(2))

gtsave(gtbl_cfr_severity_continent_F_last_month,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_F_last_month','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_F_last_month,
       file.path(path.local.msf.extra, paste0('gtbl_cfr_severity_continent_F_last_month','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()





# Extra tables ----------------------------------------

DATA_cured_died <- DATA %>% 
  filter(merge_oxygen == "Yes",
         ind_outcome_patcourse_status %in% c('Cured', 'Died')) %>% 
  select(continent, MSF_severity, ind_outcome_patcourse_status, patinfo_sex) 


dta_linelist %>% 
  group_by(MSF_severity, merge_oxygen) %>% 
  summarise(n_visits_hospi = n(),
            n_dead =  sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            n_cured = sum(ind_outcome_patcourse_status == "Cured", na.rm = TRUE),
            p_dead_sur_curedNdead = round(n_dead / (n_dead + n_cured), 2),
            p_dead_sur_tot = round(n_dead / n_visits_hospi, 2))


DATA_cured_died %>% 
  group_by(continent, patinfo_sex, MSF_severity) %>% 
  summarise(n_tot = n(),
            n_dead = sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            prop = round((n_dead / n_tot), 2),
            conca = paste(n_dead, "/", n_tot, " (", prop*100, "%)", sep ="")) %>% 
  select(continent, patinfo_sex, MSF_severity, conca) %>% 
  pivot_wider(names_from = patinfo_sex,
              values_from = conca) %>% 
  arrange(continent, MSF_severity)



DATA_cured_died %>% 
  group_by(continent, patinfo_sex) %>% 
  summarise(n_tot = n(),
            n_dead = sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            prop = round((n_dead / n_tot), 2),
            conca = paste(n_dead, "/", n_tot, " (", prop*100, "%)", sep ="")) %>% 
  select(continent, patinfo_sex, conca) %>% 
  pivot_wider(names_from = patinfo_sex,
              values_from = conca) %>% 
  arrange(continent )









