# Mathilde Mousset
# December 2020
# 
# What is that undocumented horror? Gather some fragments of scripts used to generate figures 
# and numbers for OCP report. The most interesting ones should have gone into the
# OC sitrep. This is nt supposed to be run again so I don't want to lose more time 
# cleaning it. My pessimistic sense however tells me that we probably want to keep it
# _just in case_...

# To get data and everything loaded, run the setup and import chunks from the OC sitrep.

rmarkdown::render(
  input       = file.path(path.Rmd, 'episitrep_msf_oc_level_analyses.Rmd'),
  output_file = glue::glue("{week_report}_episitrep_msf_oc_level_analysis_OCP.html"),
  output_dir  = path.local.week.oc,
  params = list(OC = "OCP"))


rmarkdown::render(
  input       = file.path(path.Rmd, 'test.Rmd'),
  # output_file = glue::glue("{week_report}_episitrep_msf_oc_level_analysis_OCP.html"),
  # output_dir  = path.local.week.oc,
  params = list(OC = "OCP"))


# IQR age admis +/- morts GLOBAL
dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  filter(merge_admit == "Yes") %>% 
  filter(outcome_status == "Died") %>% 
  summarise(quantile = c(0.25, 0.5, 0.75),
            age_y = quantile(age_in_years, 
                           c(0.25, 0.5, 0.75), na.rm = TRUE))

# IQR length od stay admis GLOBAL
dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  filter(merge_admit == "Yes") %>% 
  summarise(quantile = c(0.25, 0.5, 0.75),
            length_stay = quantile(length_stay, 
                                   c(0.25, 0.5, 0.75), na.rm = TRUE))


# IQR length od stay admis par projet
dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  filter(merge_admit == "Yes") %>% 
  group_by(country, project) %>% 
  summarise(N = n(),
            quantile = c(0.25, 0.5, 0.75),
            length_stay = quantile(length_stay, 
                                   c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  pivot_wider(names_from = quantile,
              values_from = length_stay)


# Délai temps admission -> mort (admis, morts, par projet)
dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected'),
         merge_admit == "Yes",
         outcome_status == "Died") %>%
  mutate(delay_admission_death = (outcome_date_of_outcome - date_consultation)) %>% 
  drop_na(delay_admission_death) %>% 
  group_by(country, project) %>% 
  summarise(N = n(),
            quantile = c(0.25, 0.5, 0.75),
            delay_admission_death = quantile(delay_admission_death, 
                             c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  pivot_wider(names_from = quantile,
              values_from = delay_admission_death)



dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  select(covid_status, merge_admit, severity, age_in_years, sex, outcome_status) %>% 
  filter(merge_admit == "Yes" & outcome_status == "Died") %>% 
  mutate(total = n()) %>%
  group_by(severity, total) %>% 
  summarise(n_admission_death = n(),
            
            median_age_death = median(age_in_years, na.rm = TRUE),
            age_low_death = quantile(age_in_years, 0.25, na.rm = TRUE),
            age_high_death = quantile(age_in_years, 0.75, na.rm = TRUE),
            
            n_males_death = sum(sex == "M", na.rm = TRUE),
            n_females_death = sum(sex == "F", na.rm = TRUE),
            n_sex_missing_death = sum(is.na(sex), na.rm = TRUE)
  ) %>% 
  mutate(p_admissio_death = n_admission_death / total) %>% 
  select(-total) %>% relocate(p_admissio_death, .before = median_age_death)





library(patchwork)

p1 <- dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  mutate(severity = factor(severity, levels = c("Mild", "Moderate", "Severe", "Critical"))) %>% 
  filter(merge_admit == "Yes") %>% 
  ggplot(aes(x = severity,
             fill = outcome_status)) +
  geom_bar(
    position = "fill"
  ) +
  labs(y = "Percentage of each outcome",
       title = "Outcome by severity") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggthemes::scale_fill_tableau(name = "Outcome", 
                               palette = "Tableau 20")
# theme(legend.position ="bottom")


p1


p2 <- dta_linelist %>%
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  filter(merge_admit == "Yes") %>% 
  mutate(severity = factor(severity, 
                           levels = c("Mild", "Moderate", "Severe", "Critical"),
                           labels = c("Mi", "Mo", "S", "C"))) %>% 
  ggplot(aes(x = severity)) +
  geom_bar() +
  labs(y = "", 
       x = "") +
  theme(axis.text = element_text(size = 7)) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        plot.background = element_blank())

p2


p1 + inset_element(p2, left = 0.75, bottom = 0.66, right = 1, top = 1, align_to = "full")

ggsave(file.path(path.local.msf.graphs.oc, paste0('fig9', '_', week_report, '.png')),
       # plot = fig8, 
       width = 10, 
       height = 6, 
       scale = 1.1,
       dpi = 320)




dta_linelist %>%
  filter(country %in% c("Afghanistan", "Bangladesh", "Haiti", "Iraq", "Yemen")) %>% 
  filter(covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>%
  mutate(outcome_death = ifelse(outcome_status == "Died", "Death", "Other")) %>%
  group_by(country, outcome_death, epi_week_admission) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = epi_week_admission,
             y = n,
             fill = outcome_death)) +
  geom_col() +
  facet_wrap(~country)




dta_linelist %>%
  filter(country %in% c("Afghanistan", "Bangladesh", "Haiti", "Iraq", "Yemen"),
         covid_status %in% c('Confirmed', 'Probable', 'Suspected')) %>% 
  mutate(severity = factor(severity, 
                           levels = c("Mild", "Moderate", "Severe", "Critical"))) %>% 
  filter(outcome_status == "Died") %>%
  filter(merge_admit == "Yes") %>% 
  group_by(country, epi_week_admission, severity) %>% 
  summarise(n = n()) %>% 
  drop_na(severity, epi_week_admission) %>% 
  
  ggplot(aes(x = epi_week_admission,
             y = n,
             colour = severity)) +
  geom_line() +
  geom_point(alpha = 0.8,
             size = 2) +
  # facet_wrap(~country) +
  labs(x = "Week of admission",
       y = "Nb of death",
       title = "Mortality by severity and country for hospitalized patients") +
  scale_x_date(name = "Week of Admission", 
               date_breaks = "2 weeks", 
               date_labels = "%V", 
               sec.axis = ggplot2::sec_axis(trans = ~ .), 
               expand = expansion(mult = c(0.01, 0.01))) +
  
  ggthemes::scale_colour_tableau(name = "Severity", 
                                 palette = "Tableau 20")


ggsave(file.path(path.local.msf.graphs.oc, paste0('fig10', '_', week_report, '.png')),
       # plot = fig8, 
       width = 14, 
       height = 6, 
       scale = 1.1,
       dpi = 320)



# Présentation 28 janvier 2020 --------------------------------------------

DATA <- dta_linelist %>% 
  filter(OC == "OCP",
         merge_oxygen == "Yes")


path.local.msf.graphs.oc.extras <- file.path(path.local.week.oc, "OCP", 'extras')

if (!dir.exists(path.local.msf.graphs.oc.extras)) {
  dir.create(path.local.msf.graphs.oc.extras,   showWarnings = FALSE, recursive = TRUE)
}




# Pyramide ages patients

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
  labs(title = "OCP patients who received oxygen",
       x = "Age Group", 
       y = "Patients consulted/admitted", 
       caption = glue::glue("Missing Data: Age {missing_age_sex$age}, Sex {missing_age_sex$patinfo_sex}"))

ggsave(file.path(path.local.msf.graphs.oc.extras, paste0('pyramid_age_sex_all_continent', '_', week_report, '.png')),
       plot = pyramid_age_sex_all,
       width = 8,
       height = 6)





# Dots CFR
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
  labs(title = "OCP patients who received oxygen",
       x = "Age Group", 
       y = "CFR", 
       caption = "CFR = deaths / known outcomes (cured + died)\nLine range shows binomial 95% confidence intervals")

ggsave(file.path(path.local.msf.graphs.oc.extras, paste0('dots_cfr_agegroup', '_', week_report, '.png')),
       plot = dots_cfr_agegroup,
       scale = 1.1,
       dpi = 320)


dots_cfr_agegroup



# Table
dta_cfr_status <- DATA %>%
  select(continent, ind_MSF_covid_status, ind_outcome_patcourse_status, patinfo_sex) %>%
  filter(ind_outcome_patcourse_status %in% c('Cured', 'Died'))

# Hommes
tbl_cfr_status_continent_M <- levels(dta_cfr_status$ind_MSF_covid_status) %>%
  map_df(~{
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
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_status_continent_M','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_status_continent_M,
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_status_continent_M','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Femmes
tbl_cfr_status_continent_F <- levels(dta_cfr_status$ind_MSF_covid_status) %>%
  map_df(~{
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
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_status_continent_F','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_status_continent_F,
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_status_continent_F','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()


# Table
dta_cfr_severity <- DATA %>%
  select(continent, MSF_severity, ind_outcome_patcourse_status, patinfo_sex) %>%
  filter(ind_outcome_patcourse_status %in% c('Cured', 'Died')) %>% 
  mutate(MSF_severity = as.factor(MSF_severity))

# Hommes
tbl_cfr_severity_continent_M <- levels(dta_cfr_severity$MSF_severity) %>% 
  map_df(~{
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
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_severity_continent_M','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_M,
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_severity_continent_M','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



# Femmes
tbl_cfr_severity_continent_F <- levels(dta_cfr_severity$MSF_severity) %>%
  map_df(~{
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
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_severity_continent_F','_', week_report, '.png'))) %>%
  invisible()

gtsave(gtbl_cfr_severity_continent_F,
       file.path(path.local.msf.graphs.oc.extras, paste0('gtbl_cfr_severity_continent_F','_', week_report, '.html')),
       inline_css = TRUE) %>%
  invisible()



DATA_cured_died <- dta_linelist %>% 
  filter(OC == "OCP",
         merge_oxygen == "Yes",
         ind_outcome_patcourse_status %in% c('Cured', 'Died')) %>% 
  select(continent, MSF_severity, ind_outcome_patcourse_status, patinfo_sex) 


dta_linelist %>% 
  group_by(MSF_severity, merge_oxygen) %>% 
  summarise(n_visits_hospi = n(),
            n_dead =  sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            n_cured = sum(ind_outcome_patcourse_status == "Cured", na.rm = TRUE),
            p_dead_sur_curedNdead = round(n_dead / (n_dead + n_cured), 2),
            p_dead_sur_tot = round(n_dead / n_visits_hospi, 2))

DATA %>% 
  group_by(continent, patinfo_sex, MSF_severity) %>% 
  summarise(n_tot = n(),
            n_dead = sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            prop = round((n_dead / n_tot), 2),
            conca = paste(n_dead, "/", n_tot, " (", prop*100, "%)", sep ="")) %>% 
  select(continent, patinfo_sex, MSF_severity, conca) %>% 
  pivot_wider(names_from = patinfo_sex,
              values_from = conca) %>% 
  arrange(continent, MSF_severity )



DATA %>% 
  group_by(continent, patinfo_sex) %>% 
  summarise(n_tot = n(),
            n_dead = sum(ind_outcome_patcourse_status == "Died", na.rm = TRUE),
            prop = round((n_dead / n_tot), 2),
            conca = paste(n_dead, "/", n_tot, " (", prop*100, "%)", sep ="")) %>% 
  select(continent, patinfo_sex, conca) %>% 
  pivot_wider(names_from = patinfo_sex,
              values_from = conca) %>% 
  arrange(continent )








