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
