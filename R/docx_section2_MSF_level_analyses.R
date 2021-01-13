calibri_8 <- fp_text(font.family = "Calibri", 
                     font.size = 8)

calibri_8_bold <- update(calibri_8, bold = TRUE)

# --- --- --- --- --- --- --- --- --- 
# II. MSF level analyses - Heading
# --- --- --- --- --- --- --- --- ---

my_doc <- add_heading1('Analysis at MSF level') 


my_doc <- add_end_section_continuous()


# WHAT WE EXPECT
# --- --- --- --- --- --- 

##  What we expect - Heading
my_doc %<>% 
  body_add_fpar(style = 'Description', 
                fpar(ftext("This section analyses data reported by MSF in the reporting system created by Epicentre and distributed to MSF projects. OCs are highly encouraged to send updated linelist exports on a weekly basis by Tuesdays nights to the following email address (EPI-COVID19-DATA@epicentre.msf.org) (please respect your OC guidelines by discussing with identified surveillance focal point).", 
                           prop = calibri_8)))


my_doc <- add_end_section_2columns()

my_doc %<>% 
  body_add_fpar(style = 'Description bold', 
                fpar(ftext('This section presents the main analysis of the dataset. However, supplementary analysis and figures are available in the full analysis report available ', 
                           prop = calibri_8))) %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/additional_episitrep_outputs_msf/") %>% 
  slip_in_text(style = 'Description char', 
               str = '. ') 


# === === === === === === 
# OVERVIEW
# === === === === === === 

## Heading 
# --- --- --- --- --- --- 
my_doc <- add_heading2('Overview of the updated MSF linelists')

my_doc <- add_end_section_continuous()



## - Text
# --- --- --- --- --- --- 

# 1
my_doc <- add_par_normal(
  sprintf("As of %s, %s MSF project sites representing all 5 OCs and %s countries reported data, for a total of %s patients consulted and/or admitted, including %s confirmed. %s projects reported only aggregated data (included in Table 2 and Figure 6, but not in further analysis below nor in dashboard). The number of projects reporting continue to steadily progressing.", 
  format(date_max_report , "%d %B %Y"), 
  nb_msf_sites, 
  nb_msf_countries, 
  format(nb_msf_obs, big.mark   = ','), 
  format(nb_msf_confirmed, big.mark   = ','), 
  Words(nb_msf_sites_aggregated)))
  
# 2
my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = ('Last week data may not be fully complete if projects did not report on time. Given the important number of projects, Table 2 displays numbers by country and detailed data by project is available ')) %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/tables_msf_sites/") %>% 
  slip_in_text(style = 'Normal char', 
               str = '. ') 

my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = ('Last week data may not be fully complete if projects did not report on time. Given the important number of projects, Table 2 displays numbers by country and detailed data by project is available here'))


# 3
my_doc <- add_par_normal(
  sprintf("The weekly evolution of patients consulted by covid19 status is shown in Figure 6 below. The continent with the higher number of patients is ... TO CONTINUE.... "))


my_doc <- add_end_section_2columns()


# Table
my_doc <- add_table(
  object_name = paste0('gtbl_countries_covid_status', '_', week_report, '.png'), 
  table_title = glue('Summary of Covid-19 infection-related status of patients consulted/admitted'), 
  folder = 'msf', 
  width = 16.93 * cm_to_in, 
  height = 23.02 * cm_to_in) # size may need to be adapted with the increase of the number of countries


# Histogram  
my_doc <- add_figure(
  object_name  = glue("hist_epicurve_status_continent_{week_report}.png"), 
  figure_title = glue('Weekly evolution of the total number of patients seen in MSF facilities by covid19 status and by continent (individual and aggregated data are displayed)'), 
  folder = 'msf')




# === === === === === === === 
# PATIENTS' CHARACTERISTICS
# === === === === === === === 

my_doc <- add_heading2("Characteristics of patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("Figure 7 displays the evolution in number of patients by type of health service received (consultation/admission) and by continent. Overall %s%% of patients were admitted (%s%% in Africa, %s%% in the Americas, %s%% in Asia and %s%% in Europe). However, it is to note that those numbers bring together MSF projects with very different activities and hospitalization capacities, which influences this analysis and renders difficult any final conclusion.",
          tbl_consultation_continent %>% pull(admitted_percent) %>% last() %>% format(digits = 1),
          tbl_consultation_continent %>% filter(continent=='Africa')%>% pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent=='Americas')%>% pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent=='Asia')%>% pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent=='Europe')%>% pull(admitted_percent) %>% format(digits = 1)))

# 2
my_doc <- add_par_normal(
  sprintf("Patients consulted/admitted to MSF facilities remain relatively young, with a median age of %s years (stable over the last weeks). Figure 8 shows the age pyramid of patients consulted/admitted to MSF facilities.", 
          median(dta_linelist$age_in_years, na.rm = TRUE))) 


# 3
my_doc <- add_par_normal(
  sprintf("The male/female ratio was %s among all covid19-related patients (stable). %s (%s%%) patients reported at least one comorbidity (Table 3). The frequency of comorbidities by covid status are listed in Table 4.", 
          tbl_sex_ratio %>% pull(ratio_mf) %>% last(), 
          sum(dta_linelist$Comcond_01), 
          format_percent(sum(dta_linelist$Comcond_01)/count(dta_linelist)) %>% pull()))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Histogram Week Covid Admission
my_doc <- add_figure(
  object_name  = paste0('hist_epicurve_consult_admit_continent', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of the number of consulted/admitted patients in MSF facilities, by continent (all status)", 
  folder = 'msf')


my_doc <- add_end_section_continuous()


# Age pyramid
my_doc <- add_figure(
  object_name = paste0('pyramid_age_sex_all_continent', '_', week_report, '.png'), 
  figure_title = "Age pyramid of patients consulted/admitted in MSF facilities, by continent", 
  folder = 'msf', 
  width = 8.57 * cm_to_in, 
  height = 8.57 * cm_to_in)

my_doc <- add_par_normal('')

# Table
my_doc <- add_table(
  object_name = paste0('gtbl_general', '_', week_report, '.png'), 
  table_title = glue('Patient characteristics in MSF facilities, by covid19 status'), 
  folder = 'msf', 
  width = 9.17 * cm_to_in, 
  height = 5.49 * cm_to_in) # size may need to be adapted with the increase of the number of countries


my_doc <- add_end_section_2columns()


# - Text symptoms
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("Symptoms of patients upon admission are presented in Table 5. There is not a clear pattern of symptoms that allows to differentiate confirmed and non-cases. Loss of smell and taste is more frequent among confirmed cases, as expected. Nevertheless, a non-negligible proportion of non-cases present these symptoms; these appear to mainly come from two specific projects, and remain to be further investigated."))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Table Comorbidities
my_doc <- add_table(
  object_name = paste0('gtbl_comcond_status', '_', week_report, '.png'), 
  table_title = 'Frequency and percentage of comorbidities or underlying conditions among patients consulted/admitted', 
  folder = 'msf', 
  width = 15.61 * cm_to_in, 
  height = 9.45 * cm_to_in)

my_doc <- add_par_normal('')

# Table symptoms
my_doc <- add_table(
  object_name = paste0('gtbl_sympt_all', '_', week_report, '.png'), 
  table_title = 'Frequency and percentage of signs and symptoms of patients consulted/admitted', 
  folder = 'msf', 
  width = 17.45 * cm_to_in, 
  height = 9.45 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_end_section_continuous()


# - Text CFR all patients
# --- --- --- --- --- --- 
my_doc <- add_par_normal(
  sprintf("%s confirmed, probable, or suspected cases with known outcome (cured/died) died, which gives a case fatality risk (CFR) of %s%% (Table 6). CFR among probables and suspects appear really high. This could reflect the difficult access to testing in some countries, but remain to be further investigated.", 
          Words(nb_msf_conf_prob_susp_who_died), 
          round(cfr_confirmed_probable_suspected * 100, digits = 1)))

my_doc <- add_par_normal('')


# Table CFR all patients
# --- --- --- --- --- --- 
my_doc <- add_table(
  object_name = paste0('gtbl_cfr_status_continent', '_', week_report, '.png'), 
  table_title = 'Case fatality risk in MSF facilities, by covid19 status and continent', 
  folder = 'msf', 
  width = 8.87 * cm_to_in, 
  height = 3.08 * cm_to_in)


my_doc <- add_end_section_2columns()


# --- --- --- --- --- --- 
# ANALYSES CONFIRMED
# --- --- --- --- --- --- 
my_doc <- add_heading2("Covid19 confirmed patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text Confirmed 
# --- --- --- --- --- --- 

# 1 - Age sex of confirmed
my_doc <- add_par_normal(
  sprintf("Codiv-confirmed patients consulted/admitted to MSF facilities remain relatively young, with a median age of %s years (stable over the last weeks). Among confirmed cases, the median age for females was %s years and for males %s years. Figure 9 shows the age pyramid of patients consulted/admitted to MSF facilities. In Africa the most represented age group among confirmed patients was ...",
          tbl_age_distribution_by_sex_confirmed_only %>% filter(sex == 'All') %>% pull(age_median), 
          tbl_age_distribution_by_sex_confirmed_only %>% filter(sex == 'Female') %>% pull(age_median), 
          tbl_age_distribution_by_sex_confirmed_only %>% filter(sex == 'Male') %>% pull(age_median))) 


my_doc <- add_par_normal(
  sprintf("Among confirmed patients, %s%% were admitted in hospitals. %s patients (%s%% of patients with recorded information) were supported with oxygen. %s (%s%% of patients with recorded information) were admitted into an Intensive Care Unit. %s patients (%s%%) were supported with a ventilator (Table 7).", 
          tbl_care_admitted %>% filter(merge_admit == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_oxygen %>% filter(merge_oxygen == 'Yes') %>% pull(n_Total) %>% Words(), 
          tbl_care_oxygen %>% filter(merge_oxygen == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_icu %>% filter(merge_icu == 'Yes') %>% pull(n_Total) %>% Words(), 
          tbl_care_icu %>% filter(merge_icu == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_vent %>% filter(merge_vent == 'Yes') %>% pull(n_Total) %>% Words(), 
          tbl_care_vent %>% filter(merge_vent == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1)))


my_doc <- add_end_section_2columns()


# Graph Pyramid 
my_doc <- add_figure(
  object_name = paste0('pyramid_age_sex_confirmed_continent', '_', week_report, '.png'), 
  figure_title = "Age pyramid of Covid-confirmed patients consulted/ admitted in MSF facilities, by continent", 
  folder = 'msf', 
  width = 8.57 * cm_to_in, 
  height = 8.57 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_table(
  object_name = paste0('gtbl_care', '_', week_report, '.png'), 
  table_title = 'Major patient’s care procedures by location (confirmed patients)', 
  folder = 'msf', 
  width = 8.93 * cm_to_in, 
  height = 9.29 * cm_to_in)


my_doc <- add_end_section_2columns()


my_doc <- add_par_normal(
  sprintf("The delay between onset and consultation/admission (median number of days) was %s days (Figure 10).", 
          median(dta_delay$delay_before_consultation)))

my_doc <- add_par_normal(
  sprintf("The length of stay (median number of days) was %s days (Figure 11)", 
          median(dta_length_stay$MSF_length_stay)))


my_doc <- add_end_section_2columns()

# Graph Evolution delay to admission
my_doc <- add_figure(
  object_name = paste0('boxplot_delay_before_consultation','_', week_report, '.png'), 
  figure_title = "Weekly evolution of the distribution of delay from onset of symptoms and consultation/admission", 
  folder = 'msf', 
  width = 8.37 * cm_to_in, 
  height = 5.58 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_figure(
  object_name  = paste0('boxplot_length_stay', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of the distribution of length of stay among hospitalized patient", 
  folder = 'msf', 
  width = 8.37 * cm_to_in, 
  height = 5.58 * cm_to_in)


my_doc <- add_end_section_2columns()

# - Text
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("In MSF facilities, %s%% of confirmed cases with known outcome over 65 years of age died. The median age among deceased patients was %s years (stable in the last weeks). The proportion of patients who died largely varied with the number of comorbidities declared, from %s%% (0 comorbidities declared) to %s%% (3 comorbidities declared) (Table 8).", 
          round(nb_msf_conf_above65_who_died / nb_msf_conf_above65 * 100, digits = 1), 
          median_age_confirmed_died, 
          format_percent(cfr_confirmed_no_comorbidities, digits = 1), 
          format_percent(cfr_confirmed_3_comorbidities, digits = 1)))


my_doc <- add_par_normal(
  sprintf("Figure 12 shows the CFR in different age-groups among confirmed patients in MSF facilities. CFR starts increasing
from XX-XX years old category, to reach over XX%% in patients over 80 years old"))


my_doc <- add_par_normal('')

# Graph CFR by age-group
my_doc <- add_figure(
  object_name = paste0('dots_cfr_agegroup', '_', week_report, '.png'), 
  figure_title = "Case fatality risk by age groups among Covide-19 confirmed patients", 
  folder = 'msf', 
  width = 8.97 * cm_to_in, 
  height = 5.57 * cm_to_in)


my_doc <- add_par_normal('')


# Table CFR by age-group and sex
my_doc <- add_table(
  object_name = paste0('gtbl_cfr_age_sex_comorbidities', '_', week_report, '.png'), 
  table_title = 'Case fatality risk by age, sex and number of comorbidities among Covide-19 confirmed patients', 
  folder = 'msf', 
  width = 8.1 * cm_to_in, 
  height = 7.91 * cm_to_in)


my_doc <- add_end_section_2columns()


# 2 - Comorbidities
my_doc <- add_par_normal(
  sprintf("Table 9 presents the case fatality among confirmed cases according to the types of comorbidity (patients with known cured or died outcome in the denominator). XXXX are the comorbidities associated wit hthe highest case fatality. Over/Almost XX%% with underlying respiratory disease died......"))


my_doc <- add_end_section_2columns()

my_doc <- add_table(
  object_name = paste0('gtbl_cfr_type_comcond', '_', week_report, '.png'), 
  table_title = 'Case fatality risk by type of comorbidities among Covide-19 confirmed patients', 
  folder = 'msf', 
  width = 13.73 * cm_to_in, 
  height = 8.61 * cm_to_in)


my_doc <- add_par_normal('')


# --- --- --- --- --- --- 
# ANALYSES NON-CASES
# --- --- --- --- --- --- 
my_doc <- add_heading2("Non-Covid19 patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("Among patients received in MSF facilities, some were considered non-cases after testing negative. As expected, Covid-19 disease was mainly confused with respiratory syndromes such as upper respiratory tract infection, lower respiratory tract infection, flu syndrome or chronic lung disease."))


#my_doc %<>% 
#  body_add_fpar(style = 'Normal', 
#                fpar(ftext('For more information are available ', 
#                           prop = calibri_8))) %>% 
#  slip_in_text(style = 'Hyperlink', 
#               str = "here", 
#               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/additional_episitrep_outputs/") %>% 
#  slip_in_text(style = 'Normal char', 
#               str = ".") 

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal("")

my_doc <- add_table(
  object_name = paste0('gtbl_non_cases_diagt', '_', week_report, '.png'), 
  table_title = 'Frequency of main diagnosis among non confirmed patients who attended an MSF Covid facility, by type of outcome', 
  folder = 'msf', 
  width = 15.85 * cm_to_in, 
  height = 11.92 * cm_to_in)

my_doc <- add_par_normal('')

