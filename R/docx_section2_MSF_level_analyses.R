calibri_8 <- fp_text(font.family = "Calibri", 
                     font.size = 8)

calibri_8_bold <- update(calibri_8, bold = TRUE)

calibri_10_bold_orange <- fp_text(font.family = "Calibri", 
                                  font.size = 10,
                                  color = "orange",
                                  bold = TRUE)


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
                           prop = calibri_10_bold_orange))) %>% 
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
  sprintf("As of %s, %s MSF project sites representing all 5 OCs and %s countries reported data, for a total of %s consultations/admissions, including %s confirmed. %s projects reported only aggregated data (included in Figure 6 and in the dashboard, but not in further analyses below). ", 
  format(date_max_report , "%d %B %Y"), 
  nb_msf_sites, 
  nb_msf_countries, 
  format(nb_msf_obs, big.mark   = ','), 
  format(nb_msf_confirmed, big.mark   = ','), 
  Words(nb_msf_sites_aggregated)))
  
# 2

my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = ('Last week data may not be fully complete if projects did not report on time. Detailed data by project is available ')) %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/tables_msf_sites/") %>% 
  slip_in_text(style = 'Description char', 
               str = ').') 


# 3
my_doc <- add_par_normal(
  sprintf("The weekly evolution of patients consulted by covid19 status is shown in Figure 5 below. The continent with the higher number of patients is XXX_TO_CONTINUE"))


my_doc <- add_end_section_2columns()




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
  sprintf("Figure 6 displays the evolution in number of patients by type of health service received (consultation/admission) and by continent. Overall %s%% of patients were admitted (%s%% in Africa, %s%% in the Americas, %s%% in Asia and %s%% in Europe). However, it is to note that those numbers bring together MSF projects with very different activities and hospitalization capacities, which influences this analysis and renders difficult any final conclusion.",
          tbl_consultation_continent %>% pull(admitted_percent) %>% 
            last() %>% format(digits = 1),
          tbl_consultation_continent %>% filter(continent == 'Africa') %>% 
            pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent == 'Americas') %>% 
            pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent == 'Asia') %>% 
            pull(admitted_percent) %>% format(digits = 1), 
          tbl_consultation_continent %>% filter(continent == 'Europe') %>% 
            pull(admitted_percent) %>% format(digits = 1)))

# 2
my_doc <- add_par_normal(
  sprintf("Patients consulted/admitted to MSF facilities had median age of %s years. The male/female ratio was %s among all Covid19-related patients and %s (%s%%) patients reported at least one comorbidity. The most frequent comorbidities recorded are hypertension, diabetes, respiratory diseases and malaria. XXX_TO_CONTINUE", 
          median(dta_linelist$age_in_years, na.rm = TRUE),
          tbl_sex_ratio %>% pull(ratio_mf) %>% last(), 
          sum(dta_linelist$ind_Comcond_01), 
          format_percent(sum(dta_linelist$ind_Comcond_01)/count(dta_linelist)) %>% pull())) 


my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Histogram Week Covid Admission
my_doc <- add_figure(
  object_name  = paste0('hist_epicurve_consult_admit_continent', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of the number of consulted/admitted patients in MSF facilities, by continent (all status)", 
  folder = 'msf')


# my_doc <- add_end_section_continuous()

my_doc <- add_end_section_2columns()




# - Text symptoms
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("There is not a clear pattern of symptoms that allows to differentiate confirmed and non-cases. Loss of smell and taste is more frequent among confirmed cases, although a non-negligible proportion of non-cases present these symptoms. Aches and headache also seemed more frequent in confirmed cases."))


dta_severity <- dta_linelist %>% 
  mutate(
    MSF_severity = factor(MSF_severity, levels = c('Mild', 'Moderate', 'Severe', 'Critical'))
  ) %>% 
  drop_na(MSF_severity)

tbl_severity_all <- dta_severity %>% 
  summarise(n_all_patients = n(),
            n_sc = sum(MSF_severity %in% c("Severe", "Critical"), na.rm = TRUE),
            p_sc = 100 * n_sc / n_all_patients) 

tbl_severity_confirmed <- dta_severity %>% 
  filter(ind_MSF_covid_status == "Confirmed") %>% 
  group_by(ind_MSF_covid_status) %>% 
  summarise(n_all_patients = n(),
            n_sc = sum(MSF_severity %in% c("Severe", "Critical"), na.rm = TRUE),
            p_sc = 100 * n_sc / n_all_patients)



my_doc <- add_par_normal(
  sprintf("Table 2 describes the levels of severity found in patients consulted/admitted in MSF facilities, according to their Covid19 status. Though there was a lot of missing data for this variable, overall, %s%% of all patients (%s%% of confirmed) received were assessed to be in a severe or critical state.",
          tbl_severity_all %>% pull(p_sc) %>% round(1),
          tbl_severity_confirmed %>% pull(p_sc) %>% round(1)
          )
  )


my_doc <- add_par_normal('')






my_doc <- add_table(
  object_name = paste0('gtbl_severity_status', '_', week_report, '.png'), 
  table_title = 'Levels of severity in MSF facilities, by covid19 status', 
  folder = 'msf', 
  width = 8.93 * cm_to_in, 
  height = 9.29 * cm_to_in)

my_doc <- add_end_section_2columns()



my_doc <- add_figure(
  object_name  = paste0('hist_fatality_continent', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of number of died and cured admitted patients, by continent (all status) ", 
  folder = 'msf')


my_doc <- add_end_section_continuous()






# --- --- --- --- --- --- 
# ANALYSES CONFIRMED
# --- --- --- --- --- --- 


my_doc <- add_heading2("Covid19 confirmed patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text Confirmed 
# --- --- --- --- --- --- 

# 1 - Age sex of confirmed
my_doc <- add_par_normal(
  sprintf("Covid19-confirmed patients consulted/admitted to MSF facilities remain relatively young, with a median age of %s years (stable over the last weeks). Table 3 shows that proportions of severe and critical patients appear similar across the continents. Due to disparity in projects represented, severity proportions may not be representative of cases occurring on each continent.",
          tbl_age_distribution_by_sex_confirmed_only %>% filter(sex == 'All') %>% pull(age_median))) 


my_doc <- add_par_normal(
  sprintf("Among confirmed patients, %s%% were admitted in hospitals. %s patients (%s%% of patients with recorded information) were supported with oxygen, %s%% were admitted into an Intensive Care Unit and %s%%) were supported with a ventilator (Table 4).", 
          tbl_care_admitted %>% filter(merge_admit == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_oxygen %>% filter(merge_oxygen == 'Yes') %>% pull(n_Total) %>% Words(), 
          tbl_care_oxygen %>% filter(merge_oxygen == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_icu %>% filter(merge_icu == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1), 
          tbl_care_vent %>% filter(merge_vent == 'Yes') %>% pull(p_Total) %>% format_percent(digits = 1)))


my_doc <- add_par_normal(
  sprintf("In MSF facilities, %s%% of confirmed cases with known outcome died, including %s%% in over 65 years of age. The median age among deceased patients was %s years (stable in the last weeks). The proportion of patients who died largely varied with the number of comorbidities declared (Table 5). CFR starts increasing from the XXX-XX years old category, to reach over XXX%% in patients over 80 years old", 
          round(nb_msf_conf_who_died / nb_msf_conf_known_outcome* 100, digits = 1), 
          round(nb_msf_conf_above65_who_died / nb_msf_conf_above65 * 100, digits = 1), 
          median_age_confirmed_died))



my_doc <- add_table(
  object_name = paste0('gtbl_severity_confirmed_continent', '_', week_report, '.png'), 
  table_title = 'Patient’s severity status by continent (confirmed)', 
  folder = 'msf', 
  width = 8.93 * cm_to_in, 
  height = 9.29 * cm_to_in)



my_doc <- add_par_normal(
  sprintf("The overall delay between onset and consultation/admission (median number of days) was %s days. The overall length of stay (median number of days) was %s days (Figure 11", 
          median(dta_delay$delay_before_consultation),
          median(dta_length_stay$MSF_length_stay)))


my_doc <- add_table(
  object_name = paste0('gtbl_care', '_', week_report, '.png'), 
  table_title = 'Major patient’s care procedures by location (confirmed patients)', 
  folder = 'msf', 
  width = 8.93 * cm_to_in, 
  height = 9.29 * cm_to_in)


# Table CFR by age-group and sex
my_doc <- add_table(
  object_name = paste0('gtbl_cfr_age_sex_comorbidities', '_', week_report, '.png'), 
  table_title = 'Case fatality risk by age, sex and number of comorbidities among Covide-19 confirmed patients', 
  folder = 'msf', 
  width = 8.1 * cm_to_in, 
  height = 7.91 * cm_to_in)


my_doc <- add_end_section_2columns()


my_doc <- add_table(
  object_name = paste0('gtbl_cfr_type_comcond', '_', week_report, '.png'), 
  table_title = 'Case fatality risk by type of comorbidities among Covid-19 confirmed patients', 
  folder = 'msf', 
  width = 13.73 * cm_to_in, 
  height = 8.61 * cm_to_in)


my_doc <- add_par_normal('')









# TABLE STATUS PATIENTS
# my_doc <- add_table(
#   object_name = paste0('gtbl_countries_covid_status', '_', week_report, '.png'), 
#   table_title = glue('Summary of Covid-19 infection-related status of patients consulted/admitted'), 
#   folder = 'msf', 
#   width = 16.93 * cm_to_in, 
#   height = 23.02 * cm_to_in) # size may need to be adapted with the increase of the number of countries



# Graph DELAY TO ADMISSION
# my_doc <- add_figure(
#   object_name = paste0('boxplot_delay_before_consultation','_', week_report, '.png'), 
#   figure_title = "Weekly evolution of the distribution of delay from onset of symptoms and consultation/admission", 
#   folder = 'msf', 
#   width = 8.37 * cm_to_in, 
#   height = 5.58 * cm_to_in)




#  Graph LENGTH OF STAY HOSPITALISED
# my_doc <- add_figure(
#   object_name  = paste0('boxplot_length_stay', '_', week_report, '.png'), 
#   figure_title = "Weekly evolution of the distribution of length of stay among hospitalized patient", 
#   folder = 'msf', 
#   width = 8.37 * cm_to_in, 
#   height = 5.58 * cm_to_in)



# Graph CFR BY AGE-GROUP
# my_doc <- add_figure(
#   object_name = paste0('dots_cfr_agegroup', '_', week_report, '.png'), 
#   figure_title = "Case fatality risk by age groups among Covide-19 confirmed patients", 
#   folder = 'msf', 
#   width = 8.97 * cm_to_in, 
#   height = 5.57 * cm_to_in)



# TEXT - Comorbidities
# my_doc <- add_par_normal(
#   sprintf("Table 9 presents the case fatality among confirmed cases according to the types of comorbidity (patients with known cured or died outcome in the denominator). XXXX are the comorbidities associated wit hthe highest case fatality. Over/Almost XX%% with underlying respiratory disease died......"))
# my_doc <- add_end_section_2columns()



# --- --- --- --- --- --- 
# ANALYSES NON-CASES
# --- --- --- --- --- --- 

# my_doc <- add_heading2("Non-Covid19 patients consulted or admitted in MSF facilities")
# 
# my_doc <- add_end_section_continuous()
# 
# # - Text
# # --- --- --- --- --- --- 
# # 1
# my_doc <- add_par_normal(
#   sprintf("Among patients received in MSF facilities, some were considered non-cases after testing negative. As expected, Covid-19 disease was mainly confused with respiratory syndromes such as upper respiratory tract infection, lower respiratory tract infection, flu syndrome or chronic lung disease."))


#my_doc %<>% 
#  body_add_fpar(style = 'Normal', 
#                fpar(ftext('For more information are available ', 
#                           prop = calibri_8))) %>% 
#  slip_in_text(style = 'Hyperlink', 
#               str = "here", 
#               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/additional_episitrep_outputs/") %>% 
#  slip_in_text(style = 'Normal char', 
#               str = ".") 

# my_doc <- add_end_section_2columns()
# 
# my_doc <- add_par_normal("")
# 
# my_doc <- add_table(
#   object_name = paste0('gtbl_non_cases_diagt', '_', week_report, '.png'), 
#   table_title = 'Frequency of main diagnosis among non confirmed patients who attended an MSF Covid facility, by type of outcome', 
#   folder = 'msf', 
#   width = 15.85 * cm_to_in, 
#   height = 11.92 * cm_to_in)
# 
# my_doc <- add_par_normal('')




# TABLE COMORBIDITIES
# my_doc <- add_table(
#   object_name = paste0('gtbl_comcond_status', '_', week_report, '.png'), 
#   table_title = 'Frequency and percentage of comorbidities or underlying conditions among patients consulted/admitted', 
#   folder = 'msf', 
#   width = 15.61 * cm_to_in, 
#   height = 9.45 * cm_to_in)


# TABLE SYMPTOMS
# my_doc <- add_table(
#   object_name = paste0('gtbl_sympt_all', '_', week_report, '.png'), 
#   table_title = 'Frequency and percentage of signs and symptoms of patients consulted/admitted', 
#   folder = 'msf', 
#   width = 17.45 * cm_to_in, 
#   height = 9.45 * cm_to_in)
  


# Text CFR all patients
# --- --- --- --- --- --- 
# my_doc <- add_par_normal(
#   sprintf("%s confirmed, probable, or suspected cases with known outcome (cured/died) died, which gives a case fatality risk (CFR) of %s%%. Figure 7 shows the evolution of death numbers and of CFR across the continents.", 
#           Words(nb_msf_conf_prob_susp_who_died), 
#           round(cfr_confirmed_probable_suspected * 100, digits = 1)))
# my_doc <- add_par_normal('')


# TABLE CFR ALL PATIENTS
# --- --- --- --- --- --- 
# my_doc <- add_table(
#   object_name = paste0('gtbl_cfr_status_continent', '_', week_report, '.png'), 
#   table_title = 'Case fatality risk in MSF facilities, by covid19 status and continent', 
#   folder = 'msf', 
#   width = 8.87 * cm_to_in, 
#   height = 3.08 * cm_to_in)



# GRAPH AGE PYRAMID CONFRMED
# my_doc <- add_figure(
#   object_name = paste0('pyramid_age_sex_confirmed_continent', '_', week_report, '.png'), 
#   figure_title = "Age pyramid of Covid-confirmed patients consulted/ admitted in MSF facilities, by continent", 
#   folder = 'msf', 
#   width = 8.57 * cm_to_in, 
#   height = 8.57 * cm_to_in)




# GRAPH  AGE PYRAMID ALL PATIENTS
# my_doc <- add_figure(
#   object_name = paste0('pyramid_age_sex_all_continent', '_', week_report, '.png'), 
#   figure_title = "Age pyramid of patients consulted/admitted in MSF facilities, by continent", 
#   folder = 'msf', 
#   width = 8.57 * cm_to_in, 
#   height = 8.57 * cm_to_in)



# TABLE PATIENT CHARACTERISTICS
# my_doc <- add_table(
#   object_name = paste0('gtbl_general', '_', week_report, '.png'), 
#   table_title = glue('Patient characteristics in MSF facilities, by covid19 status'), 
#   folder = 'msf', 
#   width = 9.17 * cm_to_in, 
#   height = 5.49 * cm_to_in) # size may need to be adapted with the increase of the number of countries