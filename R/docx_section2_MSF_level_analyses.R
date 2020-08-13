
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
                fpar(ftext('This section analyses data reported by MSF in the reporting system created by Epicentre and distributed to MSF projects. OCs are highly encouraged to send updated linelist exports on a weekly basis by Tuesdays nights to the following email address (EPI-COVID19-DATA@epicentre.msf.org) (please respect your OC guidelines by discussing with identified surveillance focal point).', 
                           prop = calibri_8)))


my_doc <- add_end_section_2columns()

my_doc %<>% 
  body_add_fpar(style = 'Description bold', 
                fpar(ftext('This section presents the main analysis of the dataset. However, supplementary analysis and figures are available in the full analysis report available ', 
                           prop = calibri_8))) %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://msfintl-my.sharepoint.com/:f:/g/personal/francesco_grandesso_epicentre_msf_org/ErlaP5s84LtOgD3JjqAonwABbPxv40nyq9250x6ZRzGcMA?e=e45W5x") %>% 
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
  words(nb_msf_sites), 
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
               hyperlink = "https://msfintl-my.sharepoint.com/:f:/g/personal/francesco_grandesso_epicentre_msf_org/Ep8aeLRD7CFKtnxH8SqVQFUBhH3BxEIGpojNrxLiS5gN-g?e=dTYoOT") %>% 
  slip_in_text(style = 'Normal char', 
               str = '. ') 

# 3
my_doc <- add_par_normal(
  sprintf("The weekly evolution of patients consulted by covid19 status is shown in Figure 6 below. The continent with the higher number of patients is Asia, though most were seen for triage only. They are classified probable due to testing access difficulties. The number of patients seen in MSF facilities in Africa seems to be more or less stable around 200-250 patients per week. Few suspected cases have been seen in Europe (Greece), while in the Americas the weekly number of patients is about 150-200 patients. "))

my_doc <- add_end_section_2columns()

# Histogram  
my_doc <- add_figure(
  object_name  = glue("hist_epicurve_status_continent_{week_report}.png"), 
  figure_title = glue('Weekly evolution of the total number of patients seen in MSF facilities by covid19 status and by continent (individual and aggregated data are displayed)'), 
  folder = 'msf')

# Table
my_doc <- add_table(
  object_name = glue("gtbl_countries_covid_status_{week_report}.png"), 
  table_title = glue('Summary of Covid-19 infection-related status of patients consulted/admitted'), 
  folder = 'msf', 
  width = 16.93 * cm_to_in, 
  height = 23.02 * cm_to_in) # size may need to be adapted with the increase of the number of countries





# === === === === === === === 
# PATIENTS' CHARACTERISTICS
# === === === === === === === 

my_doc <- add_heading2("Characteristics of patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("Figure 7 displays the evolution in number of patients by type of health service received (consultation/admission) and by continent. The proportion of cases admitted in the Americas is stable around 50%% and in Africa. However, it is to note that those numbers bring together MSF projects with very different activities and hospitalization capacities, which influences this analysis and renders difficult any final conclusion."))

# 2
my_doc <- add_par_normal(
  sprintf("Patients consulted/admitted to MSF facilities remain relatively young, with a median age of %s years (stable over the last weeks). Figure 8 shows the age pyramid of patients consulted/admitted to MSF facilities.", 
          median(dta$age_in_years, na.rm = TRUE))) 


# 3
my_doc <- add_par_normal(
  sprintf("Twice as many males than females consulted in MSF facility. %s (%s%%) patients reported at least one comorbidity. ", 
          nb_msf_confirmed_with_comorbidity, 
          round(nb_msf_confirmed_with_comorbidity / nb_msf_confirmed_linelist * 100, digits = 1)))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Histogram Week Covid Admission
my_doc <- add_figure(
  object_name  = paste0('hist_epicurve_consult_admit_continent', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of the number of consulted/admitted patients in MSF facilities, by continent (all status)", 
  folder = 'msf')

my_doc <- add_par_normal('')

# Age pyramid
my_doc <- add_figure(
  object_name = paste0('pyramid_age_sex_all_continent', '_', week_report, '.png'), 
  figure_title = "Age pyramid of patients consulted/admitted in MSF facilities, by continent", 
  folder = 'msf', 
  width = 10 * cm_to_in, 
  height = 10 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_end_section_continuous()

# - Text symptoms
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("Symptoms of patients upon admission are presented in Table 3. To note are loss of smell and taste which were
more frequent among confirmed and probable patients than non-cases. On the contrary, chills, runny nose and nose congestion appeared to be more frequent in non-case than in confirmed patients"))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Table symptoms
my_doc <- add_table(
  object_name = paste0('gtbl_sympt_all', '_', week_report, '.png'), 
  table_title = 'Frequency and percentage of signs and symptoms of patients admitted', 
  folder = 'msf', 
  width = 17.45 * cm_to_in, 
  height = 9.45 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_end_section_continuous()


# - Text CFR all patients
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("%s confirmed, probable, or suspected cases with known outcome (cured/died) died, which gives a case fatality risk (CFR) of %s%% (Table 4). CFR among probables and suspects appear really high, though the number of patients with known outcome is lower for those patients. This could reflect the difficult access to testing in some countries, but remain to be further investigated.", 
          Words(nb_msf_conf_prob_susp_who_died), 
          round(cfr_confirmed_probable_suspected * 100, digits = 1)))

my_doc <- add_par_normal('')

# Table CFR all patients
my_doc <- add_table(
  object_name = paste0('gtbl_cfr_status_continent', '_', week_report, '.png'), 
  table_title = 'Case fatality risk in MSF facilities, by covid19 status and continent', 
  folder = 'msf', 
  width = 8.87 * cm_to_in, 
  height = 3.08 * cm_to_in)

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')


# --- --- --- --- --- --- 
# ANALYSES CONFIRMED
# --- --- --- --- --- --- 
my_doc <- add_heading2("Covid19 confirmed patients consulted or admitted in MSF facilities")

my_doc <- add_end_section_continuous()

# - Text Confirmed 
# --- --- --- --- --- --- 

# 1 - Age sex of confirmed
my_doc <- add_par_normal(
  sprintf("Codiv-confirmed patients consulted/admitted to MSF facilities remain relatively young, with a median age of %s years (stable over the last weeks). Figure 8 shows the age pyramid of patients consulted/admitted to MSF facilities. Globally confirmed patients appear relatively young; in Africa the most represented age group among confirmed patients was 30-39 for males and 20-29 for females. The high number of probable cases in Asia renders the pyramid difficult to interpret for that continent.", 
          dta %>% filter(covid_status == 'Confirmed') %>% pull(age_in_years) %>% median(., na.rm = TRUE))) 

# 1 - Care
my_doc <- add_par_normal(
  sprintf("%s patients (%s%% of patients with recorded information) were supported with oxygen, about half of them in the Americas. %s (%s%% of patients with recorded information) were admitted into an Intensive Care Unit. %s patients were supported with a ventilator, while no patient received ECMO (Table 5).", 
          Words(nb_msf_received_oxygen), 
          round(pct_msf_received_oxygen * 100, digits = 1), 
          Words(nb_msf_icu), 
          round(pct_msf_icu * 100, digits = 1), 
          Words(nb_msf_vent)))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

# Graph Pyramid 
my_doc <- add_figure(
  object_name = paste0('pyramid_age_sex_confirmed_continent', '_', week_report, '.png'), 
  figure_title = "Age pyramid of Covid-confirmed patients consulted/ admitted in MSF facilities, by continent", 
  folder = 'msf', 
  width = 10 * cm_to_in, 
  height = 10 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_table(
  object_name = paste0('gtbl_care', '_', week_report, '.png'), 
  table_title = 'Major patient’s care procedures by location (confirmed patients)', 
  folder = 'msf', 
  width = 11.44 * cm_to_in, 
  height = 13.45 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_end_section_continuous()

my_doc <- add_par_normal(
  sprintf("The delay between onset and consultation/admission was around 7 days (slightly decreasing in the last weeks) (Figure 8)."))

# Graph Evolution delay to admission
my_doc <- add_figure(
  object_name = paste0('boxplot_delay_admission_week','_', week_report, '.png'), 
  figure_title = "Weekly evolution of the distribution of delay between onset and consultation/admission", 
  folder = 'msf', 
  width = 8.37 * cm_to_in, 
  height = 5.58 * cm_to_in)

my_doc <- add_par_normal('')

my_doc <- add_par_normal(
  sprintf("The length of stay appeared stable in last weeks and is estimated about 7-8 days (Figure 9)"))

my_doc <- add_par_normal('')

my_doc <- add_figure(
  object_name  = paste0('boxplot_length_stay_week', '_', week_report, '.png'), 
  figure_title = "Weekly evolution of the distribution of length of stay among hospitalized patient", 
  folder = 'msf', 
  width = 8.37 * cm_to_in, 
  height = 5.58 * cm_to_in)

my_doc <- add_par_normal('')

# - Text
# --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("In MSF facilities, %s%% of confirmed cases with known outcome over 65 years of age died. The median age among deceased patients was %s years (stable in the last weeks). The proportion of patients who died largely varied with the number of comorbidities declared, from %s%% (0 comorbidities declared) to %s%% (3 comorbidities declared) (Table 6).", 
          round(nb_msf_conf_above65_who_died / nb_msf_conf_above65 * 100, digits = 1), 
          median_age_confirmed_died, 
          round(cfr_confirmed_no_comorbidities * 100, digits = 1), 
          round(cfr_confirmed_3_comorbidities * 100, digits = 1)))


# 2 - Comorbidities
my_doc <- add_par_normal(
  sprintf("Table 7 presents the types of comorbidities recorded in confirmed patients with known cured or died outcome. Diabetes, cardiovascular disease (mostly hypertension), respiratory diseases and malaria are the most frequently recorded comorbidities. Almost 60%% with underlying respiratory disease died. No patient appears to be recorded with malnutrition, and among those who presented with HIV, TB or malaria, none died."))


my_doc <- add_par_normal('')

# Graph CFR by age-group
my_doc <- add_figure(
  object_name = paste0('dots_cfr_agegroup', '_', week_report, '.png'), 
  figure_title = "Case fatality risk in different age groups, among confirmed patients", 
  folder = 'msf', 
  width = 8.97 * cm_to_in, 
  height = 5.57 * cm_to_in)


my_doc <- add_par_normal('')


# Table CFR by age-group and sex
my_doc <- add_table(
  object_name = paste0('gtbl_cfr_age_sex_comorbidities', '_', week_report, '.png'), 
  table_title = 'Distribution of patient outcomes in MSF facilities, among confirmed cases', 
  folder = 'msf', 
  width = 8.1 * cm_to_in, 
  height = 7.91 * cm_to_in)

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

my_doc <- add_table(
  object_name = paste0('gtbl_cfr_type_comcond', '_', week_report, '.png'), 
  table_title = 'Frequency and percentage of comorbidities among cured and died confirmed patients', 
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
  sprintf("Among patients received in MSF facilities, some were considered non-cases after testing negative. Table 8 presents the main diagnosis attributed to non-case patients after excluding covid19. As expected, covid19 disease was mainly confused with respiratory syndromes such as upper respiratory tract infection, lower respiratory tract infection, flu syndrome or chronic lung disease."))

my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')

my_doc <- add_table(
  object_name = paste0('gtbl_non_cases_diagt', '_', week_report, '.png'), 
  table_title = 'Frequency of main diagnosis among non confirmed patients who attended an MSF Covid facility, by type of outcome', 
  folder = 'msf', 
  width = 15.85 * cm_to_in, 
  height = 11.92 * cm_to_in)

my_doc <- add_par_normal('')

