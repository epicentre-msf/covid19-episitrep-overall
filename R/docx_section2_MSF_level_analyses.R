
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
                fpar(ftext('This section of the report analyses data reported by MSF in the Excel linelist created by Epicentre and distributed to MSF projects. OCs are highly encouraged to send updated linelist exports on a weekly basis by Tuesdays nights to the following email address (EPI-COVID19-DATA@epicentre.msf.org) (please respect your OC guidelines by discussing with identified surveillance focal point).', 
                           prop = calibri_8)))


my_doc <- add_end_section_2columns()



# --- --- --- --- --- --- 
# OVERVIEW
# --- --- --- --- --- --- 

## Heading 
# --- --- --- --- --- --- 
my_doc <- add_heading2('Overview of the updated MSF linelists')

my_doc <- add_end_section_continuous()



## - Text
# --- --- --- --- --- --- 

# 1
my_doc <- add_par_normal(
  sprintf("As of %s, data were reported from %s MSF project sites representing all 5 OCs, for a total of %s patients consulted and/or admitted, including %s confirmed. Twenty-one projects however only reported aggregated data – they are included in Table 2 only as of now but not in other analysis below nor in dashboard. The number of projects reporting is constantly progressing, as well as numbers. Given that the number of projects reporting is now important, Table 2 only displays numbers by country and all the details of data by project is available here.", 
  format(date_max_report , "%d %B %Y"), 
  words(nb_msf_sites), 
  nb_msf_obs, 
  nb_msf_confirmed))

# 2
my_doc <- add_par_normal(
  sprintf("The weekly evolution of patients consulted by covid19 status is shown in Figure 7 below. The number of patients seen in MSF facilities in Africa seems to have been stable in the last weeks, but with an important decrease in the number of confirmed cases seen in the last week. In Asia, the number of patients received is increasing, however most of them are only suspected and not confirmed, probably due to testing access difficulties. Few suspected cases have been seen in Europe (Greece), while in the Americas the number of patients is oscillating between 100 and 150 patients over the last weeks."))


my_doc <- add_end_section_2columns()


# Histogram  
my_doc <- add_figure(
  object_name  = glue("msf_epicurve_status_continent_{week_report}.png"), 
  figure_title = glue('Weekly evolution of the total number of patients seen in MSF facilities by covid19 status and by continent (only individual data are displayed)'), 
  folder = 'msf')


# Table
my_doc <- add_table(
  object_name = glue("gtbl_countries_covid_status_{week_report}.png"), 
  table_title = glue('Summary of Covid-19 infection-related status of patients consulted/admitted'), 
  folder = 'msf', 
  width = 16.93 * cm_to_in, 
  height = 23.02 * cm_to_in)




# --- --- --- --- --- --- 
# ANALYSES ALL PATIENTS
# --- --- --- --- --- --- 


my_doc <- add_heading2('Covid19 patients consulted or admitted in MSF facilities')


my_doc <- add_end_section_continuous()


# - Text
# --- --- --- --- --- --- 

# 1
my_doc <- add_par_normal(
  sprintf("Figure 8 displays the evolution in number of patients by consultation/admission status and by continent. While the proportion of cases admitted in the Americas is stable around 50%%, this proportion recently dropped to 25%% in Africa and appears very low in Asia. However, it is to note that those numbers bring together MSF projects with very different activities and hospitalization capacities, which influences this analysis and renders difficult any final conclusion."))

# 2
my_doc <- add_par_normal(
  sprintf("Confirmed cases consulted/admitted to MSF facilities remain relatively young, with a median age of 39 years (stable over the last weeks). Twice as many males than females consulted in MSF facility for suspected/confirmed covid19. 259 (32%%) of the confirmed cases reported at least one comorbidity (Table 3)."))


my_doc <- add_end_section_2columns()


# Histogram 
my_doc <- add_par_normal('')


my_doc <- add_figure(
  object_name  = glue("msf_epicurve_consult_admit_continent_{week_report}.png"), 
  figure_title = glue("Weekly evolution of the number of consulted and admitted patients in MSF facilities, by continent (all covid19 status)"), 
  folder = 'msf')


# Table
my_doc <- add_par_normal('')


my_doc <- add_table(
  object_name = paste0('gtbl_general','_', week_report, '.png'), 
  table_title = "Characteristics of patients admitted to the MSF supported facilities", 
  folder = 'msf', 
  width = 14.14 * cm_to_in, 
  height = 9.74 * cm_to_in)


my_doc <- add_end_section_continuous()


# - MORE Text
# --- --- --- --- --- --- 

# 1
my_doc <- add_par_normal(
  sprintf("Ninety-two confirmed, probable, or suspected cases with known outcome died, which gives a high Case Fatality Risk (CFR) of 13.2%% (Table 4). CFR among probables and suspects appear really high, though the number of patients with known outcome is lower for those patients. This could reflect the difficult access to testing in some countries, but remain to be further investigated."))

# 2
my_doc <- add_par_normal(
  sprintf("In MSF facilities, 32.7%% of confirmed cases with known outcome over 65 years of age died.  Median age among deceased patients was 60 years. The proportion of patients who died largely varied with the number of comorbidities declared,  from 3.6%% (0 comorbidities declared) to 60%% (3 comorbidities declared) (Table 5)."))

# 3
my_doc <- add_par_normal(
  sprintf("Length of stay appeared stable in last weeks and is estimated about 7-8 days (Figure 9). The delay between onset and consultation/admission was arounf 7 days (4 days in the last week) (Figure 10)."))




my_doc <- add_end_section_2columns()


# Table
my_doc <- add_table(
  object_name = glue('gtbl_cfr_status_continent_{week_report}.png'), 
  table_title = 'Case Fatality Risk in MSF facilities, by covid status and continent', 
  folder = 'msf', 
  width = 8.1 * cm_to_in, 
  height = 2.9 * cm_to_in)


my_doc <- add_par_normal('')


my_doc <- add_table(
  object_name = glue('gtbl_cfr_factors_1_{week_report}.png'), 
  table_title = 'Distribution of patient outcomes in MSF facilities, among confirmed cases', 
  folder = 'msf', 
  width = 8.1 * cm_to_in, 
  height = 2.9 * cm_to_in)


my_doc <- add_end_section_2columns()



my_doc <- add_figure(
  object_name  = glue("msf_length_stay_{week_report}.png"), 
  figure_title = "Weekly evolution of the distribution of delay between onset and consultation/admission", 
  folder = 'msf')


my_doc <- add_par_normal('')


my_doc <- add_figure(
  object_name  = glue("msf_onset_admit_delay_{week_report}.png"), 
  figure_title = "Weekly evolution of the distribution of length of stay among hospitalized patient", 
  folder = 'msf')




# --- --- --- --- --- --- 
# ANALYSES CONFIRMED
# --- --- --- --- --- --- 


my_doc <- add_heading2("Medical characteristics of patients consulted/admitted in MSF facilities")


my_doc <- add_end_section_continuous()


# - Text
# --- --- --- --- --- --- 

# 1
my_doc <- add_par_normal(
  sprintf("Symptoms of patients upon admission are presented in Table 6. To note are anosmia and ageusia which were respectively present in 34.1 and 42.4%% of confirmed patients, while 8.5 and 10.5%% of non-cases presented these symptoms."))

# 2
my_doc <- add_par_normal(
  sprintf("Two hundred and thirty-four patients (22.2%% of patients with recorded information) were supported with oxygen, most of them in the Americas. Fifty-four (5.1%% with patients with recorded information) were admitted into an Intensive Care Unit. 29 patients were supported with a ventilator, while no patient received ECMO (Table 7)."))



my_doc <- add_par_normal(
  sprintf("Table 7 presents the types of comorbidities recorded in confirmed patients with known cured or died outcome. Diabetes, cardiovascular disease (mostly hypertension) and malaria are the most frequently recorded comorbidities. Eleven out of 16 patient with underlying respiratory disease died. No patient appears to be recorded with malnutrition, and among those who presented with HIV, TB or malaria, none died"))


my_doc <- add_end_section_2columns()


my_doc <- add_table(
  object_name = glue('gtbl_age_sex_outcome_{week_report}.png'), 
  table_title = 'Distribution of patient outcomes in MSF facilities, among confirmed, probable and suspected cases ', 
  folder = 'msf', 
  width = 14.99 * cm_to_in, 
  height = 6.51 * cm_to_in)


my_doc <- add_end_section_continuous()


my_doc <- add_table(
  object_name = glue('gtbl_risk_comcond_{week_report}.png'), 
  table_title = 'Frequency and percentage of comorbidities among cured and died confirmed patients', 
  folder = 'msf', 
  width = 9.47 * cm_to_in, 
  height = 24.73 * cm_to_in)


my_doc <- add_par_normal('')


my_doc <- add_table(
  object_name = glue('gtbl_care_{week_report}.png'), 
  table_title = 'Major patient’s care procedures by location (confirmed patients)', 
  folder = 'msf', 
  width = 9.47 * cm_to_in, 
  height = 24.73 * cm_to_in)


my_doc <- add_end_section_2columns()



my_doc <- add_par_normal('')




