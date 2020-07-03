Analysis Indicators
================
Epicentre
03 July 2020

This document outlines how covid-19 indicators are calculated from the
intersectional MSF linelist compilation dataset.

## Additional Variables

Some additional variables are added to the original dataset compilation
prior to analysis:

  - `continent`: continent the country is in.
  - `region`: region the country is in. Regions defined in the World
    Bank Development Indicators.
  - `epi_week_consultation`: calculated by taking `MSF_date_consulation`
    and re-setting the date to the Monday of the week the date falls in.
    This allows for data to be aggregated by week.
  - Similarly to the `epi_week_consultation`, also `epi_week_onset`
    (from `patcourse_dateonset`), `epi_week_admission` (from
    `patcourse_presHCF`), `epi_week_report` (from `report_date`) are
    calculated.
  - `age_5gp`: groups the `age_in_years` variable into the following
    bins: 0-4, 5-14, 15-44, 45-64, 65+
  - `age_9gp`: groups the `age_in_years` variable into the following
    bins: 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+.

## Important Variables

#### Sex

linelist variable: `patinfo_sex`  
values: F, M  
missing values: not recoded

#### Covid Status

linelist variable: `MSF_covid_status`  
values: Confirmed, Not a case, Not a suspect, Probable, Suspected  
missing values: recoded as ‘Unknown’

#### Admission

linelist variables: `patcourse_admit` and `outcome_patcourse_admit`  
values: No, Yes  
missing values: recoded as ‘Unknown’

#### Visit Type

linelist variable: `MSF_visit_type`  
values: Admission to isolation center, First consultation, First
hospitalisation, First hospitalisation after a consultation, Other,
Rehospitalisation  
missing values: not recoded

#### Final Outcome

linelist variable: `outcome_patcourse_status`  
values: Cured, Died, Left against medical advice, Other, Sent back home,
Transferred  
missing values: recoded as ‘Pending/Unknown’

#### Onset to admission delay (days)

linelist variable: `MSF_delay_before_admission`  
values: `numeric`

#### Length of stay (days)

linelist variable: `MSF_length_stay`  
values: `numeric`

## Recoding “double variables”

There is a series of variables that record the same information reported
at patient’s admission (`MSF_received_oxigen, patcourse_admit,
patcourse_presHCF, patcourse_icu, patcourse_vent, and patcourse_ecmo`)
and at patient’s discharge (`MSF_outcome_received_oxigen,
outcome_patcourse_admit, outcome_patcourse_presHCF,
outcome_patcourse_icu, outcome_patcourse_vent, and
outcome_patcourse_ecmo`). These variable may be analysed separately or
as combined variables named `received_oxigen, admit, presHCF, icu, vent,
and ecmo`. The variables respond to the question of whether the patient
received the specific care (i.e. admitted to the hospital, or to an ICU,
or received oxygen, etc.) at any time during the period of his/her
illness.

## Indicators

#### Proportion Admitted

linelist variable: `patcourse_admit`  
numerator: sum of `patcourse_admit == "Yes"`  
denominator: sum of `patcourse_admit == "Yes" or "No"`

#### CFR (case fatality risk)

linelist variable: `outcome_patcourse_status`  
numerator: sum of `outcome_patcourse_status == "Died"`  
denominator: sum of `outcome_patcourse_status == "Cured" or "Died"`

#### Symptom Prevelance

linelist variable: `MSF_symptom_*` (multiple columns)  
numerator: sum of `MSF_symptom_* == "Yes"`  
denominator: sum of `MSF_symptom_* == "Yes" or "No"`

#### Comorbidity Prevelance

linelist variable: `Comcond_*` (multiple columns)  
numerator: sum of `Comcond_* == "Yes"`  
denominator: sum of `Comcond_* == "Yes" or "No"`

## Common Filters

Prior to computing indicators, it is common to filter the data to either
confirmed, or confirmed, probable and suspect patients only. Please
refer to the analysis plan for details on what filters are used in each
table or graphic.

  - `MSF_covid_status == "Confirmed"`
  - `MSF_covid_status %in% c("Confirmed", "Probable", "Suspect")`

<br>
