Analysis Indicators
================
Epicentre
24 March 2021

This document outlines how covid-19 indicators are calculated from the
intersectional MSF linelist compilation dataset.

## Additional Variables

Some additional variables are added to the original dataset compilation
prior to analysis:

-   `continent`: continent the country is in.
-   `region`: region the country is in. Regions defined in the World
    Bank Development Indicators.
-   `epi_week_consultation`: calculated by taking `MSF_date_consulation`
    and re-setting the date to the Monday of the week the date falls in.
    This allows for data to be aggregated by week.
-   Similarly to the `epi_week_consultation`, also `epi_week_onset`
    (from `patcourse_dateonset`), `epi_week_admission` (from
    `patcourse_presHCF`), `epi_week_report` (from `report_date`) are
    calculated.
-   `age_5gp`: groups the `age_in_years` variable into the following
    bins: 0-4, 5-14, 15-44, 45-64, 65+
-   `age_9gp`: groups the `age_in_years` variable into the following
    bins: 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80+.

## Important Variables

#### Sex

linelist variable: `patinfo_sex`  
values: F, M  
missing values: not recoded

#### Covid Status

linelist variable: `ind_MSF_covid_status`  
values: Confirmed, Probable, Suspected, Not a case, Not a suspect,
(Unknown)  
missing values: recoded as ‘(Unknown)’

#### Admission

linelist variables: `patcourse_admit` and `outcome_patcourse_admit`
merged as `merge_admit`  
values: Yes, No, (Unknown)  
missing values: recoded as ‘(Unknown)’

#### Visit Type

linelist variable: `MSF_visit_type`  
values: Admission in nonmedicalized structure (isolation), First
consultation, First hospitalisation, First hospitalisation after a
consultation, Follow-up consultation, Other, Rehospitalisation  
missing values: not recoded

#### Severity

linelist variable: `MSF_severity`  
values: Critical, Mild, Moderate, Severe  
missing values: not recoded

#### Final Outcome

linelist variable: `ind_outcome_patcourse_status`  
values: Cured, Died, Left against medical advice, Transferred, Sent back
home, Other, (Pending/Unknown)  
missing values: recoded as ‘(Pending/Unknown)’

#### Onset to admission delay (days)

linelist variable: `MSF_delay_before_admission`  
values: `numeric`

#### Length of stay (days)

linelist variable: `MSF_length_stay`  
values: `numeric`

## Recoding “double variables”

There is a series of variables that record the same information reported
at patient’s admission
(`MSF_received_oxigen, patcourse_admit, patcourse_presHCF, patcourse_icu, patcourse_vent, and patcourse_iso`)
and at patient’s discharge
(`MSF_outcome_received_oxigen, outcome_patcourse_admit, outcome_patcourse_presHCF, outcome_patcourse_icu, outcome_patcourse_vent, and outcome_patcourse_status`).
These variable may be analysed separately or as combined variables named
`received_oxigen, admit, presHCF, icu, vent, and status`. The variables
respond to the question of whether the patient received the specific
care (i.e. admitted to the hospital, or to an ICU, or received oxygen,
etc.) at any time during the period of his/her illness.

## Indicators

#### Proportion Admitted

linelist variable: `merge_admit`  
numerator: sum of `merge_admit == "Yes"`  
denominator: sum of `merge_admit == "Yes" or "No"`

#### CFR (case fatality risk)

Calculated on hospitalised patients only.

filter: `merge_admit == "Yes"`  
linelist variable: `ind_outcome_patcourse_status`  
numerator: sum of `ind_outcome_patcourse_status == "Died"`  
denominator: sum of
`ind_outcome_patcourse_status == "Cured" or "Died" or "Sent back home"`

#### Symptom Prevelance

linelist variable: `MSF_symptom_*` (multiple columns)  
numerator: sum of `MSF_symptom_* == "Yes"`  
denominator: sum of `MSF_symptom_* == "Yes" or "No"`

#### Comorbidity Prevelance

linelist variable: `ind_Comcond_*` (multiple columns)  
numerator: sum of `ind_Comcond_* == "Yes"`  
denominator: sum of `ind_Comcond_* == "Yes" or "No"`

## Common Filters

Prior to computing indicators, it is common to filter the data to either
confirmed, or confirmed, probable and suspect patients only. Please
refer to the analysis plan for details on what filters are used in each
table or graphic.

-   `ind_MSF_covid_status == "Confirmed"`
-   `ind_MSF_covid_status %in% c("Confirmed", "Probable", "Suspect")`

<br>
