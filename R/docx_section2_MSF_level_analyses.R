
# II. MSF level analyses - Heading
# --- --- --- --- --- --- --- --- 

my_doc <- add_heading1('Analysis at MSF level') 


# WHAT WE EXPECT
# --- --- --- --- --- --- 

##  What we expect - Heading
my_doc <- add_heading2('What will this section present')

my_doc %<>% 
  body_add_fpar(style = 'Description', 
                fpar(ftext('This section of the report analyses data reported by MSF in Excel linelist created by Epicentre and distributed to MSF projects. OCs are highly encouraged to send on a weekly basis by Tuesdays nights the updated linelist exports to the following email address (EPI-COVID19-DATA@epicentre.msf.org) (please respect your OC guidelines by discussing with identified surveillance focal point). Country-specific reports may be produced upon request from MSF operation centres.', 
                           prop = calibri_8)))

# MSF LEVEL ANALYSIS
# --- --- --- --- --- --- 

## MSF analysis - Heading 

my_doc <- add_heading2('Analysis of the updated MSF linelists')

my_doc <- add_end_section_continuous()

## MSF analysis - Text

my_doc <- add_par_normal(txt_msf_general$par1)
my_doc <- add_par_normal(txt_msf_general$par2)
my_doc <- add_par_normal(txt_msf_general$par3)
my_doc <- add_par_normal(txt_msf_general$par4)


my_doc <- add_end_section_2columns()

my_doc <- add_par_normal('')
my_doc <- add_table_msf(table_title = 'Summary of Covid-19 infection-related status of patients consulted/admitted', 
                    object_name = glue('gtbl_countries_covid_status_{week_report}.png'), 
                    width = 16.44 * cm_to_in, 
                    height = 11.29 * cm_to_in)

my_doc <- add_par_normal('')
my_doc <- add_table_msf(table_title = 'Characteristics of patients admitted to the MSF supported facilities', 
                    object_name = glue('gtbl_general_{week_report}.png'), 
                    width = 11.72 * cm_to_in, 
                    height = 6.49 * cm_to_in)

my_doc <- add_par_normal('')
my_doc <- add_table_msf(table_title = 'Distribution of patient outcomes in MSF facilities, among confirmed, probable and suspected cases ', 
                    object_name = glue('gtbl_outcome_{week_report}.png'), 
                    width = 14.99 * cm_to_in, 
                    height = 6.51 * cm_to_in)

my_doc <- add_par_normal('')
my_doc <- add_table_msf(table_title = 'Frequency and percentage of signs and symptoms of patients admitted', 
                    object_name = glue('gtbl_sympt_{week_report}.png'), 
                    width = 9.47 * cm_to_in, 
                    height = 24.73 * cm_to_in)

