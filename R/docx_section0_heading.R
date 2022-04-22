# --- --- --- --- --- --- --- --- 
# TITLE
# --- --- --- --- --- --- --- --- 

my_doc %<>% 
  body_add_par(style = 'Title', 
               value = "MSF EpiSitrep on Covid-19 epidemic",
               pos = "before") %>% 
  body_add_par(style = 'Subtitle', 
               value = glue('Week {lubridate::week(date_max_report)-1}-{lubridate::year(date_max_report)}')) 



# --- --- --- --- --- --- --- --- 
# PARAGRAPH
# --- --- --- --- --- --- --- --- 

my_doc %<>% 
  body_add_par(style = 'Description bold', 
               value = "This is a monthly report that describes the evolution of the current Covid-19 epidemic worldwide and in MSF projects. It intends to support MSF in their Covid-19 intervention projects. Worldwide graphics and tables are also available at the Epicentre ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "COVID-19 Epi Dashboard", 
               hyperlink = "https://reports.msf.net/public/covid19/") %>% 
  slip_in_text(style = 'Description char', 
               str = ". Information on data collected through MSF linelists is also available on the ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "MSF COVID-19 Dashboard ", 
               hyperlink = "https://reports.msf.net/secure/app/covid19-linelist-dashboard/") %>% 
  slip_in_text(style = 'Description char', 
               str ="(sign-up ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/signup/") %>% 
  slip_in_text(style = 'Description char', 
               str = ').') %>% 
  slip_in_text(style = "Description bold orange Car", 
               str = " All additional routine analyses on MSF data are available ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis") %>% 
  slip_in_text(style = 'Description char', 
               str = '.') %>% 
  slip_in_text(style = 'Description char', 
               str = " This report was edited by Epicentre. For any request or query, please contact Laura Wright (") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "laura.wright@epicentre.msf.org", 
               hyperlink = "mailto:laura.wright@epicentre.msf.org") %>% 
  slip_in_text(style = 'Description char', 
               str = ').') %>% 
  
  body_add_par(style = 'Horizontal line', 
               value = '') %>% 
  body_end_section_continuous()


# --- --- --- --- --- --- --- --- 
# PARAGRAPHS (3 columns)
# --- --- --- --- --- --- --- --- 

my_doc %<>% 
  body_add_par(style = 'Description bold', 
               value = "Data sources") %>% 
  
  body_add_par(style = 'Description bullet', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "JHU CSSE data", 
               hyperlink = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv") %>% 
  slip_in_text(style = 'Description char', 
               str = glue(" last updated {format(max(dta_jhu$date, na.rm = TRUE), '%d %b %Y')}")) %>% 
  
  body_add_par(style = 'Description bullet', 
               value = "MSF linelists compiled by Epicentre") %>% 
  
  body_add_par(style = 'Description bullet', 
               value = "MSF ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "GIS Unit", 
               hyperlink = "https://mapcentre.msf.org/") %>% 
  slip_in_text(style = 'Description char', 
               str = glue(" (baseline country maps)")) %>% 
  
  body_add_par(style = 'Description bullet', 
               value = "FIND ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "SARS-COV-2 Test Tracker", 
               hyperlink = "https://www.finddx.org/covid-19/test-tracker/") %>% 
  slip_in_text(style = 'Description char', 
               str = glue(" for data on Covid-19 tests"))




my_doc %<>%   
  body_add_par(style = 'Description bold', 
               value = "Definitions and analysis methods") %>% 
  
  slip_in_column_break(pos = 'before') %>% 
  
  body_add_par(style = 'Description', 
               value = "Definitions of increasing, declining and stable trends and the definition of doubling time, as well as detailed information on the analysis methods can be found ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/analysis_methods_2020-08-20.html")




my_doc %<>%  
  body_add_par(style = 'Description bold', 
               value = "Useful links") %>% 
  
  slip_in_column_break(pos = 'before') %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "MSF Covid-19 Information Hub", 
               hyperlink = "https://msfintl.sharepoint.com/sites/msfintlcommunities/Covid-19/SitePages/Home.aspx") %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "Literature review by INSERM", 
               hyperlink = "https://reacting.inserm.fr/literature-review/") %>% 
  
  body_add_par(style = 'Description', 
               value = "") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "Epicentre Covid19 blog", 
               hyperlink = "https://msfintl.sharepoint.com/sites/grp-epi-proj-ncov/SitePages/About.aspx")


my_doc <- add_end_section_2columns()


# --- --- --- --- --- --- --- --- 
# PARAGRAPH
# --- --- --- --- --- --- --- --- 


calibri_8 <- fp_text(font.family = "Calibri", 
                     font.size = 8)

calibri_8_bold <- update(calibri_8, bold = TRUE)



my_doc %<>% 
  body_add_fpar(style = 'Description', 
                fpar(ftext("IMPORTANT NOTE: ", 
                           prop = calibri_8_bold), 
                     ftext("Data and results presented here are possibly affected by bias related with factors such as the testing strategies used by each country and the performance of their surveillance systems. Results would be better interpreted in the light of this information.", 
                           prop = calibri_8))) %>% 
  body_add_par(style = 'Horizontal line', 
               value = '') %>% 
  
  body_end_section_continuous()
