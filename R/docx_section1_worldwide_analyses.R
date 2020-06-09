
# I. Worldwide analysis - Heading
# --- --- --- --- --- --- --- --- 
my_doc <- add_heading1(heading_text = 'Worldwide analysis') 


# COUNT CASES
# --- --- --- --- 

## - Heading 
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 cases') 
my_doc <- add_end_section_continuous()

## - Text 
my_doc <- add_par_normal(txt_world_cases$par1)
my_doc <- add_par_normal(txt_world_cases$par2)
my_doc <- add_par_normal(txt_world_cases$par3)
my_doc <- add_par_normal(txt_world_cases$par4)
my_doc <- add_par_normal(txt_world_cases$par5)
my_doc <- add_end_section_2columns()

## - Map
my_doc <- add_figure_map_world_grid(object_name  = glue("map_world_cases_grid_{week_report}.png"),
                                    figure_title = glue('Mapping of number of Covid-19 cases and cases trends estimated during the period from {format(msf_date_max - 11, "%d %B %Y")} to {format(msf_date_max, "%d %B %Y")} (12 days)'))
my_doc <- add_end_section_continuous()


my_doc <- add_par_normal(txt_world_cases$par6)
my_doc <- add_par_normal(txt_world_cases$par7)

my_doc <- add_figure_map_world(object_name  = glue("map_world_cases_attack_rates_{week_report}.png"), 
                               figure_title = glue('Cumulative incidence of Covid-19 reported cases since beginning of epidemic, per 100,000 population'))

my_doc <- add_end_section_2columns(widths = c(7 * cm_to_in, 10 * cm_to_in))



# COUNT DEATHS
# --- --- --- --- 

## - Heading
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 associated deaths') 
my_doc <- add_end_section_continuous()

## - Text
my_doc <- add_par_normal(txt_world_deaths$par1)
my_doc <- add_par_normal(txt_world_deaths$par2)
my_doc <- add_par_normal(txt_world_deaths$par3)
my_doc <- my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = 'The full table of cases, deaths and trends can be found ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://msfintl-my.sharepoint.com/:f:/g/personal/francesco_grandesso_epicentre_msf_org/Ek1jIZ1ghe5GqibjBM3GnpsBJyTF7QBYmFY6bEhgSGVHAA?e=bewh3q/") %>% 
  slip_in_text(style = 'Normal char', 
               str = ".") 
my_doc <- add_end_section_2columns()

## - Map
my_doc <- add_figure_map_world_grid(object_name  = glue('map_world_deaths_grid_{week_report}.png'),
                                    figure_title = glue('Mapping of number of Covid-19 associated deaths and deaths trends estimated during the period from {format(msf_date_max - 11, "%d %B %Y")} to {format(msf_date_max, "%d %B %Y")} (12 days)'))



# DOUBLING TIME
# --- --- --- --- 

## - Heading
my_doc <- add_heading2(heading_text = 'Doubling time in cases and deaths') 
my_doc <- add_end_section_continuous()

## - Text
my_doc <- add_par_normal(txt_world_doubling$par1)
my_doc <- add_par_normal(txt_world_doubling$par2)
my_doc <- add_par_normal(txt_world_doubling$par3)
my_doc <- add_end_section_2columns()

## - Map
my_doc <- add_figure_map_world_grid(object_name  = glue('map_world_doubling_grid_{week_report}.png'),
                                    figure_title = glue('Doubling time of the number of Covid-19 cases and associated deaths estimated in the last 12 days (only countries with increasing trends are displayed)'))

my_doc <- add_par_normal('')
## - Table
my_doc <- add_table(table_title = glue('Countries with estimated cases or deaths doubling time of less than {threshold_doubling_time} days'), 
                    object_name = glue("gtbl_cfr_rank_doubling_{week_report}.png"), 
                    width = 12.7 * cm_to_in, 
                    height = 9.93 * cm_to_in)

my_doc <- add_end_section_continuous()

my_doc <- add_par_normal(txt_world_doubling$par4)
my_doc <- add_par_normal(txt_world_doubling$par5)
my_doc <- add_end_section_2columns()


my_doc <- add_figure_dot_plot(object_name  = glue('dots_hotspot_cases_{week_report}.png'),
                                    figure_title = glue('Distribution of doubling time of Covid-19 cases according to length in days since the first significant increasing trend reported (only countries with doubling time of less than {threshold_doubling_time} days as of {format(msf_date_max, "%d %B %Y")} are displayed)'))

my_doc <- add_end_section_continuous()

my_doc <- add_par_normal(txt_world_doubling$par6)
my_doc <- add_end_section_2columns()

my_doc <- add_figure_dot_plot(object_name  = glue('dots_ratio_tests_{week_report}.png'),
                              figure_title = glue('Relation between the number of tests carried out in the last 12 days (as ratio of the number of confirmed cases) and the cumulative number of cases reported'))


