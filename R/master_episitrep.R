
# -- Prepare environment

source(here::here('R', 'setup.R'), encoding = 'UTF-8')
source(file.path(path.R, "utils_get_data.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")
source(file.path(path.R, "set_time_frame.R")  , encoding = "UTF-8")


# -- Download geo data if not already present locally
get_geo_data(path = path.local.data, force = FALSE)



# === === === === === === === === 
# Run analysis
# === === === === === === === === 
update_analyes_worldwide <- TRUE


if (update_analyes_worldwide) {
  rmarkdown::render(input = file.path(path.Rmd, 'episitrep_worldwide_analyses.Rmd'), 
                    output_file = paste0(week_report, '_', 'episitrep_worldwide_analyses', '.html'), 
                    output_dir  = path.local.week) 
  } else {
    load(file.path(path.local.worldwide.data, paste0('episitrep_worldwide_analyses', '_', week_report, '.RData')))
}



update_analyes_msf_level <- TRUE

if (update_analyes_msf_level) {
  rmarkdown::render(input = file.path(path.Rmd, 'episitrep_msf_level_analyses.Rmd'), 
                    output_file = paste0(week_report, '_', 'episitrep_msf_level_analysis', '.html'),
                    output_dir  = path.local.week)
  } else {
    load(file.path(path.local.msf.data, paste0('episitrep_msf_level_analyses', '_', week_report, '.RData'))) 
}



# === === === === === === === ===  
# Edit docx file
# === === === === === === === === 

my_doc <- read_docx(file.path(path.templates, 'template_EpiSitrep_world_Covid-19.docx'))
source(file.path(path.R, "utils_vis.R")    , encoding = "UTF-8")
source(file.path(path.R, 'utils_officer.R'), encoding = 'UTF-8')

#styles_info(my_doc)


# Doc title
source(file.path(path.R, 'docx_section0_heading.R'), encoding = 'UTF-8')

# I. Worldwide analyses
source(file.path(path.R, 'docx_section1_worldwide_analyses.R'), encoding = 'UTF-8')

# II. MSF level analyses
source(file.path(path.R, 'docx_section2_MSF_level_analyses.R'), encoding = 'UTF-8')


# === === === === === === === === 
# Save docx file
# === === === === === === === === 
print(my_doc, target = file.path(path.local.week, glue("draft_EpiSitrep_world_Covid-19_{week_report}.docx")))
  
