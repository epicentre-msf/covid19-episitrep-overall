
# -- Prepare environment

source(here::here('R', 'setup.R'), encoding = 'UTF-8')
source(file.path(path.R, "utils_get_data.R")  , encoding = "UTF-8")
source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")


# Set the left and right censoring for date of consultation. 
# The right-censoring create also the week value and the folders where to save the outputs
dates_and_week <- set_date_frame(create_folders = TRUE)

date_min_report <- dates_and_week[[1]]
date_max_report <- dates_and_week[[2]]
week_report     <- dates_and_week[[3]]


# -- Download geo data if not already present locally
get_geo_data(path = path.local.data, force = FALSE)



# === === === === === === === === 
# Run analysis
# === === === === === === === === 
update_analyes_worldwide <- TRUE

if (update_analyes_worldwide) {
  file_out_worldwide <- paste0(week_report, '_', 'episitrep_worldwide_analyses', '.html')
  rmarkdown::render(
    input = file.path(path.Rmd, 'episitrep_worldwide_analyses.Rmd'), 
    output_file = file_out_worldwide, 
    output_dir  = path.local.week
  ) 
  # copy public sharepoint archive
  file.copy(
    from = file.path(path.local.week, file_out_worldwide),
    to = file.path(path.sharepoint.public, "addtional_episitrep_outputs_worldwide", "older_outputs"),
    overwrite = TRUE
  )
  # overwrite as index html file in public sharepoint
  file.copy(
    from = file.path(path.local.week, file_out_worldwide),
    to = file.path(path.sharepoint.public, "addtional_episitrep_outputs_worldwide", "index.html"),
    overwrite = TRUE
  )
  } else {
    load(file.path(path.local.worldwide.data, paste0('episitrep_worldwide_analyses', '_', week_report, '.RData')))
}



update_analyes_msf_level <- TRUE

if (update_analyes_msf_level) {
  file_out_msf <- paste0(week_report, '_', 'episitrep_msf_level_analysis', '.html')
  # render to local folder
  rmarkdown::render(
    input = file.path(path.Rmd, 'episitrep_msf_level_analyses.Rmd'), 
    output_file = file_out_msf,
    output_dir  = path.local.week
  )
  # copy public sharepoint archive
  file.copy(
    from = file.path(path.local.week, file_out_msf),
    to = file.path(path.sharepoint.public, "additional_episitrep_outputs_msf", "older_outputs"),
    overwrite = TRUE
  )
  # overwrite as index html file in public sharepoint
  file.copy(
    from = file.path(path.local.week, file_out_msf),
    to = file.path(path.sharepoint.public, "additional_episitrep_outputs_msf", "index.html"),
    overwrite = TRUE
  )
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
  
