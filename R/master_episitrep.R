# === === === === === === === === 
# ---- Prepare environment ----
# === === === === === === === === 

# Set locale on French computer
if (Sys.getlocale(category = "LC_TIME") == "French_France.1252") {
  Sys.setlocale(category = "LC_ALL", locale = "en_GB.UTF-8")
  Sys.setenv(LANG = "en_GB.UTF-8") 
}

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


# --- Download geo data if not already present locally
get_geo_data(path = path.local.data, force = FALSE)


# === === === === === === === === 
# ---- Run analyses ----
# === === === === === === === === 

# World
file_out_worldwide <- paste0(week_report, '_', 'episitrep_worldwide_analyses', '.html')

rmarkdown::render(
  input = file.path(path.Rmd, 'episitrep_worldwide_analyses.Rmd'), 
  output_file = file_out_worldwide, 
  output_dir  = path.local.week)


# MSF data
file_out_msf <- paste0(week_report, '_', 'episitrep_msf_level_analysis', '.html')

rmarkdown::render(
  input = file.path(path.Rmd, 'episitrep_msf_level_analyses.Rmd'), 
  output_file = file_out_msf,
  output_dir  = path.local.week)


# Sections
oc_list <- list("OCP", "OCA", "OCB", "OCBA", "OCG")
purrr::walk(oc_list, 
            ~rmarkdown::render(
              input       = file.path(path.Rmd, 'episitrep_msf_oc_level_analyses.Rmd'), 
              output_file = glue::glue("{week_report}_episitrep_msf_oc_level_analysis_{.}.html"),
              output_dir  = path.local.week.oc,
              params = list(OC = .)
              )
            )
            


# === === === === === === === ===  ===  
# ---- Edit and save docx file ----
# === === === === === === === ===  === 

load(file.path(path.local.worldwide.data, paste0('episitrep_worldwide_analyses', '_', week_report, '.RData')))
load(file.path(path.local.msf.data, paste0('episitrep_msf_level_analyses', '_', week_report, '.RData'))) 


my_doc <- read_docx(file.path(path.templates, 'template_EpiSitrep_world_Covid-19.docx'))
source(file.path(path.R, "utils_vis.R")    , encoding = "UTF-8")
source(file.path(path.R, 'utils_officer.R'), encoding = 'UTF-8')

#styles_info(my_doc)

# --- 0. Heading
source(file.path(path.R, 'docx_section0_heading.R'), encoding = 'UTF-8')

# ---- 1. Worldwide analyses
source(file.path(path.R, 'docx_section1_worldwide_analyses.R'), encoding = 'UTF-8')

# --- 2. MSF level analyses
source(file.path(path.R, 'docx_section2_MSF_level_analyses.R'), encoding = 'UTF-8')

# --- Save docx file
print(my_doc, target = file.path(path.local.week, glue("draft_EpiSitrep_world_Covid-19_{week_report}.docx")))



# === === === === === === 
# ---- Run Deepdives ----
# === === === === === ===  
file_out_deepdive_africa <- paste0(week_report, '_', 'deepdive_Africa', '.html')

rmarkdown::render(
  input = file.path(path.Rmd, 'deep_dive_Africa.Rmd'),
  output_file = file_out_deepdive_africa,
  output_dir  = path.local.week)


# Simplified deepdive for all continents
continent_list <- list("Africa", "Americas", "Asia", "Europe")

purrr::walk(continent_list, 
            ~rmarkdown::render(
              input       = file.path(path.Rmd, 'deep_dive.Rmd'), 
              output_file = glue::glue("{week_report}_deepdive_{.}.html"),
              output_dir  = path.local.week.deepdive,
              params = list(continent = .))
)





# === === === === === === === === === ===
# ---- Plots continent & countries ----
# === === === === === === === === === === 

# Note: the scripts run in a new environment, child to this one so as to 
# not interfere with each other.
source(here::here('R', 'run_geofacet_plots.R'), encoding = 'UTF-8',
       local = new.env(parent = .GlobalEnv))

source(here::here('R', 'run_multiplot_world_continent.R'), 
       encoding = 'UTF-8', local = new.env(parent = .GlobalEnv))


source(here::here('R', 'run_multiplot_country.R'), encoding = 'UTF-8',
       local = new.env(parent = .GlobalEnv))




# === === === === === === === === === === ===
# ---- Copy outputs to various locations ----
# === === === === === === === === === === ===

# GIS unit sharepoint --------------------------------------

## Copy table of trends to GIS Unit sharepoint
file.copy(
  from = file.path(path.local.week, 'worldwide', 'tables', paste0('epi-case-trends-', week_report, '.csv')),
  to = file.path(sharepoint.parent.dir, 'MSF', 'GIS @ MSF - EPI csv data'), 
  overwrite = TRUE)



# Public folder ---------------------------------------


###### Worldwide analyses ######
## Copy to archive
file.copy(
  from = file.path(path.local.week, file_out_worldwide),
  to = file.path(path.sharepoint.public, "addtional_episitrep_outputs_worldwide", "archive"),
  overwrite = TRUE
)
## overwrite index
file.copy(
  from = file.path(path.local.week, file_out_worldwide),
  to = file.path(path.sharepoint.public, "addtional_episitrep_outputs_worldwide", "index.html"),
  overwrite = TRUE
)

###### Worldwide tables summary ######
## Copy to archive
file.copy(
  from = file.path(path.local.worldwide.tables, paste0(week_report, '_', 'world_summary_cases_deaths.html')), 
  to = file.path(path.sharepoint.public, "tables_world_summary", "archive"),
  overwrite = TRUE
)
## overwrite index
file.copy(
  from = file.path(path.local.worldwide.tables, paste0(week_report, '_', 'world_summary_cases_deaths.html')), 
  to = file.path(path.sharepoint.public, "tables_world_summary", "index.html"),
  overwrite = TRUE
)


###### MSF data analysis ######

## Copy to archive
file.copy(
  from = file.path(path.local.week, file_out_msf),
  to = file.path(path.sharepoint.public, "additional_episitrep_outputs_msf", "archive"),
  overwrite = TRUE
)
## overwrite index
file.copy(
  from = file.path(path.local.week, file_out_msf),
  to = file.path(path.sharepoint.public, "additional_episitrep_outputs_msf", "index.html"),
  overwrite = TRUE
)

###### MSF summary table ######
## Copy to archive
file.copy(
  from = file.path(path.local.msf.tables, paste0(week_report, '_', 'SUMMARY-TABLE_MSF-sites_by_patients_Covid-status.html')), 
  to = file.path(path.sharepoint.public, "tables_msf_sites", "archive"),
  overwrite = TRUE
)
## overwrite index
file.copy(
  from = file.path(path.local.msf.tables, paste0(week_report, '_', 'SUMMARY-TABLE_MSF-sites_by_patients_Covid-status.html')), 
  to = file.path(path.sharepoint.public, "tables_msf_sites", "index.html"),
  overwrite = TRUE
)


###### OC ######
## Copy to archive
purrr::walk(oc_list, 
            ~ file.copy(
              from = file.path(path.local.week.oc,
                               glue::glue("{week_report}_episitrep_msf_oc_level_analysis_{.}.html")), 
              
              to = file.path(path.sharepoint.public, "additional_episitrep_outputs_oc", "archive"),
              overwrite = TRUE))


## overwrite index
purrr::walk(oc_list, 
            ~ file.copy(
              from = file.path(path.local.week.oc,
                               glue::glue("{week_report}_episitrep_msf_oc_level_analysis_{.}.html")), 
              
              to = file.path(path.sharepoint.public, "additional_episitrep_outputs_oc", ., "index.html"),
              overwrite = TRUE))



# Non public folder ---------------------------------------

# path.sharepoint.sitrep.week <- file.path(path.sharepoint.sitrep,
#                                     week_report)
# 
# if (!exists(path.sharepoint.sitrep.week)){
#   dir.create(path.sharepoint.sitrep.week, showWarnings = FALSE, recursive = TRUE)
# }

# Copy the whole local X week directory in the sharepoint.
file.copy(path.local.week, 
          path.sharepoint.sitrep, 
          recursive = TRUE)


  
  

  