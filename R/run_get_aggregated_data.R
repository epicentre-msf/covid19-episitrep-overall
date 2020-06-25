library(tidyverse)
library(readxl)

OS <- Sys.info()[['sysname']]
sharepoint_root_dir <- dplyr::case_when(OS == "Windows" ~ "D:", OS == "Darwin" ~ "~")
agg_data_path <- file.path(sharepoint_root_dir, 'MSF', 'GRP-EPI-COVID-19 - NCoVEpi', "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all.xlsx")

agg_data_names <- c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown")

df_aggregated <- excel_sheets(agg_data_path) %>% 
  map_df(~{
    oc <- read_excel(path = agg_data_path, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
    country <- read_excel(path = agg_data_path, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
    project <- read_excel(path = agg_data_path, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
    
    read_excel(path = agg_data_path, sheet = .x, skip = 5, col_names = FALSE) %>% 
      mutate(sheet = .x, oc = oc, country = country, project = project)
  }) %>% 
  select(sheet, oc, country, project, 1:7) %>% 
  set_names(agg_data_names)

