library(purrr)
library(readxl)


# Note: 'path.sharepoint' is set in setup.R
path.sharepoint.agg.data <- file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx")

# Get weekly aggregated data
agg_data_names <- c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown")

dta_weekly_aggregated <- excel_sheets(path.sharepoint.agg.data) %>% 
  map_df(~{
    oc      <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
    country <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
    project <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
    
    read_excel(path = path.sharepoint.agg.data, sheet = .x, skip = 5, col_names = FALSE) %>% 
      mutate(sheet = .x, oc = oc, country = country, project = project)
  }) %>% 
  select(sheet, oc, country, project, 1:7) %>% 
  set_names(agg_data_names)


# Get first and last activity dates by project
dta_project_aggregated_dates <- excel_sheets(path.sharepoint.agg.data) %>%
 map_df(~{
   
   if (is_empty(read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "I1", col_names = FALSE))) {
     date_first <- NA
   } else {
     date_first <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "I1", col_names = FALSE) %>% pull()
     date_first <- lubridate::as_date(date_first)
   }
   
   if (is_empty(read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "K1", col_names = FALSE))) {
     date_last  <- NA
   } else {
     date_last <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "K1", col_names = FALSE) %>% pull()
     date_last <- lubridate::as_date(date_last)
   }
   
   tibble::tibble(sheet = .x, date_first = date_first, date_last = date_last)
 })
