library(purrr)
library(readxl)


# Note: 'path.sharepoint' is set in setup.R
path.sharepoint.agg.data <- file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all.xlsx")

agg_data_names <- c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown")

df_weekly_aggregated <- excel_sheets(path.sharepoint.agg.data) %>% 
  map_df(~{
    oc <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
    country <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
    project <- read_excel(path = path.sharepoint.agg.data, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
    
    read_excel(path = path.sharepoint.agg.data, sheet = .x, skip = 5, col_names = FALSE) %>% 
      mutate(sheet = .x, oc = oc, country = country, project = project)
  }) %>% 
  select(sheet, oc, country, project, 1:7) %>% 
  set_names(agg_data_names)

