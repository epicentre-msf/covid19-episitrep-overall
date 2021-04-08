# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# --- LOOPS to plot geofaceted trends for each continent 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- 


## Setup environment -------------------------------------------------------

# source(here::here('R',   'setup.R'),            encoding = 'UTF-8')
# source(file.path(path.R, "utils_management.R"), encoding = "UTF-8")
# source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")

# dates_and_week <- set_date_frame(create_folders = FALSE)
# date_min_report <- dates_and_week[[1]]
# date_max_report <- dates_and_week[[2]]
# week_report     <- dates_and_week[[3]]


library(tidyverse)
library(countrycode)
library(geofacet)
library(glue)
library(slider)
library(covidutils)


# Set paths
set_paths(path.local, week_report)
path.local.geofacet <- file.path(path.local.week, 'geofacet_plots')
path.local.geofacet.data <- file.path(path.local.geofacet, 'data')
path.local.geofacet.americas <- file.path(path.local.geofacet, 'Americas')
path.local.geofacet.asia     <- file.path(path.local.geofacet, 'Asia')
path.local.geofacet.europe   <- file.path(path.local.geofacet, 'Europe')
path.local.geofacet.africa   <- file.path(path.local.geofacet, 'Africa')

if(!exists(path.local.geofacet)) {
  dir.create(path.local.geofacet, showWarnings = FALSE, recursive = TRUE)
  dir.create(path.local.geofacet.data, showWarnings = FALSE, recursive = TRUE)
  dir.create(path.local.geofacet.americas, showWarnings = FALSE, recursive = TRUE)
  dir.create(path.local.geofacet.asia, showWarnings = FALSE, recursive = TRUE)
  dir.create(path.local.geofacet.europe, showWarnings = FALSE, recursive = TRUE)
  dir.create(path.local.geofacet.africa, showWarnings = FALSE, recursive = TRUE)
}


# Get & prepare data --------------------------------------------

## --- ECDC data
# still needed to get country population data
dta_ecdc <- covidutils::get_ecdc_data() %>%
  prepare_ecdc_geodata_geofacet() %>%
  filter(between(date, left = date_min_report, right = date_max_report)) %>%
  mutate(cases_per_100000   = cases/population_2019 * 1e5,
         deaths_per_million = deaths/population_2019 * 1e6) %>%
  pivot_longer(cols = c(cases, deaths, cases_per_100000,
                        deaths_per_million),
               names_to = "count",
               values_to = "value_raw") %>%
  group_by(code, count) %>%
  arrange(date) %>%
  mutate(value_ma = slide_dbl(value_raw,
                              mean,
                              .before = 1,
                              .after = 1)) %>%
  ungroup()




## --- JHU data
# For case and death data
dta_jhu  <- get_owid_jhcsse() %>% 
  prepare_jhu_geodata_geofacet() %>% 
  # filter(between(date, left = NULL, right = date_max_report)) %>% 
  left_join(dta_ecdc %>% 
              select(iso_a3, population_2019) %>% 
              distinct(),
            by = "iso_a3") %>% 
  mutate(cases_per_100000   = cases / population_2019 * 1e5, 
         deaths_per_million = deaths / population_2019 * 1e6) 



## --- GIS data - Mercator projection
sf_mercator <- rnaturalearth::ne_countries(type = "countries", 
                                           returnclass = "sf") %>% 
  tibble::as_tibble() %>% 
  sf::st_as_sf() %>% 
  dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  dplyr::mutate(
    continent = suppressWarnings(countrycode::countrycode(iso_a3, 
                                                          origin = "iso3c",
                                                          destination = "continent")),
    region = suppressWarnings(countrycode::countrycode(iso_a3, 
                                                       origin = "iso3c",
                                                       destination = "region23"))) %>% 
  dplyr::filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
  cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(., 
                                                            of_largest_polygon = TRUE))))  %>% 
  dplyr::rename(lon = X, lat = Y) 



# Generate grids --------------------------------------

grid_americas <- data.frame(
  name = c("Canada", "Bahamas", "Puerto Rico", "United States", "Dom. Rep.", "Haiti", "Cuba", "Mexico", "Jamaica", "Belize", "Guatemala", "Honduras", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Trinid. Tob.", "Venezuela", "Colombia", "Guyana", "Suriname", "Ecuador", "Brazil", "Peru", "Bolivia", "Paraguay", "Chile", "Uruguay", "Argentina", "Falkland Isl."),
  code = c("CA", "BS", "PR", "US", "DO", "HT", "CU", "MX", "JM", "BZ", "GT", "HN", "SV", "NI", "CR", "PA", "TT", "VE", "CO", "GY", "SR", "EC", "BR", "PE", "BO", "PY", "CL", "UY", "AR", "FK"),
  row = c(1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 9, 9, 10, 10, 10, 11),
  col = c(1, 6, 7, 1, 7, 6, 5, 1, 5, 3, 1, 2, 1, 2, 2, 3, 6, 5, 4, 6, 7, 4, 7, 4, 5, 6, 4, 6, 5, 5),
  stringsAsFactors = FALSE
)


grid_asia <- data.frame(
  name = c("Russia", "Dem. Rep. Korea", "Georgia", "Kyrgyzstan", "Kazakhstan", "Japan", "Republic of Korea", "Mongolia", "Cyprus", "Turkey", "Armenia", "Turkmenistan", "Uzbekistan", "Tajikistan", "Azerbaijan", "Taiwan", "China", "Syria", "Afghanistan", "Bhutan", "Nepal", "Iran", "Lebanon", "Iraq", "Kuwait", "Palestine", "Israel", "Pakistan", "Bangladesh", "Myanmar", "Lao PDR", "Jordan", "Qatar", "Saudi Arabia", "United Arab Emirates", "India", "Thailand", "Cambodia", "Vietnam", "Yemen", "Oman", "Sri Lanka", "Philippines", "Brunei Darussalam", "Malaysia", "Indonesia", "Timor-Leste"),
  code = c("RU", "KP", "GE", "KG", "KZ", "JP", "KR", "MN", "CY", "TR", "AM", "TM", "UZ", "TJ", "AZ", "TW", "CN", "SY", "AF", "BT", "NP", "IR", "LB", "IQ", "KW", "PS", "IL", "PK", "BD", "MM", "LA", "JO", "QA", "SA", "AE", "IN", "TH", "KH", "VN", "YE", "OM", "LK", "PH", "BN", "MY", "ID", "TL"),
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7),
  col = c(9, 13, 3, 7, 6, 14, 13, 11, 1, 2, 3, 5, 6, 7, 4, 13, 12, 3, 7, 9, 8, 6, 2, 4, 5, 2, 3, 7, 9, 10, 11, 4, 6, 5, 6, 8, 11, 12, 13, 5, 6, 9, 14, 13, 12, 13, 14),
  stringsAsFactors = FALSE
)


# Africa
africa_countries_grid1[africa_countries_grid1$code == "NAM", "code"] <- "NA"
grid_africa <- africa_countries_grid1


# Europe
grid_europe <- data.frame(
  name = c("Iceland", "Finland", "Norway", "Sweden", "Estonia", "Latvia", "Russian Federation", "Lithuania", "Poland", "Denmark", "Netherlands", "Belarus", "Belgium", "Ireland", "United Kingdom", "Germany", "Czech Republic", "Slovakia", "Ukraine", "Romania", "Hungary", "Austria", "France", "Luxembourg", "Switzerland", "Moldova", "Slovenia", "Italy", "Spain", "Portugal", "Croatia", "Serbia", "Bulgaria", "Montenegro", "Bosnia and Herzegovina", "Albania", "Macedonia", "Greece"),
  code = c("IS", "FI", "NO", "SE", "EE", "LV", "RU", "LT", "PL", "DK", "NL", "BY", "BE", "IE", "GB", "DE", "CZ", "SK", "UA", "RO", "HU", "AT", "FR", "LU", "CH", "MD", "SI", "IT", "ES", "PT", "HR", "RS", "BG", "ME", "BA", "AL", "MK", "GR"),
  row = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 8, 8, 9),
  col = c(1, 7, 5, 6, 10, 9, 11, 8, 6, 5, 4, 9, 4, 2, 3, 5, 6, 7, 9, 8, 7, 6, 3, 4, 5, 9, 6, 4, 2, 1, 7, 8, 9, 8, 7, 8, 7, 7),
  stringsAsFactors = FALSE
)


# Gather grids in list
list_grid <- list(grid_americas,
                  grid_asia,
                  grid_africa,
                  grid_europe)


# Labels ----------------------------------------------

# To use in figure titles
vec_names <- c("Americas",
               "Asia",
               "Africa",
               "Europe")

# To use in file names
vec_names_path <- c("Americas",
                    "Asia",
                    "Africa",
                    "Europe")


# Continent data ---------------------------------------
prepare_continent_data <- function(data) {
  
  dta_america <- data %>% 
    filter(continent == "Americas")
  
  dta_asia <- data %>% 
    filter(continent == "Asia" | country == "Russia")
  
  dta_africa <- data %>% 
    filter(continent == "Africa")
  
  dta_europe <- data %>% 
    filter(continent == "Europe")
  
  list_data <- list(dta_america, 
                    dta_asia,
                    dta_africa,
                    dta_europe)
  
  return(list_data)
  
}





# Plot JHU - 60 days ------------------------------------------------

# Gather everything in a dataframe
dta_60d <- tibble(names_paths  = vec_names_path,
                  continent = vec_names,
                  width     = c(13, 20, 12, 15),
                  height    = c(13, 8, 10, 10),
                  data      = prepare_continent_data(dta_jhu %>% 
                                                       filter(date >= lubridate::today() - 30)),
                  grid      = list_grid) 


# Make all plot
purrr::pmap(dta_60d,              # data
     geofacet_plot_all,    # function
     data_source = "JHU_60days",   # function options...
     nb_days = "60d",
     colour_raw = "#f04042")


# Plot JHU - all -------------------------------------------

# Data since the begining
# dta_all <- tibble(names_paths = vec_names_path,
#                   continent   = vec_names,
#                   width       = c(12, 20, 12, 25),
#                   height      = c(10, 8, 10, 10),
#                   data        = prepare_continent_data(dta_jhu),
#                   grid        = list_grid) 
# 
# pmap(dta_all,
#      geofacet_plot_all, 
#      data_source = "JHU",
#      colour_raw = "#f04042"
# )



# Save data -------------------------------------------
saveRDS(dta_60d,
        file.path(path.local.geofacet.data, paste0('geofacet_data', '_', week_report, '.RData')))







