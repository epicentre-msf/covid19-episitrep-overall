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
path.local.geofacet <- file.path(path.local.worldwide.graphs,
                                 'geofacet_plots')
if(!exists(path.local.geofacet)) {
  dir.create(path.local.geofacet, showWarnings = FALSE, recursive = TRUE)
}



# Get & prepare data --------------------------------------------

## --- ECDC data
dta_ecdc <- covidutils::get_ecdc_data()


# Prepare data
dta_ecdc <- dta_ecdc %>% 
  prepare_ecdc_data_geofacet() %>% 
  drop_na(iso_a3) %>% 
  filter(between(date, left = NULL, right = date_max_report)) %>% 
  
  rename(code = geoid) %>% 
  mutate(cases_per_100000   = cases/population_2019*1e5, 
         deaths_per_million = deaths/population_2019*1e6) %>% 
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




# Continent data ---------------------------------------

dta_south_central_america <- dta_ecdc %>% 
  filter(continent == "Americas",
         region != "North America") %>% 
  filter(country != "Mexico")


dta_south_east_asia <- dta_ecdc %>% 
  filter(continent == "Asia",
         region != "East Asia & Pacific")


dta_africa <- dta_ecdc %>% 
  filter(continent == "Africa")

dta_europe <- dta_ecdc %>% 
  filter(continent == "Europe")




# Generate grids --------------------------------------


# Amérique du Sud et centrale
shp_south_central_america <- sf_mercator %>%
  filter(continent == "Americas",
         region != "Northern America")

grid_south_central_america <- grid_auto(shp_south_central_america, 
                                names = "country",
                                codes = "iso_a2",
                                seed = 1) %>% 
  select(name = name_country, code = code_iso_a2, row, col)



grid_south_central_america <- data.frame(
  name = c("Bahamas", "Puerto Rico", "Dom. Rep.", "Haiti", "Cuba", "Mexico", "Jamaica", "Belize", "Guatemala", "Honduras", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Trinid. & Tob.", "Venezuela", "Colombia", "Guyana", "Suriname", "Ecuador", "Brazil", "Peru", "Bolivia", "Paraguay", "Chile", "Uruguay", "Argentina", "Falkland Isl."),
  code = c("BS", "PR", "DO", "HT", "CU", "MX", "JM", "BZ", "GT", "HN", "SV", "NI", "CR", "PA", "TT", "VE", "CO", "GY", "SR", "EC", "BR", "PE", "BO", "PY", "CL", "UY", "AR", "FK"),
  row = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 8, 8, 8, 9, 9, 9, 10),
  col = c(6, 7, 7, 6, 5, 1, 5, 3, 1, 2, 1, 2, 2, 3, 6, 5, 4, 6, 7, 4, 7, 4, 5, 6, 4, 6, 5, 5),
  stringsAsFactors = FALSE
) %>% 
  filter(name != "Mexico")


# Asie du sud
shp_south_east_asia <- sf_mercator %>%
  filter(continent == "Asia",
         region != "East Asia & Pacific")

grid_south_east_asia <- grid_auto(shp_south_east_asia, 
                                  names = "country",
                                  codes = "iso_a2",
                                  seed = 1) %>% 
  select(name = name_country, code = code_iso_a2, row, col)


# Africa
africa_countries_grid1[africa_countries_grid1$code == "NAM", "code"] <- "NA"
grid_africa <- africa_countries_grid1


# Europe
shp_europe <- sf_mercator %>%
  filter(continent == "Europe")

grid_europe <- grid_auto(shp_europe, 
                                  names = "country",
                                  codes = "iso_a2",
                                  seed = 1) %>% 
  select(name = name_country, code = code_iso_a2, row, col)



### Plot

list_data <- list(dta_south_central_america, 
                  dta_south_east_asia,
                  dta_africa,
                  dta_europe)

list_names <- list("South & Central America",
                   "South & East Asia",
                   "Africa",
                   "Europe")

list_names_path <- list("south_central_america",
                        "south_east_asia",
                        "africa",
                        "europe")

list_grid <- list(grid_south_central_america,
                  grid_south_east_asia,
                  grid_africa,
                  grid_europe)

pmap(list(list_data,
          list_names,
          list_grid,
          list_names_path),
<<<<<<< Updated upstream
     ~ {geofacet_plot_all(my_data = ..1,
                          my_continent = ..2,
                          my_grid = ..3,
                          my_names_path = ..4
                          )
       }, 
     my_width = 12, 
     my_height = 10
=======
     ~ {geofacet_plot_all(data = ..1,
                          continent = ..2,
                          grid = ..3,
                          names_path = ..4,
                          data_source = "ECDC",
                          colour_raw = "#f04042")}, 
     width = 12, 
     height = 10
>>>>>>> Stashed changes
)







