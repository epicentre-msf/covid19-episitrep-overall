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

dta_south_america <- dta_ecdc %>% 
  filter(continent == "Americas",
         region != "North America")


dta_south_east_asia <- dta_ecdc %>% 
  filter(continent == "Asia",
         region != "East Asia & Pacific")


dta_africa <- dta_ecdc %>% 
  filter(continent == "Africa")




# Generate grids --------------------------------------


# Amérique du Sud et centrale
shp_south_america <- sf_mercator %>%
  filter(continent == "Americas",
         region != "Northern America")

grid_south_america <- grid_auto(shp_south_america, 
                                names = "country",
                                codes = "iso_a2",
                                seed = 1) %>% 
  select(name = name_country, code = code_iso_a2, row, col)



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







### Plot




list_data <- list(dta_south_america, 
                  dta_south_east_asia,
                  dta_africa)

list_names <- list("South & Central America",
                   "South & East Asia",
                   "Africa")

list_names_path <- list("south_central_america",
                        "south_east_asia",
                        "africa")

list_grid <- list(grid_south_america,
                  grid_south_east_asia,
                  grid_africa)

pmap(list(list_data,
          list_names,
          list_grid,
          list_names_path),
     ~ {geofacet_plot_all(my_data = ..1,
                          my_continent = ..2,
                          my_grid = ..3,
                          my_names_path = ..4
                          )
       }, 
     my_width = 12, 
     my_height = 10
)







