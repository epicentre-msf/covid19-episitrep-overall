#' Import world shapefile as sf object
#' 
#' @param scale detail on map polygons, one of 'small', 'medium', 'large'
#' @param proj projection of map, one of 'robinson' (default) or 'mercator'
#' 
#' @export
get_world_sf <- function(scale = c('small', 'medium', 'large'), proj = c('robinson', 'mercator')) {
  
  library(sf)
  library(dplyr)
  
  scale <- match.arg(scale, several.ok = FALSE)
  proj <- match.arg(proj, several.ok = FALSE)
  
  world_map_raw <- rnaturalearth::ne_countries(scale = scale, type = "countries", returnclass = "sf")
  
  if (proj == "robinson") 
    world_map_raw <- world_map_raw %>% sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84") 
  
  world_map_raw %>% 
    tibble::as_tibble() %>% # for nicer printing
    sf::st_as_sf() %>% 
    dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
    dplyr::mutate(
      continent = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent")),
      region = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"))
    ) %>% 
    dplyr::filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(., of_largest_polygon = TRUE)))) %>% 
    dplyr::rename(lon = X, lat = Y)
}


#' Import ECDC dataset
#' 
#' @export
get_ecdc_data <- function() {
  
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  
  d <- readr::read_csv(base_url) %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y") -1) %>%
    dplyr::rename(geoid = geoId, country_ecdc = countriesAndTerritories, iso_a3 = countryterritoryCode, population_2019 = popData2019) %>%
    dplyr::select(-dateRep) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # Complete missing infos
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country == "Congo - Brazzaville" ~ "Republic of Congo", 
        country_ecdc == 'Cases_on_an_international_conveyance_Japan' ~ 'Cruise Ship', 
        is.na(country) ~ gsub('_', ' ', country_ecdc), 
        TRUE ~ country), 
      iso_a3 = dplyr::case_when(
        country == 'Kosovo' ~ 'XKX', 
        country == 'Anguilla' ~ 'AIA', 
        country == 'Bonaire, Saint Eustatius and Saba' ~ 'BES', 
        country == 'Falkland Islands (Malvinas)' ~ 'FLK', 
        country == 'Montserrat' ~ 'MSR',  
        country == 'Taiwan' ~ 'TWN', 
        country == 'Western Sahara' ~ 'ESH', 
        country == 'Cruise Ship' ~ NA_character_, 
        TRUE ~ iso_a3), 
      continent = countrycode::countrycode(iso_a3, origin = 'iso3c', destination = 'continent'), 
      continent = dplyr::case_when(
        country == 'Kosovo' ~ 'Europe', 
        country == 'Cruise Ship' ~ 'Undefined', 
        is.na(country) ~ "Unknown", 
        TRUE ~ continent), 
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"), 
      region = dplyr::case_when(
        country == 'Kosovo' ~ 'Southern Europe', 
        country == 'Cruise Ship' ~ 'Undefined', 
        is.na(country) ~ "Unknown", 
        TRUE ~ region), 
      source = "ECDC"
    ) %>% 
    dplyr::select(date, country_ecdc:geoid, country:region, iso_a3, cases, deaths, population_2019, source)
  
}


