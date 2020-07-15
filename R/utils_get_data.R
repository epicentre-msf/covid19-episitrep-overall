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
  proj  <- match.arg(proj, several.ok = FALSE)
  
  world_map_raw <- rnaturalearth::ne_countries(scale = scale, type = "countries", returnclass = "sf")
  
  if (proj == "robinson") 
    world_map_raw <- world_map_raw %>% sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84") 
  
  world_map_raw %>% 
    tibble::as_tibble() %>% # for nicer printing
    sf::st_as_sf() %>% 
    dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
    dplyr::mutate(
      continent = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent")),
      region = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"))
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
  
  d <- readr::read_csv(base_url)
  
}



#' Import FIND test dataset
#' 
#' @export
get_find_data <- function() {
  
  base_url <- "https://finddx.shinyapps.io/FIND_Cov_19_Tracker/_w_989488b3/downloads/cv_tests_download.csv"
  
  d <- readr::read_csv(base_url)
  
}
