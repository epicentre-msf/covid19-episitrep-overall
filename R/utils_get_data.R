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


#' Download and save shapefiles and geo data locally if not present
#'
#' @param path path to local directory to store geo data
#'
#' @export
get_geo_data <- function(path) {
  
  path_shp <- file.path(path, paste0('sf_world','.RDS'))
  
  if (!file.exists(path_shp)) {
    sf_world <- get_world_sf(scale = 'small', proj = 'robinson')
    saveRDS(sf_world, file = path_shp)
  }
  
  path_countries <- file.path(path, paste0('df_countries','.RDS'))
  path_pop_country <- file.path(path, paste0('df_pop_country','.RDS'))
  path_pop_region <- file.path(path, paste0('df_pop_region','.RDS'))
  path_pop_continent <- file.path(path, paste0('df_pop_continent','.RDS'))
  path_iso_a3 <- file.path(path, paste0('iso-a3_for_tests','.csv'))
  
  if (any(!file.exists(c(path_countries, path_pop_country, path_pop_region, path_pop_continent, path_iso_a3)))) {
    df_ecdc <- get_ecdc_data() %>% prepare_ecdc_dta()
    
    df_countries     <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(continent, region, iso_a3, country))
    df_pop_country   <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(iso_a3, country, pop = population_2019))
    df_pop_region    <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(region)    %>% summarise(pop = sum(population_2019, na.rm = TRUE))
    df_pop_continent <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(continent) %>% summarise(pop = sum(population_2019, na.rm = TRUE))
    df_iso_a3 <- dplyr::select(df_countries, iso_a3, country) %>% dplyr::arrange(iso_a3, country) %>% tidyr::drop_na()
    
    saveRDS(df_countries    , file = path_countries)
    saveRDS(df_pop_country  , file = path_pop_country)
    saveRDS(df_pop_region   , file = path_pop_region)
    saveRDS(df_pop_continent, file = path_pop_continent)
    readr::write_csv(df_iso_a3, path = path_iso_a3)
  }
}



#' Import FIND test dataset
#' 
#' @export
get_find_data <- function() {
  
  base_url <- "https://finddx.shinyapps.io/FIND_Cov_19_Tracker/_w_989488b3/downloads/cv_tests_download.csv"
  
  d <- readr::read_csv(base_url)
  
}
