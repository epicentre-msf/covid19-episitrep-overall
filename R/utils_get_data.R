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
    dplyr::rename(geoid = geoId, country_ecdc = countriesAndTerritories, iso_a3 = countryterritoryCode, population_2018 = popData2018) %>%
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
    dplyr::select(date, country_ecdc:geoid, country:region, iso_a3, cases, deaths, population_2018, source)
  
}


#' Import WHO dataset
#' 
#' @export
get_who_data <- function() {
  
  who_raw <- NCoVUtils::get_who_cases() %>% 
    dplyr::mutate(date = as.Date(Date)) %>% 
    dplyr::select(-SituationReport, -Date, -dplyr::starts_with("RA")) %>% 
    tidyr::pivot_longer(
      cols = -c(date),
      names_to = c("country_who", "indicator", "drop"),
      names_sep = "-",
      values_to = "value"
    )
  
  who_countries <- who_raw %>% 
    dplyr::filter(country_who != "Region", is.na(drop)) %>% 
    dplyr::select(-drop) %>% 
    tidyr::replace_na(list(indicator = "cases")) %>% 
    dplyr::filter(
      indicator %in% c("cases", "deaths"), 
      !country_who %in% c("InternationalConveyance", "HealthCareWorkers")
    ) %>% 
    tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
    dplyr::group_by(country_who) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~c(.[1], diff(.)) %>% as.integer) %>% # convert from cumulative to daily
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(iso_a3 = countrycode::countrycode(country_who, "country.name", "iso3c")) %>% 
    dplyr::mutate(iso_a3 = dplyr::case_when(
      country_who == "CentralAfricanRepublic" ~"CAF",
      country_who == "DominicanRepublic"      ~"DOM",
      country_who == "Eswatini"               ~"SWZ",
      country_who == "IsleofMan"              ~"IMN",
      country_who == "Isreal"                 ~"ISR",
      country_who == "Kosovo"                 ~"XKX",
      country_who == "RepublicofKorea"        ~"KOR",
      country_who == "SaintLucia"             ~"LCA",
      country_who == "SaintMartin"            ~"MAF",
      country_who == "SintMaartin"            ~"MAF",
      country_who == "SouthAfrica"            ~"ZAF",
      country_who == "UnitedStatesofAmerica"  ~"USA",
      TRUE ~ iso_a3
    )) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # countrycode gives DRC the name of 'Congo - Kinshasa' for some reason? fix this
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
      source = "WHO"
    )
  
  return(who_countries)
  
}

#' Import John Hopkins dataset
#' 
#' @export
get_jhcsse_data <- function() {
  
  url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  
  df_cases <- clean_jhcsse_data(url_cases, type = "cases")
  df_deaths <- clean_jhcsse_data(url_deaths, type = "deaths")
  
  df_jhcsse <- 
    dplyr::full_join(df_cases, df_deaths, by = c("iso_a3", "country_jh", "date")) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # make congo names more PC
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
      source = "JHCSSE"
    ) %>% 
    dplyr::select(date, country_jh, iso_a3, country:region, cases, deaths, source)
  
}

clean_jhcsse_data <- function(path, type = c("cases", "deaths")) {
  readr::read_csv(path) %>% 
    dplyr::filter(is.na(`Province/State`)) %>% # filter to only countries
    dplyr::select(-`Province/State`, -Lat, -Long) %>% 
    dplyr::rename(country_jh = `Country/Region`) %>% 
    tidyr::pivot_longer(-country_jh, names_to = "date", values_to = type) %>% 
    dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
    dplyr::group_by(country_jh) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_if(is.numeric, ~c(.[1], diff(.)) %>% as.integer) %>% # convert from cumulative to daily
    dplyr::mutate_if(is.numeric, ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(iso_a3 = countrycode::countrycode(country_jh, "country.name", "iso3c")) 
}

#' Import interventions dataset
#' 
#' @export
get_interventions_data <- function() {
  
  base_url <- "https://data.humdata.org"
  dl_url <- xml2::read_html(paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>% 
    rvest::html_node(css = ".ga-download") %>% 
    rvest::html_attr("href") %>% 
    xml2::url_absolute(base_url)
  
  temp <- tempdir()
  filename <- "interventions.xlsx"
  curl::curl_download(dl_url, destfile = fs::path(temp, filename))
  
  readxl::read_excel(fs::path(temp, filename), sheet = "Database") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name"),
      # make congo names more PC
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country),
      continent = countrycode::countrycode(iso, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso, origin = "iso3c", destination = "region")
    ) 
}


