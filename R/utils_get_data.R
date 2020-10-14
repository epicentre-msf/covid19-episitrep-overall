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
get_ecdc_data <- function(local_path = path.local.worldwide.data, file_name = 'dta_ECDC.RDS', force = FALSE) {
  
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  
  
  get_new_dta <- if(!file.exists(file.path(local_path, file_name))) {
    
    TRUE
    
    } else {
      
      last_update <- readRDS(file.path(local_path, file_name))$last_update
      ifelse(last_update == Sys.Date(), FALSE, TRUE)
      
    }
  
  
  
  if (get_new_dta | force) {
    
    dta <- readr::read_csv(base_url)
    last_update <-  Sys.Date()
    dta <- list("dta" = dta, "last_update" = last_update)
    saveRDS(dta, file = file.path(path.local.worldwide.data, file_name))
    
    } else {
      
      dta <- readRDS(file.path(path.local.worldwide.data, file_name))
      
    }
  
  return(dta)
}




#' Download and save shapefiles and geo data locally if not present
#'
#' @param path path to local directory to store geo data
#' @param update if force = FALSE files are downloaded only if they do not exist in the local directory. If force = TRUE the files are downloaded even if they already exist in the local directory
#'
#' @export
get_geo_data <- function(path, force = FALSE) {
  
  path_shp <- file.path(path, paste0('sf_world','.RDS'))
  
  if (!file.exists(path_shp) | force) {
    sf_world <- get_world_sf(scale = 'small', proj = 'robinson')
    saveRDS(sf_world, file = path_shp)
  }
  
  path_countries     <- file.path(path, paste0('df_countries','.RDS'))
  path_pop_country   <- file.path(path, paste0('df_pop_country','.RDS'))
  path_pop_region    <- file.path(path, paste0('df_pop_region','.RDS'))
  path_pop_continent <- file.path(path, paste0('df_pop_continent','.RDS'))
  path_iso_a3        <- file.path(path, paste0('iso-a3_for_tests','.csv'))
  
  if (any(!file.exists(c(path_countries, path_pop_country, path_pop_region, path_pop_continent, path_iso_a3))) | force) {
    df_ecdc <- get_ecdc_data()[[1]] %>% prepare_ecdc_dta()
    
    df_countries     <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(continent, region, iso_a3, country))
    df_pop_country   <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(iso_a3, country, pop = population_2019))
    df_pop_region    <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(region)    %>% summarise(pop = sum(population_2019, na.rm = TRUE))
    df_pop_continent <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(continent) %>% summarise(pop = sum(population_2019, na.rm = TRUE))
    df_iso_a3 <- get_iso_for_tests()
    
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
get_FIND_data <- function(local_path = path.local.worldwide.data, file_name = 'dta_tests.RDS', force = FALSE) {
  
  base_url <- "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv"
  
  get_dta <- if(!file.exists(file.path(local_path, file_name))) {
    
    TRUE
    
  } else {
    
    last_update <- readRDS(file.path(local_path, file_name))$last_update
    ifelse(last_update == Sys.Date(), FALSE, TRUE)
    
  }

  
  
  if (get_dta | force) {
    
    dta <- readr::read_csv(base_url)
    last_update <-  Sys.Date()
    dta <- list("dta" = dta, "last_update" = last_update)
    saveRDS(dta, file = file.path(path.local.worldwide.data, file_name))
    
  } else {
    
    dta <- readRDS(file.path(path.local.worldwide.data, file_name))
    
  }
  
  return(dta)
  
}


get_iso_for_tests <- function() {
  tibble::tribble(
    ~iso_a3,                            ~country,
    "ABW",                             "Aruba",
    "AFG",                       "Afghanistan",
    "AGO",                            "Angola",
    "ALB",                           "Albania",
    "AND",                           "Andorra",
    "ARE",              "United Arab Emirates",
    "ARG",                         "Argentina",
    "ARM",                           "Armenia",
    "ATG",               "Antigua and Barbuda",
    "AUS",                         "Australia",
    "AUT",                           "Austria",
    "AZE",                        "Azerbaijan",
    "BDI",                           "Burundi",
    "BEL",                           "Belgium",
    "BEN",                             "Benin",
    "BFA",                      "Burkina Faso",
    "BGD",                        "Bangladesh",
    "BGR",                          "Bulgaria",
    "BHR",                           "Bahrain",
    "BHS",                           "Bahamas",
    "BIH",            "Bosnia and Herzegovina",
    "BLR",                           "Belarus",
    "BLZ",                            "Belize",
    "BMU",                           "Bermuda",
    "BOL",                           "Bolivia",
    "BRA",                            "Brazil",
    "BRB",                          "Barbados",
    "BRN",                            "Brunei",
    "BTN",                            "Bhutan",
    "BWA",                          "Botswana",
    "CAF",          "Central African Republic",
    "CAN",                            "Canada",
    "CHE",                       "Switzerland",
    "CHL",                             "Chile",
    "CHN",                    "Mainland China",
    "CIV",                     "Cote d'Ivoire",
    "CMR",                          "Cameroon",
    "COD",  "Democratic Republic of the Congo",
    "COG",             "Republic of the Congo",
    "COL",                          "Colombia",
    "COM",                           "Comoros",
    "CPV",                        "Cape Verde",
    "CRI",                        "Costa Rica",
    "CUB",                              "Cuba",
    "CUW",                           "Curaçao",
    "CYM",                    "Cayman Islands",
    "CYP",                            "Cyprus",
    "CZE",                    "Czech Republic",
    "DEU",                           "Germany",
    "DJI",                          "Djibouti",
    "DMA",                          "Dominica",
    "DNK",                           "Denmark",
    "DOM",                "Dominican Republic",
    "DZA",                           "Algeria",
    "ECU",                           "Ecuador",
    "EGY",                             "Egypt",
    "ERI",                           "Eritrea",
    "ESH",                    "Western Sahara",
    "ESP",                             "Spain",
    "EST",                           "Estonia",
    "ETH",                          "Ethiopia",
    "FIN",                           "Finland",
    "FJI",                              "Fiji",
    "FRA",                            "France",
    "FRO",                     "Faroe Islands",
    "GAB",                             "Gabon",
    "GBR",                                "UK",
    "GEO",                           "Georgia",
    "GGY",                          "Guernsey",
    "GHA",                             "Ghana",
    "GIB",                         "Gibraltar",
    "GIN",                            "Guinea",
    "GMB",                        "The Gambia",
    "GNB",                     "Guinea Bissau",
    "GNQ",                 "Equatorial Guinea",
    "GRC",                            "Greece",
    "GRD",                           "Grenada",
    "GRL",                         "Greenland",
    "GTM",                         "Guatemala",
    "GUM",                              "Guam",
    "GUY",                            "Guyana",
    "HKG",                         "Hong Kong",
    "HND",                          "Honduras",
    "HRV",                           "Croatia",
    "HTI",                             "Haiti",
    "HUN",                           "Hungary",
    "IDN",                         "Indonesia",
    "IMN",                       "Isle of Man",
    "IND",                             "India",
    "IRL",                           "Ireland",
    "IRN",        "Iran (Islamic Republic of)",
    "IRQ",                              "Iraq",
    "ISL",                           "Iceland",
    "ISR",                            "Israel",
    "ITA",                             "Italy",
    "JAM",                           "Jamaica",
    "JEY",                            "Jersey",
    "JOR",                            "Jordan",
    "JPN",                             "Japan",
    "KAZ",                        "Kazakhstan",
    "KEN",                             "Kenya",
    "KGZ",                        "Kyrgyzstan",
    "KHM",                          "Cambodia",
    "KNA",                 "St. Kitts & Nevis",
    "KOR",                 "Republic of Korea",
    "KWT",                            "Kuwait",
    "LAO",  "Lao People's Democratic Republic",
    "LBN",                           "Lebanon",
    "LBR",                           "Liberia",
    "LBY",                             "Libya",
    "LCA",                         "St. Lucia",
    "LIE",                     "Liechtenstein",
    "LKA",                         "Sri Lanka",
    "LSO",                           "Lesotho",
    "LTU",                         "Lithuania",
    "LUX",                        "Luxembourg",
    "LVA",                            "Latvia",
    "MAR",                           "Morocco",
    "MCO",                            "Monaco",
    "MDA",                           "Moldova",
    "MDG",                        "Madagascar",
    "MDV",                          "Maldives",
    "MEX",                            "Mexico",
    "MKD",                   "North Macedonia",
    "MLI",                              "Mali",
    "MLT",                             "Malta",
    "MMR",                           "Myanmar",
    "MNE",                        "Montenegro",
    "MNG",                          "Mongolia",
    "MNP",          "Northern Mariana Islands",
    "MOZ",                        "Mozambique",
    "MRT",                        "Mauritania",
    "MSR",                        "Montserrat",
    "MUS",                         "Mauritius",
    "MWI",                            "Malawi",
    "MYS",                          "Malaysia",
    "NAM",                           "Namibia",
    "NCL",                     "New Caledonia",
    "NER",                             "Niger",
    "NGA",                           "Nigeria",
    "NIC",                         "Nicaragua",
    "NLD",                       "Netherlands",
    "NOR",                            "Norway",
    "NPL",                             "Nepal",
    "NZL",                       "New Zealand",
    "OMN",                              "Oman",
    "PAK",                          "Pakistan",
    "PAN",                            "Panama",
    "PER",                              "Peru",
    "PHL",                       "Philippines",
    "PNG",                  "Papua New Guinea",
    "POL",                            "Poland",
    "PRI",                       "Puerto Rico",
    "PRT",                          "Portugal",
    "PRY",                          "Paraguay",
    "PSE",    "Occupied Palestinian Territory",
    "PYF",                  "French Polynesia",
    "QAT",                             "Qatar",
    "ROU",                           "Romania",
    "RUS",                            "Russia",
    "RWA",                            "Rwanda",
    "SAU",                      "Saudi Arabia",
    "SDN",                             "Sudan",
    "SEN",                           "Senegal",
    "SGP",                         "Singapore",
    "SLE",                      "Sierra Leone",
    "SLV",                       "El Salvador",
    "SMR",                        "San Marino",
    "SOM",                           "Somalia",
    "SRB",                            "Serbia",
    "SSD",                       "South Sudan",
    "STP",             "Sao Tome and Principe",
    "SUR",                          "Suriname",
    "SVK",                          "Slovakia",
    "SVN",                          "Slovenia",
    "SWE",                            "Sweden",
    "SWZ",                          "Eswatini",
    "SXM",                      "Saint Martin",
    "SYC",                        "Seychelles",
    "SYR",                             "Syria",
    "TCA",            "Turks & Caicos Islands",
    "TCD",                              "Chad",
    "TGO",                              "Togo",
    "THA",                          "Thailand",
    "TJK",                        "Tajikistan",
    "TLS",                       "Timor-Leste",
    "TTO",               "Trinidad and Tobago",
    "TUN",                           "Tunisia",
    "TUR",                            "Turkey",
    "TWN",                            "Taiwan",
    "TZA",                          "Tanzania",
    "UGA",                            "Uganda",
    "UKR",                           "Ukraine",
    "URY",                           "Uruguay",
    "USA",                               "USA",
    "UZB",                        "Uzbekistan",
    "VAT",                      "Vatican City",
    "VCT",          "St. Vincent & Grenadines",
    "VEN",                         "Venezuela",
    "VGB",            "British Virgin Islands",
    "VIR",               "U.S. Virgin Islands",
    "VNM",                           "Vietnam",
    "XKX",                            "Kosovo",
    "YEM",                             "Yemen",
    "ZAF",                      "South Africa",
    "ZMB",                            "Zambia",
    "ZWE",                          "Zimbabwe"
  )
}



#' Import MSF linelist dataset
#' 
#' @import
get_msf_linelist <- function(path_local = path.local.msf.data, file_name = 'dta_MSF_linelist.RDS', path_remote = path.sharepoint.data, force = FALSE) {
  
  dta_path_local  <- file.path(path_local, file_name) 
  dta_path_remote <- max(fs::dir_ls(path_remote, regexp = "[.]rds$"))
  
  if (!file.exists(dta_path_local) | force) {
    
    dta <- readRDS(dta_path_remote)
    saveRDS(dta, file = dta_path_local)
    
  } else {
    
    dta <- readRDS(dta_path_local)
    
  }
  
  return(dta)
}



#' Import MSF aggregated dataset
#' 
#' @import
get_msf_aggregated <- function(path_local = path.local.msf.data, file_name = 'dta_MSF_aggregated.RDS', path_remote = file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx"), vars_name = c("sheet", "oc", "country", "project", "date", "week", "suspected", "probable", "confirmed", "non_cases", "unknown"), force = FALSE) {
  
  library(purrr)
  library(readxl)

  dta_path_local <- file.path(path_local, file_name) 
  
  if (!file.exists(dta_path_local) | force) {
    
    dta <- excel_sheets(path_remote) %>% 
      setdiff(., c('Feuil1', 'feuil1', 'Sheet1', 'sheet1', 'Sheet4')) %>% 
      map_df(~{
        oc      <- read_excel(path = path_remote, sheet = .x, range = "B1", col_names = FALSE) %>% pull()
        country <- read_excel(path = path_remote, sheet = .x, range = "D1", col_names = FALSE) %>% pull()
        project <- read_excel(path = path_remote, sheet = .x, range = "F1", col_names = FALSE) %>% pull()
        
        read_excel(path = path_remote, sheet = .x, skip = 5, col_names = FALSE) %>% 
          mutate(sheet = .x, oc = oc, country = country, project = project)
      }) %>% 
      select(sheet, oc, country, project, 1:7) %>% 
      set_names(vars_name)
    
    saveRDS(dta, file = dta_path_local)
    
  } else {
      
    dta <- readRDS(dta_path_local)
      
  }
  
  return(dta)
  
}



# Get first and last activity dates by project
#' 
#' @import
get_msf_aggregated_dates <- function(path_local = path.local.msf.data, file_name = 'dta_MSF_aggregated_dates.RDS', path_remote = file.path(path.sharepoint, "coordination", "Surveillance focal points coordination", "Aggregated reporting", "Report_covid_aggregate_all_v2.xlsx"), force = FALSE) {
  
  library(purrr)
  library(readxl)
  
  dta_path_local <- file.path(path_local, file_name) 
  
  if (!file.exists(dta_path_local) | force) {
    
    dta <- excel_sheets(path_remote) %>% 
      setdiff(., c('Feuil1', 'feuil1', 'Sheet1', 'sheet1', 'Sheet4')) %>% 
      map_df(~{
        
        if (is_empty(read_excel(path = path_remote, sheet = .x, range = "I1", col_names = FALSE))) {
          date_first <- NA
        } else {
          date_first <- read_excel(path = path_remote, sheet = .x, range = "I1", col_names = FALSE) %>% pull()
          date_first <- lubridate::as_date(date_first)
        }
        
        if (is_empty(read_excel(path = path_remote, sheet = .x, range = "K1", col_names = FALSE))) {
          date_last  <- NA
        } else {
          date_last <- read_excel(path = path_remote, sheet = .x, range = "K1", col_names = FALSE) %>% pull()
          date_last <- lubridate::as_date(date_last)
        }
        
        tibble::tibble(sheet = .x, date_first = date_first, date_last = date_last)
      })
    
    saveRDS(dta, file = dta_path_local)
    
  } else {
    
    dta <- readRDS(dta_path_local)
    
  }
  
  return(dta)
  
}

