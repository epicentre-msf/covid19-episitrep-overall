name": "sea_grid1",
  "desc": "Grid for South East Asian countries.



## --- GIS data - Mercator projection
sf_mercator <- rnaturalearth::ne_countries(type = "countries", returnclass = "sf") %>% 
  tibble::as_tibble() %>% 
  sf::st_as_sf() %>% 
  dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  dplyr::mutate(
    continent = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent")),
    region = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"))) %>% 
  dplyr::filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
  cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(., of_largest_polygon = TRUE))))  %>% 
  dplyr::rename(lon = X, lat = Y) 



dta_south_america <- sf_mercator %>% 
  filter(continent == "Americas",
         region == "South America")


auto_countries

grd <- grid_auto(dta_south_america, 
                 names = "country",
                 codes = "iso_a3",
                 seed = 1) # names are inferred
grd <- grid_auto("south america", seed = 1)

grid_preview(grd, label = "name_country")
grid_design(grd)



ggplot(grd, 
       aes(x = col, 
           y = -row,
           label = name)) + 
  geom_rect(aes(xmin = as.numeric(col) - 0.5, 
                xmax = as.numeric(col) + 0.5, 
                ymin = as.numeric(-row) - 0.5, 
                ymax = as.numeric(-row) + 0.5),
            fill = "gray", 
            color = "#e1e1e1", 
            alpha = 0.7) +
  # ggplot2::ylim(levels(row)) + 
  # ggplot2::xlim(levels(col)) + 
  ggplot2::geom_text()

