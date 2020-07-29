


sf_world <- get_world_sf(scale = 'small', proj = 'robinson')
saveRDS(sf_world, file = file.path(path.data, paste0('sf_world','.RDS')))



df_ecdc <- get_ecdc_data() %>% prepare_ecdc_dta()

df_countries     <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(continent, region, iso_a3, country))
df_pop_country   <- df_ecdc %>% filter(!is.na(iso_a3)) %>% distinct_at(vars(iso_a3, country, pop = population_2019))
df_pop_region    <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(region)    %>% summarise(pop = sum(population_2019, na.rm = TRUE))
df_pop_continent <- df_ecdc %>% filter(!is.na(iso_a3)) %>% group_by(continent) %>% summarise(pop = sum(population_2019, na.rm = TRUE))


saveRDS(df_countries    , file = file.path(path.data, paste0('df_countries','.RDS')))
saveRDS(df_pop_country  , file = file.path(path.data, paste0('df_pop_country','.RDS')))
saveRDS(df_pop_region   , file = file.path(path.data, paste0('df_pop_region','.RDS')))
saveRDS(df_pop_continent, file = file.path(path.data, paste0('df_pop_continent','.RDS')))

