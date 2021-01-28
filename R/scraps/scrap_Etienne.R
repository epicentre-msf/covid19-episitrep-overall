
# Specific African countries ----------------------------------------------


tbl_preds_cases_30d_extra <- lapply(lst_mdl_cases_30d, function (x) x[['preds']]) %>% 
  tibble::enframe(name = "iso_a3") %>% 
  unnest(cols = c(value)) %>% 
  left_join(df_countries)  %>% 
  # filter(iso_a3 %in% iso_a3_increasing) %>%
  filter(country %in% c("Mali", "Burkina Faso", "Niger", "Chad", "Nigeria", "Malawi", "Democratic Republic of the Congo", "Cameroon", "Côte d’Ivoire", "Sierra Leone", "Liberia"))

preds_extra <- ggplot(tbl_preds_cases_30d_extra, aes(x = date, y = cnt)) + 
    facet_wrap(~ country, scales = "free_y", ncol = 4) + 
    geom_point(colour = '#1A62A3', size = 2) + 
geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1A62A3', alpha = 0.4) + 
  geom_line(aes(y = fit), colour = '#1A62A3', size = 1) + 
  scale_x_date(date_labels = "%d-%b") +
  xlab('') + 
  ylab(paste0('frequency and fitted values')) + 
  theme_light() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.text = element_text(size = 11))

preds_extra

ggsave(file.path(path.local.deepdive.Africa, paste0('preds_extra', '_', week_report, '.png')), 
       plot = preds_extra, 
       height = 9,
       width = 12,
       scale = 0.8)




# Haiti -------------------------------------------------------------------



dta <- get_owid_jhcsse() %>% 
  tidyr::drop_na(iso_a3) %>% 
  filter(between(date, left = NULL, right = date_max))


## --- ECDC Covid data
dta_ecdc <- get_owid_jhcsse() %>% 
  tidyr::drop_na(iso_a3) %>% 
  filter(between(date, left = NULL, right = date_max) & (country == 'Haiti'))

lst_dta_ecdc <- dta_ecdc %>% 
  multisplit("iso_a3")

lst_mdl_cases_30d <- list()


for(i in names(lst_dta_ecdc)){
  mdl <- linear_trend(dta = lst_dta_ecdc[[i]], series = 'cases', n_days = 30, 
                      last_date = date_max)
  lst_mdl_cases_30d[[i]] <- mdl
}

lst_preds_cases_30d <- lapply(lst_mdl_cases_30d, function (x) x[['preds']])


tbl_coeff_cases_30d <- lapply(lst_mdl_cases_30d, function (x) x[['coeffs']]) %>% 
  tibble::enframe(name = "iso_a3") %>% 
  unnest(cols = c(value)) %>% 
  mutate(
    trend = case_when(
      lwr > 0 ~ "Increasing", 
      upr < 0 ~ "Declining", 
      upr > 0 & lwr < 0 ~ "Stable", 
      TRUE ~ NA_character_) %>% 
      factor(levels = c('Increasing', 'Stable', 'Declining')))


tbl_preds_cases_30d <- lapply(lst_mdl_cases_30d, function (x) x[['preds']]) %>% 
  tibble::enframe(name = "iso_a3") %>% 
  unnest(cols = c(value)) %>% 
  left_join(df_countries)
  # filter(iso_a3 %in% iso_a3_increasing)

preds <- ggplot(tbl_preds_cases_30d, aes(x = date, y = cnt)) + 
  facet_wrap(~ country, scales = "free_y", ncol = 4) + 
  geom_point(colour = '#1A62A3', size = 2) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1A62A3', alpha = 0.4) + 
  geom_line(aes(y = fit), colour = '#1A62A3', size = 1) + 
  scale_x_date(date_labels = "%d-%b") +
  xlab('') + 
  ylab(paste0('frequency and fitted values')) + 
  theme_light() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.text = element_text(size = 11))

preds

ggsave(file.path(path.local.deepdive.Africa, paste0('preds_Haiti', '_', week_report, '.png')), 
       plot = preds, 
       height = 9,
       width = 12,
       scale = 0.8)
