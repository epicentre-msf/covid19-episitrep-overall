
# set ggplot theme
ggplot2::theme_set(ggplot2::theme_light(base_size = 14) + theme(panel.grid.minor = element_blank()))


cm_to_in <- 0.39370079

rounder <- function(x,y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
}


pyramid_brks <- function(x, n = 5) {
  brks <- pretty(0:max(abs(x)), n = n)
  c(-brks, brks)
}

pyramid_limits <- function(x) {
  c(-max(abs(x)), max(abs(x)))
}


add_brks <- function(x, n = 5, style = "jenks") {
  breaks <- classInt::classIntervals(x, n = n, style = style)
  br <- breaks$brks
  cut(x, br, include.lowest = TRUE, labels = label_breaks(br))
}


label_breaks <- function(breaks, big_numbers = FALSE, replace_Inf = TRUE) {
  
  if (big_numbers){
    labs <- sprintf("%s-%s", frmt_num(breaks[1:length(breaks) - 1]), frmt_num(breaks[2:length(breaks)]))
  } else {
    labs <- sprintf("%s-%s", breaks[-length(breaks)], breaks[-1] - 1)
  }
  
  if(replace_Inf){
    labs <- gsub("-Inf", "+", labs)
  }
  
  return(labs)
}



frmt_num <- function(x) {
  scales::label_number_si()(x)
}


guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}



freq_prct <- function(x, value){
  paste0(sum(x == value, na.rm = TRUE), 
         ' (', 
         format(round(sum(x == value, na.rm = TRUE) / sum(!is.na(x)) * 100, digits = 1), nsmall = 1), 
         ')')
}



call_countries_with <- function(dta, left = -Inf, right = Inf, series){
  
  series <- sym(series)
  
  dta %>% 
    filter(between(!!series, left = left, right =  right)) %>% 
    arrange(desc(!!series)) %>% 
    pull(country)
}




call_countries_increasing <- function(obs, continent_name = NULL){
  
  selected_tbl <- switch(obs, 
                         cases  = tbl_cases_increasing_trend, 
                         deaths = tbl_deaths_increasing_trend)
  
  if (!is.null(continent_name)) {
    selected_tbl <- filter(selected_tbl, continent %in% continent_name)
  }
  
  selected_tbl <- arrange(selected_tbl, desc(coeff))
  
  called_countries <- pull(selected_tbl, 'country')
  
  return(called_countries)
  
}



call_countries_doubling <- function(est, continent_name = NULL, threshold = threshold_doubling_time){
  
  est <- sym(est)
  
  if (!is.null(continent_name)) {
    selected_tbl <- filter(tbl_cfr_doubling_rank, continent %in% continent_name)
  } else {
    selected_tbl <- tbl_cfr_doubling_rank
  }
  
  selected_tbl <- filter(selected_tbl, !!est < threshold)
  selected_tbl <- arrange(selected_tbl, desc(!!est))
  
  called_countries <- pull(selected_tbl, 'country')
  
  return(called_countries)
  
}



cbind_diff <- function(x = list()){
  # Find max length
  max_length <- max(unlist(lapply(x, length)))
  
  # Set length of each vector as
  res <- lapply(x, function(x){
    length(x) <- max_length
    return(x)
  })
  
  return(as.data.frame(res))
}




plot_map_world_count <- function(tbl_dta, series){
  
  legend_title <- switch(series, 
                         cases  = 'Covid-19 cases', 
                         deaths = 'Covid-19 associated deaths')
  
  plot_title <- switch(series, 
                       cases  = 'Cases count', 
                       deaths = 'Deaths count')
  
  plot_palette <- switch(series, 
                         cases  = 'Blues', 
                         deaths = 'Reds')
  
  labels <- tbl_dta %>% pull(brks) %>% levels()
  
  sf_dta <- tbl_dta %>% 
    full_join(
      select(sf_world, iso_a3),
      by = "iso_a3"
    ) %>% 
    st_as_sf()
  
  plot_map <- ggplot(sf_dta) + 
    geom_sf(aes(fill = brks), size = .1) + 
    scale_fill_brewer(
      name = legend_title, palette = plot_palette, 
      drop = FALSE, 
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(70 / length(labels), units = "mm"),
        title.hjust = 0.5,
        nrow = 1,
        label.position = "bottom",
        title.position = 'top')) +
    labs(title = plot_title, 
         caption = caption_world_map) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom")
  
  return(plot_map)
  
}




plot_map_world_trend <- function(tbl_dta, series, model_for_trends = 'linear', plot_palette = RdAmGn){
  
  RdAmGn <- c('#D95F02', '#E6AB02', '#1B9E77') # Three-colours palette (Red-Amber-Green) colour-blind safe
  
  legend_title <- switch(series, 
                         cases  = 'Trends of cases count', 
                         deaths = 'Trends of deaths count')
  
  plot_title <- switch(series, 
                       cases  = 'Trends in cases', 
                       deaths = 'Trends in deaths')
  
  series <- sym(series)
  
  sf_dta <- tbl_dta %>% 
    select(c(iso_a3 : !!series), all_of(vars_trends(model_for_trends))) %>% 
    inner_join(
      select(sf_world, iso_a3),
      by = "iso_a3"
    ) %>% 
    st_as_sf()
  
  labels <- sf_dta %>% as_tibble() %>% pull(trend) %>% levels()
  
  plot_map <- ggplot(sf_dta) + 
    geom_sf(aes(fill = trend), size = .1, alpha = 0.8) + 
    scale_fill_manual(
      name = legend_title, 
      values = plot_palette, 
      drop = FALSE, 
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(50 / length(labels), units = "mm"),
        title.hjust = 0.5,
        nrow = 1,
        label.position = "bottom",
        title.position = 'top')) +
    labs(title = plot_title, caption = caption_world_map) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
  
  return(plot_map)
  
}



country_plot <- function(country_iso, series, lst_dta = lst_ecdc, model = 'linear', date_min = NULL) {
  
  choice <- paste(series, model, sep = '_')
  
  mld_list <- switch(choice, 
                     cases_linear   = model_cnt_cases_linear, 
                     deaths_linear  = model_cnt_deaths_linear, 
                     cases_poisson  = model_cnt_cases_poisson, 
                     deaths_poisson = model_cnt_deaths_poisson)

  mld_par <- mld_list[[5]]
  dates_extent <- c(mld_par[[1]][1], mld_par[[1]][2])
  
  mdl <- mld_list[[2]][[country_iso]]
  
  dta_obs <- lst_dta[[country_iso]] %>% 
    select(date, obs = all_of(series))
  
  dta_mdl <- tibble(dta_obs %>% 
                          filter(between(date, 
                                         left = dates_extent[1],  
                                         right = dates_extent[2])), 
                        fit = mdl$fit, 
                        lwr = mdl$lwr, 
                        upr = mdl$upr)
  
  obs_max <- max(dta_obs$obs, na.rm = TRUE)
  
  if (is.null(date_min)) {
    date_min <- dta_obs %>% filter(obs != 0) %>% pull(date) %>% min()
  }
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  # The complete epicurve
  plot_obs <- ggplot(dta_obs, aes(x = date, y = obs)) + 
    geom_col(colour = main_colour, fill = main_colour) + 
    scale_x_date(limits = c(date_min, NA)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = 'Since the first cases reported') + 
    theme_light()
  
  # The model
  plot_mdl <- ggplot(dta_mdl, aes(x = date, y = obs)) + 
    geom_point(size = 2 , colour = main_colour) + 
    geom_line(aes(y = fit), colour = main_colour, size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = main_colour, alpha = 0.4) + 
    xlab('') + 
    ylab(paste0(series, '/ fitted values')) + 
    labs(subtitle = paste('Last', length(dta_mdl$obs), 'days')) + 
    theme_light() 
  
  # List the plots
  return(list(plot_obs, plot_mdl, model = model))
  
}



# Plot cases or deaths for a single country with a zoom in the last 12 days -->
country_duo_plots <- function(series, country_iso, lst_dta = lst_ecdc, model = 'linear') {
  
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  
  grid.arrange(country_plot(country_iso = country_iso, series = series, model = model)[[1]], 
               country_plot(country_iso = country_iso, series = series, model = model)[[2]],
             ncol = 2, 
             top = textGrob(paste(glue('Covid-19 cases and deaths and trend estimations in {name_country}'), 
                                  glue('Data until {format(date_max_report, "%d %B %Y")}'), 
                                  sep = "\n"), 
                            gp = gpar(fontface = 'bold')))
}



# To to plot both cases and deaths into a single graphic plot
country_multi_plots <- function(country_iso, lst_dta = lst_ecdc, model = 'linear') {
  
  # Parameters
  main_colour  <- c(cases = '#1A62A3', deaths = '#e10000')
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  date_min     <- lst_dta[[country_iso]] %>% filter(cases != 0) %>% pull(date) %>% min()
  
  # Table observations
  dta_obs <- lst_dta[[country_iso]] %>% 
    select(date, cases, deaths) %>% 
    pivot_longer(-date, names_to = 'obs', values_to = 'count')
  
  
  # Table predictions
  lst_cases_mdl <- switch(model, 
                          linear  = model_cnt_cases_linear, 
                          poisson = model_cnt_cases_poisson)
  
  
  lst_deaths_mdl <- switch(model, 
                           linear  = model_cnt_deaths_linear, 
                           poisson = model_cnt_deaths_poisson)
  
  mld_par <- lst_cases_mdl[[5]]
  dates_extent <- c(mld_par[[1]][1], mld_par[[1]][2])
  
  dta_cases_mod <- lst_dta[[country_iso]] %>% 
    select(date, count = cases) %>% 
    mutate(
      obs = 'cases') %>% 
    filter(between(date, dates_extent[1], dates_extent[2])) %>% 
    tibble::add_column(lst_cases_mdl[['preds']][[country_iso]])
  
  dta_deaths_mod <- lst_dta[[country_iso]] %>% 
    select(date, count = deaths) %>% 
    mutate(
      obs = 'deaths') %>% 
    filter(between(date, dates_extent[1], dates_extent[2])) %>% 
    tibble::add_column(lst_deaths_mdl[['preds']][[country_iso]]) # This should be 11 rows
  
  dta_mod <- rbind(dta_cases_mod, dta_deaths_mod)
  
  
  # Plots
  plot_obs <- ggplot(dta_obs, aes(x = date, y = count)) + 
    facet_wrap(~obs, scales = "free_y", ncol = 1) + 
    geom_col(aes(colour = obs, fill = obs)) + 
    scale_colour_manual(values = main_colour) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = c(date_min, NA), date_labels = "%b-%Y") +
    xlab('') + 
    ylab('frequency') + 
    labs(subtitle = 'Since the first cases reported') + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  
  plot_mod <- ggplot(dta_mod, aes(x = date, y = count)) + 
    facet_wrap(~ obs, scales = "free_y", ncol = 1) + 
    geom_point(aes(colour = obs), size = 2) + 
    scale_colour_manual(values = main_colour) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = obs), alpha = 0.4) + 
    geom_line(aes(y = fit, colour = obs), size = 1) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = c(dates_extent[[1]], dates_extent[[2]]), date_labels = "%d-%b") +
    xlab('') + 
    ylab(paste0('frequency and fitted values')) + 
    labs(subtitle = paste('Last', length(dta_cases_mod$obs), 'days')) + 
    theme_light() + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  grid.arrange(plot_obs, 
               plot_mod, 
               ncol = 2, 
               top = textGrob(paste(glue('Covid-19 cases and deaths and trend estimations in {name_country}'), 
                                    glue('Data until {format(date_max_report, "%d %B %Y")} (fitting with {model} regression model)'), 
                                    sep = "\n"), 
                              gp = gpar(fontface = 'bold')))
}



country_plot_coeff <- function(series, country_iso) {
  
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  
  df_country <- switch(series, 
                       cases  = lst_coeffs_cases[[country_iso]], 
                       deaths = lst_coeffs_deaths[[country_iso]])
  
  quo_series <- sym(series)
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  date_min <- min(df_country$date, na.rm = TRUE)
  date_max <- max(df_country$date, na.rm = TRUE)
  
  plot_crv <- ggplot(df_country, aes(x = date, y = !!quo_series)) + 
    geom_col(colour = main_colour,  fill = main_colour) + 
    xlim(c(date_min, date_max)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = glue('Number of {series} reported')) + 
    theme_light()
  
  plot_cff <- ggplot(df_country, aes(x = date)) +
    geom_line(aes(y = coeff), colour = '#1B9E77', size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
    xlim(c(date_min, date_max)) + 
    #scale_x_date(date_breaks = "4 weeks", date_labels = "%d %b") + 
    xlab(NULL) + 
    ylab('Slope coefficient') + 
    labs(subtitle = 'Slope coefficient curve') + 
    theme_light()
  
  grid_plot <- grid.arrange(rbind(ggplotGrob(plot_crv), ggplotGrob(plot_cff)), 
                               top = textGrob(glue('Evolution of the slope cofficient in {name_country}'), 
                                              gp = gpar(fontface = 'bold')))
  
  return(grid_plot)
  
}



make_tbl_prop <- function(dta, var1, var2 = NULL) {
  
  var1 <- sym(var1)
  
  tbl1 <- dta %>%
    group_by(!!var1, .drop = FALSE) %>%
    summarise(
      n = n()) %>% 
    mutate(
      pct = n/sum(n)) %>% 
    ungroup()
  
  if (!is.null(var2)){
    
    var2 <- sym(var2)
    
    tbl2 <- dta %>%
      group_by(!!var2, !!var1, .drop = FALSE) %>%
      summarise(
        n = n()) %>% 
      mutate(
        pct = n/sum(n)) %>% 
      pivot_wider(names_from = !!var2, values_from = c('n', 'pct')) %>% 
      full_join(tbl1) %>% 
      rename(n_Total = n, pct_Total = pct) %>% 
      ungroup()
  }
  
  if (is.null(var2)) {
    return(tbl1)
  } else {
    return(tbl2)
  }
  
}



make_tbl_cfr <- function(dta, x_var, label_var){
  
  x_var <- sym(x_var)
  
  dta <- dta %>% 
    filter(covid_status == 'Confirmed', outcome_status %in% c('Cured', 'Died'), !is.na(!!x_var))
  
  tbl_cfr_total <- dta %>% 
    select(!!x_var, outcome_status) %>% 
    group_by(!!x_var) %>% 
    summarise(
      totals_Total  = paste0(sum(outcome_status == 'Died'), '/', n()), 
      died_p_Total = mean(outcome_status == 'Died')) %>% 
    mutate(
      label_var = label_var)
  
  tbl_cfr_continent <- dta %>%
    select(continent, !!x_var, outcome_status) %>%
    group_by(continent, !!x_var) %>%
    summarise(
      totals = paste0(sum(outcome_status == 'Died'), '/', n()),
      died_p = mean(outcome_status == 'Died')) %>%
    pivot_wider(names_from = continent, values_from = c(totals, died_p))
  
  tbl_cfr <- right_join(tbl_cfr_continent, tbl_cfr_total)
  
  return(tbl_cfr)
}

