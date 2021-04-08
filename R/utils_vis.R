
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


#label_breaks <- function(breaks, exclusive = FALSE, replace_Inf = TRUE) {
#  
#  if (exclusive){
#    labs <- sprintf("%s-%s", breaks[-length(breaks)], breaks[-1] - 1)
#  } else {
#    labs <- sprintf("%s-%s", frmt_num(breaks[1:length(breaks) - 1]), frmt_num(breaks[2:length(breaks)]))
#  }
#  
#  if(replace_Inf){
#    labs <- gsub("-Inf", "+", labs)
#  }
#  
#  return(labs)
#}



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


format_percent <- function(x, digits = 1L, symbol = FALSE){
  if (symbol) {
    paste0(format(round(x * 100, digits = digits), nsmall = digits), '%')
  } else {
    round(x * 100, digits = digits)
  }
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
                         cases  = tbl_case_increasing_trend, 
                         deaths = tbl_death_increasing_trend)
  
  if (!is.null(continent_name)) {
    selected_tbl <- filter(selected_tbl, continent %in% continent_name)
  }
  
  selected_tbl <- arrange(selected_tbl, desc(coeff))
  
  called_countries <- pull(selected_tbl, 'country')
  
  return(called_countries)
  
}



call_countries_doubling <- function(est, continent_name = NULL,
                                    tbl_dta = tbl_doubling_cfr_rank, 
                                    threshold = threshold_doubling_time){
  
  est <- rlang::sym(est)
  
  if (!is.null(continent_name)) {
    selected_tbl <- filter(tbl_dta, continent %in% continent_name)
  } else {
    selected_tbl <- tbl_dta
  }
  
  selected_tbl <- filter(selected_tbl, !!est < threshold, !!est >= 0)
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
                       cases  = 'Cumulative Case count', 
                       deaths = 'Cumulative Death count')
  
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
    coord_sf(datum = NA) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
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
          legend.position = "bottom",
          plot.margin = margin(0, 0, 0, 0, "pt"))
  
  return(plot_map)
  
}




plot_map_world_trend <- function(tbl_dta, 
                                 series, 
                                 model_for_trends = 'linear', 
                                 plot_palette = RdAmGn){
  # continent = NULL
  RdAmGn <- c('#D95F02', '#E6AB02', '#1B9E77') # Three-colours palette (Red-Amber-Green) colour-blind safe
  
  legend_title <- switch(series, 
                         cases  = 'Trends of case count', 
                         deaths = 'Trends of death count')
  
  plot_title <- switch(series, 
                       cases  = 'Trends in cases', 
                       deaths = 'Trends in deaths')
  
  series <- sym(series)
  
  # if(!is.null(continent)) {
  #   # Define lims
  #   xlim_continent <- switch(continent,
  #                            "Africa"   = c(-25, 60),
  #                            "Europe"   = c(-30, 80),
  #                            "Americas" = c(-160, -30),
  #                            "Asia"     = c(20, 170))
  # 
  #   ylim_continent <- switch(continent,
  #                            "Africa"   = c(-35, 40),
  #                            "Europe"   = c(28, 73),
  #                            "Americas" = c(-70, 90),
  #                            "Asia"     = c(-20, 90))
  # }
  
  
  sf_dta <- tbl_dta %>% 
    select(c(iso_a3 : !!series), all_of(vars_trends(model_for_trend))) %>% 
    inner_join(
      select(sf_world, iso_a3),
      by = "iso_a3"
    ) %>% 
    st_as_sf()
  
  # if(!is.null(continent)) {
  #   sf_dta <- sf_dta %>% filter(continent == continent)
  # }
  
  
  labels <- sf_dta %>% as_tibble() %>% pull(trend) %>% levels()
  
  plot_map <- ggplot(sf_dta) + 
      geom_sf(aes(fill = trend), size = .1, alpha = 0.8) + 
      coord_sf(datum = NA) +
    scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
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
      theme(plot.title = element_text(hjust = 0.5), 
            legend.position = "bottom",
            plot.margin = margin(0, 0, 0, 0, "pt"))

    # plot_map 
    # ggplot(sf_dta) + 
    #   geom_sf(aes(fill = trend), size = .1, alpha = 0.8) + 
    #   scale_fill_manual(
    #     name = NULL, 
    #     values = RdAmGn, 
    #     drop = FALSE, 
    #     guide = guide_legend(
    #       keyheight = unit(3, units = "mm"),
    #       keywidth = unit(70 / length(labels), units = "mm"),
    #       title.hjust = 0.5,
    #       nrow = 1,
    #       label.position = "bottom",
    #       title.position = 'top')) + 
    #   coord_sf(
    #     crs = 3395,
    #     xlim = xlim_continent, 
    #     ylim = ylim_continent, 
    #     expand = FALSE,
    #     # datum = NA
    #     ) + 
    #   labs(title = 'Last 30 days') + 
    #   theme_light() +
    #   theme(plot.title = element_text(hjust = 0.5), 
    #         axis.text.x = element_blank(), 
    #         axis.text.y = element_blank(), 
    #         legend.position = "bottom")
       
  return(plot_map)
  
}


plot_cfr_ma <- function(dta){
  
  dta_cfr <- dta %>% 
    select(date, continent, region, country, iso_a3, cases, deaths) %>% 
    tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                    max(date, na.rm = TRUE), by = 1), 
                    fill = list(cases = 0, deaths = 0)) %>% 
    mutate(
      cases_lagged = dplyr::lag(cases, 8), 
      cases_ma = forecast::ma(cases_lagged, order = 5), 
      deaths_ma = forecast::ma(deaths, order = 5), 
      cfr = deaths_ma / cases_ma) %>% 
    mutate(
      cfr = as.double(cfr), 
      cfr = case_when(
        cfr > 1 ~ NA_real_, 
        TRUE ~ cfr))
  
  dta_cfr %>% as.data.frame()
  
  
  # Parameters
  main_colour  <- c(cases = '#1A62A3', deaths = '#e10000')
  name_country <- unique(dta$country)
  date_min     <- dta_cfr %>% filter(cases != 0) %>% pull(date) %>% min()
  
  n_max <- max(dta_cfr$deaths, na.rm = TRUE)
  p_max <- rounder(max(dta_cfr$cfr, na.rm = TRUE), .1)
  p_max <- ifelse(p_max > 1, 1, p_max)
  scaling_factor <- p_max / n_max # sets the y limit of the proportion rounded up to nearest 10% above the max
  
  
  p1 <- ggplot(dta_cfr, aes(x = date, y = cases)) + 
    geom_col(fill = '#1A62A3', width = 1) + 
    scale_x_date(name = NULL, date_breaks = "2 months", date_labels = "%b\n%Y") + 
    scale_y_continuous(name = 'Cases') + 
    theme_light() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  p2 <- ggplot(dta_cfr, aes(x = date, y = deaths)) + 
    geom_col(fill = '#de2d26', width = 1) + 
    geom_line(data = dta_cfr,
              aes(y = cfr / scaling_factor),
              key_glyph = "timeseries", size = 1) + 
    scale_x_date(name = NULL, date_breaks = "2 months", date_labels = "%b\n%Y") + 
    scale_y_continuous(name = 'Deaths', sec.axis = ggplot2::sec_axis(~ . * scaling_factor, name = "Case fatality (lag 8 days)", labels = scales::percent_format(accuracy = 1))) + 
    labs(fill = NULL, colour = NULL) + 
    theme_light() + 
    theme(legend.position = "top", legend.direction = 'vertical', legend.text = element_text(size = 11))
  
  p1 + p2 + plot_layout(ncol = 1)
  
}


country_plot <- function(country_iso, series, lst_dta = lst_jhu, model = 'linear', date_min = NULL) {
  
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
country_duo_plots <- function(series, country_iso, lst_dta = lst_jhu, model = 'linear') {
  
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  
  grid.arrange(country_plot(country_iso = country_iso, series = series, model = model)[[1]], 
               country_plot(country_iso = country_iso, series = series, model = model)[[2]],
               ncol = 2, 
               top = textGrob(paste(glue('Covid-19 cases and deaths and trend estimations in {name_country}'), 
                                    glue('Data until {format(date_max_report, "%d %B %Y")}'), 
                                    sep = "\n"), 
                              gp = gpar(fontface = 'bold')))
}



# To plot both cases and deaths into a single graphic plot
# This function was replace by the one below "country_four_plots" from week 29-2020
country_four_plots <- function(country_iso, lst_dta = lst_jhu, model = 'linear') {
  
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




#' To plot both cases and deaths into a single graphic plot
#'
#' @param country_iso 
#' @param lst_dta 
#' @param countries 
#'
#' @return
#' @export
#'
#' @examples
country_six_plots <- function(country_iso, 
                              lst_dta = lst_dta_jhu, 
                              countries = df_countries) {
  
  # Parameters
  main_colour  <- c(cases = '#1A62A3', deaths = '#e10000')
  name_country <- countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  date_min     <- lst_dta[[country_iso]] %>% filter(cases != 0) %>% pull(date) %>% min()
  
  # Table observations
  dta_obs <- lst_dta[[country_iso]] %>% 
    select(date, cases, deaths) %>% 
    pivot_longer(-date, names_to = 'obs', values_to = 'count')
  
  
  # Table predictions
  mdl_cases_short  <- model_cnt_cases_linear_short
  mdl_cases_long   <- model_cnt_cases_linear_long
  mdl_deaths_short <- model_cnt_deaths_linear_short
  mdl_deaths_long  <- model_cnt_deaths_linear_long
  
  mld_par_short <- mdl_cases_short$par
  dates_extent_short <- c(mld_par_short[[1]][1], mld_par_short[[1]][2])
  
  mld_par_long <- mdl_cases_long$par
  dates_extent_long <- c(mld_par_long[[1]][1], mld_par_long[[1]][2])
  
  
  dta_cases_short <- lst_dta[[country_iso]] %>% 
    select(date, count = cases) %>% 
    mutate(
      obs = 'cases') %>% 
    filter(between(date, dates_extent_short[1], dates_extent_short[2])) %>% 
    tibble::add_column(mdl_cases_short[['preds']][[country_iso]])
  
  dta_deaths_short <- lst_dta[[country_iso]] %>% 
    select(date, count = deaths) %>% 
    mutate(
      obs = 'deaths') %>% 
    filter(between(date, dates_extent_short[1], dates_extent_short[2])) %>% 
    tibble::add_column(mdl_deaths_short[['preds']][[country_iso]]) 
  
  dta_mld_short <- rbind(dta_cases_short, dta_deaths_short)
  
  
  dta_cases_long <- lst_dta[[country_iso]] %>% 
    select(date, count = cases) %>% 
    mutate(
      obs = 'cases') %>% 
    filter(between(date, dates_extent_long[1], dates_extent_long[2])) %>% 
    tibble::add_column(mdl_cases_long[['preds']][[country_iso]])
  
  dta_deaths_long <- lst_dta[[country_iso]] %>% 
    select(date, count = deaths) %>% 
    mutate(
      obs = 'deaths') %>% 
    filter(between(date, dates_extent_long[1], dates_extent_long[2])) %>% 
    tibble::add_column(mdl_deaths_long[['preds']][[country_iso]]) 
  
  dta_mld_long <- rbind(dta_cases_long, dta_deaths_long)
  
  
  # Plots
  plot_obs <- ggplot(dta_obs, aes(x = date, y = count)) + 
    facet_wrap(~obs, scales = "free_y", ncol = 1) + 
    geom_col(aes(colour = obs, fill = obs)) + 
    scale_colour_manual(values = main_colour) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = c(date_min, NA), breaks = '2 months', date_labels = "%b-%Y") +
    xlab('') + 
    ylab('frequency') + 
    labs(subtitle = 'Since the first cases reported') + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  
  plot_mdl_long <- ggplot(dta_mld_long, aes(x = date, y = count)) + 
    facet_wrap(~ obs, scales = "free_y", ncol = 1) + 
    geom_point(aes(colour = obs), size = 2) + 
    scale_colour_manual(values = main_colour) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = obs), alpha = 0.4) + 
    geom_line(aes(y = fit, colour = obs), size = 1) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = dates_extent_long, date_labels = "%d-%b") +
    xlab('') + 
    ylab(paste0('frequency and fitted values')) + 
    labs(subtitle = paste('Last', (dates_extent_long[[2]] - dates_extent_long[[1]] + 1), 'days')) + 
    theme_light() + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  
  plot_mdl_short <- ggplot(dta_mld_short, aes(x = date, y = count)) + 
    facet_wrap(~ obs, scales = "free_y", ncol = 1) + 
    geom_point(aes(colour = obs), size = 2) + 
    scale_colour_manual(values = main_colour) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = obs), alpha = 0.4) + 
    geom_line(aes(y = fit, colour = obs), size = 1) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = dates_extent_short, breaks = '4 days', date_labels = "%d-%b") +
    xlab('') + 
    ylab(paste0('frequency and fitted values')) + 
    labs(subtitle = paste('Last', (dates_extent_short[[2]] - dates_extent_short[[1]] + 1), 'days')) + 
    theme_light() + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  
  
  ggarrange(plot_obs, 
            plot_mdl_long, 
            plot_mdl_short, 
            ncol = 3, 
            widths = c(2,1.4,1.1)) %>% 
    
    annotate_figure(top = text_grob(paste(glue('Covid-19 cases and deaths and trend estimations in {name_country}'), 
                                          glue('Data until {format(date_max_report, "%d %B %Y")} (fitting with linear regression model)'), 
                                          sep = "\n"), 
                                    face = "bold", size = 14))
}







country_plot_coeff <- function(series, country_iso) {
  
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  
  lst_coeffs <- switch(series, 
                       cases  = lst_coeffs_cases[[country_iso]], 
                       deaths = lst_coeffs_deaths[[country_iso]])
  
  quo_series <- sym(series)
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  date_min <- min(lst_coeffs$date, na.rm = TRUE)
  date_max <- max(lst_coeffs$date, na.rm = TRUE)
  
  plot_crv <- ggplot(lst_coeffs, aes(x = date, y = !!quo_series)) + 
    geom_col(colour = main_colour,  fill = main_colour) + 
    xlim(c(date_min, date_max)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = glue('Number of {series} reported')) + 
    theme_light()
  
  plot_cff <- ggplot(lst_coeffs, aes(x = date)) +
    geom_line(aes(y = coeff), colour = '#1B9E77', size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
    xlim(c(date_min, date_max)) + 
    xlab(NULL) + 
    ylab('Slope coefficient') + 
    labs(subtitle = 'Slope coefficient curve') + 
    theme_light()
  
  grid.arrange(rbind(ggplotGrob(plot_crv), ggplotGrob(plot_cff)), 
               top = textGrob(glue('Evolution of the slope cofficient in {name_country}'), 
                              gp = gpar(fontface = 'bold')))
  
}



#' Title
#'
#' @param dta 
#' @param var_x 
#' @param var_y 
#' @param prop_by 
#' @param drop_levels 
#'
#' @return
#' @export
#'
#' @examples
tbl_prop <- function(dta, var_x, var_y = NULL, prop_by = 'col', drop_levels = FALSE) {
  
  var_x <- sym(var_x)
  
  tbl1 <- dta %>%
    group_by(!!var_x, .drop = drop_levels) %>%
    summarise(
      n = n())
  
  if (prop_by == 'row') {
    tbl1 <- tbl1 %>% 
      group_by(!!var_x)
  }
  
  tbl1 <- tbl1 %>% 
    mutate(
      N = sum(n), 
      p = n / N) %>% 
    ungroup()
  
  
  if (!is.null(var_y)) {
    
    var_y <- sym(var_y)
    
    var_by <- base::switch(prop_by, 
                           'col' = var_y, 
                           'row' = var_x)
    
    tbl2 <- dta %>% 
      group_by(!!var_y, !!var_x, .drop = drop_levels) %>%
      summarise(
        n = n()) %>% 
      group_by(!!var_by) %>% 
      mutate(
        N = sum(n), 
        p = n / N) %>% 
      group_by(!!var_x) %>% 
      pivot_wider(names_from = !!var_y, values_from = c('n', 'N', 'p')) %>% 
      full_join(tbl1) %>% 
      rename(n_Total = n, N_Total = N, p_Total = p) %>% 
      ungroup()
    
  }
  
  
  if (is.null(var_y)) {
    
    return(tbl1)
    
  } else {
    
    return(tbl2)
    
  }
  
}


tbl_cfr <- function(dta, x_var){
  
  x_var <- sym(x_var)
  
  tbl_cfr_total <- dta %>% 
    select(!!x_var, outcome_status) %>% 
    group_by(!!x_var) %>% 
    summarise(
      nN_Total  = paste0(sum(outcome_status == 'Died'), '/', n()), 
      p_Total = mean(outcome_status == 'Died'))
  
  tbl_cfr_continent <- dta %>%
    select(continent, !!x_var, outcome_status) %>%
    group_by(continent, !!x_var) %>%
    summarise(
      nN = paste0(sum(outcome_status == 'Died'), '/', n()),
      p = mean(outcome_status == 'Died')) %>%
    pivot_wider(names_from = continent, values_from = c(nN, p))
  
  tbl_cfr <- right_join(tbl_cfr_continent, tbl_cfr_total)
  
  return(tbl_cfr)
}



format_nbp <- function(my_N, my_p) {
  my_p <- round(100 * my_p, 1)
  my_string <- ifelse(my_N == 0,
                      "-",
                      glue::glue("{my_N} ({my_p}%)"))
}

format_med <- function(med, low, high) {
  med  <- round(med,  0)
  low  <- round(low,  0)
  high <- round(high, 0)
  
  formatted <- ifelse(is.na(med),
                      "-",
                      glue::glue("{med} [{low}-{high}]")
  )
}



#' Plot mortality by severity status  per project
#'
#' Generate and save the mortality over time for each severity status for a given project.
#' 
#' @param data The summarised data for a project. 
#' @param select_project Character string describing the project
#' @param path_save Character string containing the path to save the graph
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' dta_linelist %>%
#'  group_by(country, project, epi_week_admission, severity) %>% 
#'  summarise(n = n()) %>% 
#'  group_by(project) %>% 
#'  nest() %>% 
#'  mutate(plot = map2(.x = data, 
#'                     .y = project,
#'                     ~ plot_mortality_project(data = .x,
#'                                              select_project = .y,
#'                                              path_save = path.local.msf.graphs.oc.per.project))) 
plot_mortality_project <- function(data, 
                                   select_project = "",
                                   path_save = "") {
  
  
  # Get the country the project is in.
  country <- data %>% select(country) %>% unique() %>% pull()
  
  # String for plot title and name 
  country_project <- paste(country, select_project, sep = "_")
  
  pd <- position_dodge2(1)
  
  
  fig_data <- data %>% 
    ggplot(aes(x = epi_week_admission,
               y = n,
               colour = severity)) +
    geom_line(position = pd) +
    geom_point(alpha = 0.8,
               size = 2,
               position = pd) +
    
    labs(x = "Week of admission",
         y = "Nb of death",
         title = glue::glue("Mortality by severity for hospitalized patients, {country_project}")) +
    
    scale_x_date(name = "Week of Admission", 
                 date_breaks = "2 weeks", 
                 date_labels = "%V", 
                 sec.axis = ggplot2::sec_axis(trans = ~ .), 
                 expand = expansion(mult = c(0.01, 0.01))) +
    
    ggthemes::scale_colour_tableau(name = "Severity", 
                                   palette = "Tableau 20") 
  
  
  ggsave(filename = paste0(country_project, '_', 'mortality_severity', '_',
                           week_report, '.png'),
         path = path_save,
         plot = fig_data, 
         width = 7,
         height = 4,
         scale = 1.1,
         dpi = 320
  )
  
  return(fig_data)
}



#' Plot mortality by project
#'
#' Generate and saves a plot with number of hospitalised patients and mortality 
#' rates for a given project
#' 
#' @param data The summarised data for a project. 
#' @param select_project Character string describing the project
#' @param path_save Character string containing the path to save the graph
#'
#' @returnA ggplot object
#' @export
#'
#' @examples
plot_mortality_admission_project <- function(data, 
                                             select_project = "",
                                             path_save = "") {
  
  country <-  data %>% select(country) %>% unique() %>% pull()
  country_project <- paste(country, select_project, sep = "_")
  
  # Get scaling factor
  scaling_factor <- data %>% 
    ungroup() %>% 
    summarise(m_hospi = max(n_hospi, na.rm = TRUE),
              m_p = max(p_death, na.rm = TRUE)) %>% 
    mutate(scaling_factor = m_p /m_hospi) %>% pull(scaling_factor)
  
  
  fig_data <- data %>% 
    
    ggplot(aes(x = epi_week_admission)) +
    
    geom_col(aes(y = n_hospi), fill = "#A0CBE8") +
    
    geom_line(aes(y = p_death / scaling_factor), colour = "red") +
    geom_point(aes(y = p_death / scaling_factor),
               alpha = 0.8, colour = "red", size = 2) +
    
    scale_x_date(name = "Week of Admission", 
                 date_breaks = "2 weeks", 
                 date_labels = "%V", 
                 sec.axis = ggplot2::sec_axis(trans = ~ .), 
                 expand = expansion(mult = c(0.01, 0.01))) +
    
    scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * scaling_factor, 
                                                    name = "Mortality rate", 
                                                    labels = scales::percent_format(accuracy = 2L)),
                       expand = expansion(mult = c(0, 0.02))) +
    
    labs(x = "Week of admission",
         y = "Nb of hospitalizations",
         title = glue::glue("Hospitalized patients and mortality, {country_project}"))
  
  
  ggsave(filename = paste0(country_project, '_', 'mortality_admission', '_', week_report, '.png'),
         path = path_save,
         plot = fig_data, 
         width = 7,
         height = 4,
         scale = 1.1,
         dpi = 320
  )
  
  return(fig_data)
}



fct_plot_1 <- function(data,
                       country_project = "",
                       path_save = "") {
  
  status_levels <- c("Confirmed", "Probable", "Suspected", "Not a case", 
                     "Not a suspect", "Unknown")
  status_levels_cols <- c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
                          "#8CD17D")
  
  
  # Get the country the project is in.
  # country <- data %>% select(country) %>% unique() %>% pull()
  
  # String for plot title and name 
  # country_project <- paste(country, select_project, sep = "_")
  
  plot_1 <- data %>%
    ggplot(aes(x = epi_week_consultation,
               y = n,
               fill = ind_MSF_covid_status)) +
    geom_col() +
    
    labs(title = paste("Evolution of the total number of patients by Covid19 status",
                       country_project, sep = " "),
         caption = "Individual and aggregated data are displayed") +
    
    scale_x_date(name = "Week of Consultation",
                 date_breaks = "2 week",
                 date_labels = "%V",
                 expand = expansion(mult = c(0.01, 0.01)),
                 sec.axis = ggplot2::sec_axis(trans = ~ as.Date(.))) +
    
    scale_y_continuous(name = "Patients", expand = expansion(mult = c(0, 0.02))) +
    
    scale_fill_manual(name = "Status", 
                      breaks = status_levels,
                      values = status_levels_cols) +
    theme_light() +
    theme(legend.position = 'top',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  
  ggsave(filename = paste0(country_project, '_', 'hist_epicurve_status', '_',
                           week_report, '.png'),
         path = path.local.msf.graphs.oc.per.project,
         plot = plot_1,
         width = 6,
         height = 4,
         scale = 1.1,
         dpi = 320
  )
  
  return(plot_1)
}



fct_plot_2 <- function(data, 
                       country_project = "",
                       path_save = "") {
  
  # country <- data %>% select(iso_a3) %>% unique() %>% pull()
  
  pd <- position_dodge2(1)
  
  # Prepare data for both plots
  data <- data %>%
    filter(merge_admit == "Yes")  %>% 
    drop_na(epi_week_admission)
  
  # MSF_severity & MORTALITY
  data <- data %>% 
    filter(outcome_patcourse_status == "Died") %>%
    group_by(epi_week_admission, MSF_severity) %>%
    summarise(n = n()) %>%
    drop_na(MSF_severity)
  
  if(nrow(data) > 0) {
    plot_1 <- data %>% 
      
      ggplot(aes(x = epi_week_admission,
                 y = n,
                 colour = MSF_severity)) +
      geom_line(position = pd) +
      geom_point(alpha = 0.8,
                 size = 2,
                 position = pd) +
      
      labs(x = "Week of admission",
           y = "Nb of death",
           title = glue::glue("Mortality by MSF_severity for hospitalized patients, {country_project}")) +
      
      scale_x_date(name = "Week of Admission", 
                   date_breaks = "2 weeks", 
                   date_labels = "%V", 
                   sec.axis = ggplot2::sec_axis(trans = ~ .), 
                   expand = expansion(mult = c(0.01, 0.01))) +
      
      ggthemes::scale_colour_tableau(name = "MSF_severity", 
                                     palette = "Tableau 20") +
      theme(legend.position = "bottom")
    
    
    ggsave(filename = paste0(country_project, '_', 'mortality_MSF_severity', '_', week_report, '.png'),
           path = path_save,
           plot = plot_1,
           width = 8,
           height = 5,
           scale = 1.1,
           dpi = 320
    )
    
  } else {
    plot_1 <- NULL
  }
  
  return(plot_1)
}



fct_plot_3 <- function(data, 
                       country_project = "",
                       path_save = "") {
  
  
  # Prepare data for both plots
  data <- data %>%
    filter(merge_admit == "Yes")  %>% 
    drop_na(epi_week_admission)
  
  # MORTALITY & ADMISSION
  tbl_hospi_death <- data %>% 
    group_by(epi_week_admission) %>%
    summarise(n_hospi = n(),
              n_death = sum(outcome_patcourse_status == "Died", na.rm = TRUE),
              p_death = n_death / n_hospi)
  
  
  # Get scaling factor
  scaling_factor <- tbl_hospi_death %>% 
    ungroup() %>% 
    summarise(m_hospi = max(n_hospi, na.rm = TRUE),
              m_p = max(p_death, na.rm = TRUE)) %>% 
    mutate(scaling_factor = (m_p + 0.01) /(m_hospi + 0.01)) %>% pull(scaling_factor)
  
  
  if(nrow(tbl_hospi_death) > 0) {
    plot_3 <- tbl_hospi_death %>% 
      
      ggplot(aes(x = epi_week_admission)) +
      
      geom_col(aes(y = n_hospi), fill = "#A0CBE8") +
      
      geom_line(aes(y = p_death / scaling_factor), colour = "red") +
      geom_point(aes(y = p_death / scaling_factor),
                 alpha = 0.8, colour = "red", size = 2) +
      
      scale_x_date(name = "Week of Admission", 
                   date_breaks = "2 weeks", 
                   date_labels = "%V", 
                   sec.axis = ggplot2::sec_axis(trans = ~ .), 
                   expand = expansion(mult = c(0.01, 0.01))) +
      
      scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * scaling_factor, 
                                                      name = "Mortality rate", 
                                                      labels = scales::percent_format(accuracy = 2L)),
                         expand = expansion(mult = c(0, 0.02))) +
      
      labs(x = "Week of admission",
           y = "Nb of hospitalizations",
           title = glue::glue("Mortality of hospitalized patients, {country_project}"))
    
    
    ggsave(filename = paste0(country_project, '_', 'mortality', '_', week_report, '.png'),
           path = path_save,
           plot = plot_3,
           width = 8,
           height = 5,
           scale = 1.1,
           dpi = 320
    )
    
  } else {
    plot_3 <- NULL
  }
  
  return(plot_3)
}



print_plot <- function(x){
  if (!is.null(x)) {
    print(x)
  }
}




# Geofacted plots -------------------------------------


#' Generate a geofaceted plot for a given grid
#'
#' @param data The dataset. Note: should contain a code column
#'  that correspond to the code columns of the grid.
#' @param .count A character vector indicating which type of data should
#' be ploted. Can take the values cases, deaths, cases_per_100000 
#' and deaths_per_million.
#' @param continent The name of the continent (or region) to be 
#' pasted in the title.
#' @param my_grid A dataframe containing the grid for geofacetting 
#' the countries. Columns typically are row, col, code, names.
#' @param scales A character vector indicating whether the y-scales 
#' should be free or fixed.
#' @param angle Integer for the x-axis angle.
#' @param label_size An integer that defines the text size in 
#' facet lables
#' @param data_source A string that describe the source of data
#' (to be used in the caption)
#' @param colour_raw A string to define the colour of raw data
#'
#'
#' @return A geofaceted plot (ggplot2 and geofacet)
#' @export
#'
#' @examples
geofacet_plot <- function(data, 
                          .count = c("cases", "deaths", "cases_per_100000",
                                     "deaths_per_million"),
                          continent = "",
                          grid = "africa_countries_grid1",
                          scales = "fixed", 
                          angle = 45,
                          label_size = 7,
                          data_source = "ECDC",
                          colour_raw = "steelblue") {
  
  .count <- match.arg(.count)
  count_label <- .count %>% 
    str_replace_all("_", " ")
  
  data <- data %>% 
    filter(count == .count)
  
  ggplot(data, aes(x = date)) + 
    geom_line(aes(y = value_raw), colour = colour_raw) +
    geom_line(aes(y = value_ma), colour = "grey20", size = 1) +
    # geom_smooth(aes(y = value_raw), 
    # se = FALSE, colour = "grey20") +
    facet_geo(~ code,
              grid = grid,
              label = "name", 
              scales = scales) +
    
    theme_bw() + 
    ylab("") + 
    scale_x_date("", 
                 date_breaks = "1 week", 
                 date_labels = "%W") + 
    theme(strip.text.x = element_text(size = label_size,
                                      face = "bold"), 
          axis.text.x = element_text(angle = angle, 
                                     hjust = 1, 
                                     vjust = 1),
          panel.grid.minor = element_blank()) +
    
    labs(title = glue("COVID-19 {count_label} in {continent}"),
         subtitle = "", 
         caption = glue("Data from {data_source}"))
}



# Sometime the RAM on my computer was not enough to run the function
# on the JHU data in long format, so I made a wide version.
geofacet_plot_wide <- function(data, 
                               .count = "cases",
                               continent = "",
                               grid   = "africa_countries_grid1",
                               scales = "fixed", 
                               angle  = 0,
                               label_size = 7,
                               data_source = "JHU",
                               colour_raw  = "black",
                               colour_ma   = "#f04042",
                               mov_av = FALSE) {
  
  # c("cases", "deaths", "cases_per_100000",
  #   "deaths_per_million")
  
  # .count <- match.arg(.count)
  
  count_label <- .count %>%
    str_replace_all("_", " ")
  
  
  data$value_raw <- data[[.count]]
  
  
  # If one wants to add moving average to the raw data
  if (isTRUE(mov_av)) {
    data <- data %>% 
      group_by(iso_a3) %>% 
      arrange(date) %>% 
      mutate(value_ma = slide_dbl(value_raw, 
                                  mean, 
                                  .before = 7, 
                                  .after = 7,
                                  .complete = TRUE)) %>% 
      ungroup() 
  }
  
  
  
  plot <- ggplot(data, aes(x = date)) +
    # Raw data
    geom_line(aes(y = value_raw), 
              # size = 1,
              colour = colour_raw) +
    
    # geom_smooth(aes(y = value_raw), 
    # se = FALSE, colour = "grey20") +
    facet_geo(~ code,
              grid = grid,
              label = "name", 
              scales = scales) +
    
    theme_bw() + 
    ylab("") + 
    scale_x_date("", 
                 date_breaks = "1 week", 
                 date_labels = "%W") + 
    theme(strip.text.x = element_text(size = label_size,
                                      face = "bold"), 
          axis.text.x = element_text(angle = angle, 
                                     hjust = 1, 
                                     vjust = 1),
          panel.grid.minor = element_blank()) +
    
    labs(title = glue("COVID-19 {count_label} in {continent}"),
         subtitle = "", 
         caption = glue("Data from {data_source}"))
  
  
  if (isTRUE(mov_av)) {
    plot <- plot +
      geom_line(aes(y = value_ma), 
                colour = colour_ma, 
                size = 0.5,
                # alpha = 0.7
      )
  }
  
  return(plot)
  
}



#' Generate all graphs for a continent/region
#' 
#' For each region, generate the graphs  for "cases", "deaths", 
#' "cases_per_100000","deaths_per_million", both with fixed and free_y 
#' scales.
#'
#' @param data 
#' @param continent 
#' @param grid 
#' @param names_paths 
#' @param width 
#' @param height 
#' 
#' @inheritParams geofacet_plot
#' @return All graphs for a continent/region
#' @export
#'
#' @examples
geofacet_plot_all <- function(data,
                              continent,
                              grid,
                              names_paths,
                              width = 12,
                              height = 10,
                              label_size = 7,
                              data_source = "JHU",
                              nb_days     = "60d",
                              colour_raw  = "steelblue",
                              colour_ma   = "black",
                              mov_av      = TRUE){
  
  my_count   <- c("cases", "deaths", "cases_per_100000", "deaths_per_million")
  my_scales  <- c("free_y", "fixed")

  # my_count   <- c("cases")
  # my_scales  <- c("free_y", "fixed")
  
  conditions <- tidyr::crossing(my_count, my_scales)
  
  purrr::map2(.x = conditions$my_count,
              .y = conditions$my_scales,
              ~ {
                geofacet_plot_wide(data    = data,
                                   .count  = .x, 
                                   continent = continent,
                                   grid   = grid,
                                   scales = .y,
                                   angle  = 90,
                                   label_size = label_size,
                                   data_source = data_source,
                                   colour_raw  = colour_raw,
                                   colour_ma   = colour_ma,
                                   mov_av      = mov_av)  %>% 
                  
                  ggsave(file = file.path(path.local.geofacet, continent,
                                          glue::glue('{names_paths}_geofacet_{.x}_{.y}_{week_report}_{nb_days}.png')),
                         width  = width, 
                         height = height)
              }
  )
}



