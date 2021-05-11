
#' Filter the time-series to a time-frame defined by time_unit_extent
#' Fill the gaps in the time-series, assuming that at missing date, cases and deaths = 0 
#' Smoothing using moving average with a parameterable time-window (in days)
#' Model time-series using a linear regression

linear_model_cnt <- function(series, 
                             lst_dta, 
                             last_date, 
                             time_unit_extent = 14, 
                             ma_window = 3, 
                             min_sum = 30){
  
  
  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)

  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]] %>% 
      filter(between(date, dates_extent[1], dates_extent[2])) %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      max(date, na.rm = TRUE), by = 1), 
                      fill = list(cases = NA_real_, deaths = NA_real_))
    
    if (dim(dta)[1] > ma_window & sum(dta[series], na.rm = TRUE) > min_sum) {
      
      dta$ma <- forecast::ma(dta[series], order = ma_window)
      dta$ma <- na_if(dta$ma, 0) # Replace 0 values as NA
      
      mdl <- lm(log(ma) ~ date, data = dta)
      
      # Matrix of predictions
      mdl_preds <- matrix(data = NA, 
                          nrow = dim(dta)[1], 
                          ncol = 3, 
                          dimnames = list(c(1:dim(dta)[1]), c('fit', 'lwr', 'upr')))
      
      preds <- exp(predict(mdl, interval = 'confidence'))
      
      matched_rows <- match(rownames(preds), rownames(mdl_preds))
      matched_cols <- match(colnames(preds), colnames(mdl_preds))
      mdl_preds[matched_rows, matched_cols] <- preds
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
      
    } else {
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds %>% as_tibble()
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)

  }
  
  # Calculate doubling time
  tbl_doubling_time <- make_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs, 
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_sourced  = dates_extent, 
                         time_unit_modelled = time_unit_extent - (ma_window - 1), 
                         model = 'lm(log(ma) ~ date, data = dta)', 
                         moving_average_extent = ma_window, 
                         minimum_observations_sum = min_sum)))
}



#' Create a cumulative sum
#' Filter to a time frame defined by time_unit_extent
#' Model data for each country using a linear regression of the cumulative count

linear_model_cml <- function(series, lst_dta, 
                             last_date, 
                             time_unit_extent = 14, 
                             min_sum = 100){
  
  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)
  
  lst_dta <- base::lapply(lst_dta, function(x) {cbind(x, cml = cumsum(x[[series]]))})
  
  lst_dta <- base::lapply(lst_dta, function(x) {x %>% filter(between(date, dates_extent[[1]], dates_extent[[2]]))})
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]]
    dta$cml[1] <- ifelse(dta$cml[1] == 0, 0.001, dta$cml[1])
    
    if (dta[['cml']][nrow(dta)] > min_sum) {
      mdl <- lm(log(cml) ~ date, data = dta) 
      
      mdl_preds <- tibble(exp(data.frame(predict(mdl, interval = 'confidence'))))
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(prd = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- make_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_modelled  = dates_extent, 
                         model = 'lm(log(cml) ~ date, data = dta)',
                         minimum_observations_sum = min_sum)))  
}



# Filter to a time frame defined by time_unit_extent
# Model data for each country based on a quasipoisson regression
# (quasipoisson distribution is used mostly because of the zero values)

quasipoisson_model_cnt <- function(series, 
                                   lst_dta, 
                                   last_date, 
                                   time_unit_extent = 14, 
                                   ma_window = 3, 
                                   min_sum = 30){
  
  # The Model (quasipoisson regression)

  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]] %>% 
      filter(between(date, dates_extent[1], dates_extent[2])) %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      max(date, na.rm = TRUE), by = 1), 
                      fill = list(cases = NA_real_, deaths = NA_real_))

    dta <- lst_dta[[i]]
    
    if (sum(dta[series], na.rm = TRUE) > min_sum) {
      
      dta$obs <- dta[[series]]
      
      mdl <- glm(obs ~ date, family = quasipoisson(link = 'log'), data = dta)
      
      mdl_fit <- predict(mdl, type = 'link', se.fit = TRUE)
      
      mdl_preds  <- tibble(fit = exp(mdl_fit$fit), 
                           lwr = exp(mdl_fit$fit - (1.96 * mdl_fit$se.fit)), 
                           upr = exp(mdl_fit$fit + (1.96 * mdl_fit$se.fit)))
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])

    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- make_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs, 
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_sourced  = dates_extent, 
                         model = 'glm(obs ~ date, family = quasipoisson(link = "log"), data = dta)', 
                         minimum_observations_sum = min_sum)))
}



#' Split dataset in a list of data frames
#' Create a cumulative sum
#' Filter to a time frame defined by time_unit_extent
#' Model data for each country
#' smoodthing using a predefined window 
#' Then making a linear regression

quasipoisson_model_cml <- function(series, lst_dta, 
                                   last_date, 
                                   time_unit_extent = 14, 
                                   min_sum = 100){
  
  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)
  
  lst_dta <- base::lapply(lst_dta, function(x) {cbind(x, cml = cumsum(x[[series]]))})
  
  lst_dta <- base::lapply(lst_dta, function(x) {x %>% filter(between(date, dates_extent[[1]], dates_extent[[2]]))})
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]]
    
    if (dta[['cml']][nrow(dta)] > min_sum) {
      
      if (dta[['cml']][1] == dta[['cml']][nrow(dta)]) {
        
        mdl <- glm(cml ~ date, family = poisson(), data = dta)
        
        mdl_fit <- predict(mdl, se.fit = TRUE)
        
        mdl_preds  <- tibble(fit = exp(mdl_fit$fit), 
                             lwr = exp(mdl_fit$fit - (1.96 * mdl_fit$se.fit)), 
                             upr = exp(mdl_fit$fit + (1.96 * mdl_fit$se.fit)))
        
        mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                             lwr   = confint(mdl)[2,1], 
                             upr   = confint(mdl)[2,2])
      } else {
      
        mdl <- glm(cml ~ date, family = quasipoisson(), data = dta)
        
        mdl_fit <- predict(mdl, se.fit = TRUE)
        
        mdl_preds  <- tibble(fit = exp(mdl_fit$fit), 
                             lwr = exp(mdl_fit$fit - (1.96 * mdl_fit$se.fit)), 
                             upr = exp(mdl_fit$fit + (1.96 * mdl_fit$se.fit)))
        
        mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                             lwr   = confint(mdl)[2,1], 
                             upr   = confint(mdl)[2,2])
      }
      
    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
      
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- make_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              parameters = list(time_unit_modelled  = dates_extent, 
                                minimum_observations_sum = min_sum)))   
}




# DOUBLING TIME

make_doubling_time <- function(tbl_coeffs) {
  
  df_doubling_time <- tibble(iso_a3 = character(), 
                             est = numeric(), 
                             lwr = numeric(), 
                             upr = numeric())
  
  for (i in tbl_coeffs$iso_a3) {
    
    row_coeffs <- tbl_coeffs %>% filter(iso_a3 == i)
    
    if (!is.na(row_coeffs$coeff)) {
      est <- log(2)/row_coeffs$coeff
      lwr <- log(2)/row_coeffs$upr
      upr <- log(2)/row_coeffs$lwr
    } else {
      est <- NA_real_
      lwr <- NA_real_
      upr <- NA_real_
    }
    
    df_doubling_time <- df_doubling_time %>% 
      add_row(iso_a3 = i, 
              est = est,
              lwr = lwr,
              upr = upr)
  }
  return(df_doubling_time)
}



ts_coeff <- function(series, 
                     lst_dta, 
                     time_unit_extent = 5, 
                     ma_window = 3, 
                     min_sum = 30){
  
  lst_coeffs  <- list()
  
  for (j in names(lst_dta)) {
    
    dta <- lst_dta[[j]] %>% 
      select(date, continent, region, country, iso_a3, series) %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      max(date, na.rm = TRUE), by = 1), 
                      fill = list(series = NA_real_))
    
    seq_dates <- if(length(dta$date) < time_unit_extent) {
      min(dta$date) 
    } else {
      seq.Date(min(dta$date, na.rm = TRUE) + (time_unit_extent - 1) / 2, 
               max(dta$date, na.rm = TRUE) - (time_unit_extent - 1) / 2, 
               by = 1)
    }
    
    dta <- dta %>% 
      mutate(
        mov_av = as.double(forecast::ma(dta[series], order = ma_window)) %>% 
          na_if(., 0)) # Replace 0 values with NA
    
    tbl_coeffs <- tibble(date  = as.character(), 
                         coeff = numeric(), 
                         lwr   = numeric(), 
                         upr   = numeric())
    
    for (i in as.character(seq_dates)) {
      
      temp <- dta %>% 
        filter(between(date, 
                       as.Date(i) - (time_unit_extent - 1) / 2, 
                       as.Date(i) + (time_unit_extent - 1) / 2))
      
      if (sum(!is.na(temp['mov_av'])) > 2 & sum(temp['mov_av'], na.rm = TRUE) > min_sum) {
        mdl  <- lm(log(mov_av) ~ date, data = temp)
        mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                             lwr   = confint(mdl)[2,1], 
                             upr   = confint(mdl)[2,2])
      } else {
        mdl_coeffs <- tibble(coeff = NA_real_, 
                             lwr   = NA_real_, 
                             upr   = NA_real_)
      }
      
      tbl_coeffs <- tbl_coeffs %>% 
        add_row(date = i, mdl_coeffs)
      
    }
    
    dta_coeff <- left_join(dta,  tbl_coeffs %>% mutate(date = as.Date(date)))
    
    lst_coeffs[[j]] <- dta_coeff
    
  }
  return(lst_coeffs)
}




get_trend_models <- function(lst_jhu  = lst_dta_jhu, 
                             date_max = date_max_report, 
                             periods_trends = c(14, 30), 
                             local_path     = path.local.worldwide.data, 
                             file_name   = 'trends_models.RDS', 
                             last_update = last_update_dta_jhu, 
                             force       = FALSE) {
  
  make_new_models <- ifelse(!file.exists(file.path(local_path, file_name)) | force, TRUE, FALSE)
  
  
  if (make_new_models) {
    
    # <!-- Modelling Cases Trends -->
    
    model_cnt_cases_linear_short  <- linear_model_cnt(series = 'cases', 
                                                      lst_dta = lst_jhu, 
                                                      last_date = date_max, 
                                                      time_unit_extent = periods_trends[1], 
                                                      min_sum = 30)
    
    model_cnt_cases_linear_long   <- linear_model_cnt(series = 'cases', 
                                                      lst_dta = lst_jhu, 
                                                      last_date = date_max, 
                                                      time_unit_extent = periods_trends[2], 
                                                      min_sum = 30)
    
    # <!-- Modelling Deaths Trends -->
    
    model_cnt_deaths_linear_short  <- linear_model_cnt(series = 'deaths', 
                                                       lst_dta = lst_jhu, 
                                                       last_date = date_max, 
                                                       time_unit_extent = periods_trends[1], 
                                                       min_sum = 30)
    
    model_cnt_deaths_linear_long   <- linear_model_cnt(series = 'deaths', 
                                                       lst_dta = lst_jhu, 
                                                       last_date = date_max, 
                                                       time_unit_extent = periods_trends[2], 
                                                       min_sum = 30)
    

    lst_coeffs_cases <- ts_coeff(series = 'cases', 
                                 lst_dta = lst_dta_jhu, 
                                 time_unit_extent = 5, 
                                 ma_window = 3, 
                                 min_sum = 30)
    
    lst_coeffs_deaths <- ts_coeff(series = 'deaths', 
                                  lst_dta = lst_dta_jhu, 
                                  time_unit_extent = 5, 
                                  ma_window = 3, 
                                  min_sum = 30)
    
    
    mdls <- list("model_cnt_cases_linear_short"  = model_cnt_cases_linear_short, 
                 "model_cnt_cases_linear_long"   = model_cnt_cases_linear_long, 
                 "model_cnt_deaths_linear_short" = model_cnt_deaths_linear_short, 
                 "model_cnt_deaths_linear_long"  = model_cnt_deaths_linear_long,
                 "lst_coeffs_cases"  = lst_coeffs_cases, 
                 "lst_coeffs_deaths" = lst_coeffs_deaths, 
                 "periods_trends" = c("short" = periods_trends[1], "long" = periods_trends[2]))
    
    # <!-- Save models in a unique RDS -->
    
    saveRDS(mdls, file = file.path(path.local.worldwide.data, file_name)) 
    
  } else {
    
    mdls <- readRDS(file = file.path(path.local.worldwide.data, file_name))
    
  }
  
  return(mdls)
  
}


ts_coeff_single <- function(dta, series = "cases", 
                            time_unit_extent = 5, 
                            ma_window = 3, 
                            min_sum = 30){
  
  dta <- dta %>% 
    tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                    max(date, na.rm = TRUE), by = 1), 
                    fill = list(series = NA_real_))
  
  seq_dates <- if(length(dta$date) < time_unit_extent) {
    min(dta$date) 
  } else {
    seq.Date(min(dta$date, na.rm = TRUE) + (time_unit_extent - 1) / 2, 
             max(dta$date, na.rm = TRUE) - (time_unit_extent - 1) / 2, 
             by = 1)
  }
  
  dta <- dta %>% 
    mutate(
      mov_av = as.double(forecast::ma(dta[series], order = ma_window)) %>% 
        na_if(., 0)) # Replace 0 values with NA
  
  tbl_coeffs <- tibble(date  = as.character(), 
                       coeff = numeric(), 
                       lwr   = numeric(), 
                       upr   = numeric())
  
  for (i in as.character(seq_dates)) {
    
    temp <- dta %>% 
      filter(between(date, 
                     as.Date(i) - (time_unit_extent - 1) / 2, 
                     as.Date(i) + (time_unit_extent - 1) / 2))
    
    if (sum(!is.na(temp['mov_av'])) > 2 & sum(temp['mov_av'], na.rm = TRUE) > min_sum) {
      mdl  <- lm(log(mov_av) ~ date, data = temp)
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
    } else {
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(date = i, mdl_coeffs)
    
  }
  
  dta_coeff <- left_join(dta,  tbl_coeffs %>% mutate(date = as.Date(date)))
  
  return(dta_coeff)
}



plot_coeff <- function(dta, name, series = "cases") {
  
  name_country <- name
  
  quo_series <- sym(series)
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  date_min <- min(dta$date, na.rm = TRUE)
  date_max <- max(dta$date, na.rm = TRUE)
  
  plot_crv <- ggplot(dta, aes(x = date, y = !!quo_series)) + 
    geom_col(colour = main_colour,  fill = main_colour) + 
    scale_y_continuous(labels = scales::label_number_si()) +
    xlim(c(date_min, date_max)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = glue('Number of {series} reported')) + 
    theme_light()
  
  plot_cff <- ggplot(dta, aes(x = date)) +
    geom_hline(yintercept = 0, colour = "red", alpha = .6) +
    geom_line(aes(y = coeff), colour = '#1B9E77', size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
    xlim(c(date_min, date_max)) + 
    #scale_x_date(date_breaks = "4 weeks", date_labels = "%d %b") + 
    xlab(NULL) + 
    ylab('Slope coefficient') + 
    labs(subtitle = 'Slope coefficient curve') + 
    theme_light()
  
  grid.arrange(rbind(ggplotGrob(plot_crv), ggplotGrob(plot_cff)), 
               top = textGrob(glue('Evolution of the slope cofficient in {name_country}'), 
                              gp = gpar(fontface = 'bold')))
  
}





# linear_trend_deepdive <- function(dta, 
#                                   series, 
#                                   last_date, 
#                                   n_days, 
#                                   ma_window = 3, 
#                                   min_sum = 30){
#   
#   dta <- dta %>% select(date, cnt = all_of(series))
#   
#   dates_extent <- c(last_date - (n_days - 1), last_date)
#   
#   dta <- dta %>% 
#     filter(between(date, dates_extent[1], dates_extent[2])) %>% 
#     tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
#                                     max(date, na.rm = TRUE), by = 1), 
#                     fill = list(cnt = NA_real_))
#   
#   
#   # Moving average
#   dta$ma <- forecast::ma(dta$cnt, order = ma_window)
#   dta$ma <- na_if(dta$ma, 0) # Replace 0 values as NA
#   
#   
#   # Empty matrix of predictions
#   m_preds <- matrix(data = NA, 
#                     nrow = dim(dta)[1], 
#                     ncol = 3, 
#                     dimnames = list(c(1:dim(dta)[1]), c('fit', 'lwr', 'upr')))
#   
#   
#   # Run model with conditions
#   if (dim(dta)[1] > ma_window & sum(dta$cnt, na.rm = TRUE) > min_sum) {
#     
#     mdl <- lm(log(ma) ~ date, data = dta)
#     
#     preds <- exp(predict(mdl, interval = 'confidence'))
#     
#     matched_rows <- match(rownames(preds), rownames(m_preds))
#     matched_cols <- match(colnames(preds), colnames(m_preds))
#     m_preds[matched_rows, matched_cols] <- preds
#     
#     tbl_preds <- tibble(date = seq.Date(from = dates_extent[1], to = dates_extent[2], by = 1), 
#                         cnt  = dta$cnt, 
#                         ma   = dta$ma, 
#                         fit  = as.double(m_preds[, 'fit']), 
#                         lwr  = as.double(m_preds[, 'lwr']), 
#                         upr  = as.double(m_preds[, 'upr']))
#     
#     mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
#                          lwr   = confint(mdl)[2,1], 
#                          upr   = confint(mdl)[2,2])
#     
#   } else {
#     mdl <- NA_character_
#     
#     tbl_preds <- tibble(date = seq.Date(from = dates_extent[1], to = dates_extent[2], by = 1), 
#                         cnt  = dta$cnt, 
#                         ma   = dta$ma, 
#                         fit  = as.double(m_preds[, 'fit']), 
#                         lwr  = as.double(m_preds[, 'lwr']), 
#                         upr  = as.double(m_preds[, 'upr']))
#     
#     mdl_coeffs <- tibble(coeff = NA_real_, 
#                          lwr   = NA_real_, 
#                          upr   = NA_real_)
#   }
#   
#   return(list(mdl = mdl, preds = tbl_preds, coeffs = mdl_coeffs))
# }



#' Get model prediction deepdive
#'
#' @param df a list of dataframes (on df for each country)
#' @param time_unit_extent the number of days to include
#' @param min_sum what should be the minimum number of cases
#' to run the model
#' @param ma_window the number of days on which to calculate
#' the moving average 
#' @param var cases or deaths?
#'
#' @return
#' @export
#'
#' @examples
get_preds <- function(df,
                      time_unit_extent = 30,
                      min_sum = 30,
                      ma_window = 3){
  
  
  # filter data to date range of interest
  last_date <- max(df$date, na.rm = TRUE) - 2
  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)
  
  df1 <- df %>%
    dplyr::filter(dplyr::between(date, dates_extent[1], dates_extent[2])) %>%
    tidyr::complete(
      date = seq.Date(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = 1),
      fill = list(cases = NA_real_, deaths = NA_real_)
    )
  
  # Compute moving average
  df1 <- df1 %>% 
    group_by(country) %>% 
    mutate(ma0 = as.numeric(forecast::ma(cases, order = ma_window)),
           ma = dplyr::na_if(ma0, 0)) %>% 
    ungroup()  
  
  
  
  if (nrow(df1) > ma_window & sum(df1[[var]], na.rm = TRUE) > min_sum) {
    
    # Run linear model and get predicated values and confidence intervals
    df2 <- df1 %>% 
      nest(data = -country) %>% 
      mutate(fit = map(data, ~lm(log(ma) ~ date, data = .)),
             augmented = map(fit, broom::augment, interval = 'confidence')) %>% 
      unnest(augmented) %>% 
      mutate(exp_fitted = exp(.fitted),
             exp_lower = exp(.lower),
             exp_upper = exp(.upper),
             check_counts_ma = exp(`log(ma)`)) %>%
      select(-fit, -data,
             -starts_with("."), -`log(ma)`)
  } else {
    
    df2 <- df1 %>% 
      nest(data = -c(country, date)) %>% 
      select(-data) %>% 
      mutate(exp_fitted  = NA_real_, 
             exp_lower  = NA_real_,
             exp_upper   = NA_real_,
             check_counts_ma  = NA_real_) %>% 
      select(country, date, everything())
  }
  
  return(df2)
}

