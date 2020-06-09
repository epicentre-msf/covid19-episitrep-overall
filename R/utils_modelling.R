
#' Split dataset into a list of time-series, one for each country (splitting uses the iso_a3 variable)
#' Filter the time-series to a time-frame defined by time_unit_extent
#' Fill the gaps in the time-series, assuming that at missing date, cases and deaths = 0 
#' Smoothing using moving average with a parameterable time-window (in days)
#' Model time-series using a linear regression

linear_model_cnt <- function(series, lst_dta, last_date, time_unit_extent = 12, ma_window = 3, min_sum = 30){
  
  
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
    
    if (dim(dta)[1] > ma_window & sum(dta[series]) > min_sum) {
      
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
  tbl_doubling_time <- linear_doubling_time(tbl_coeffs)
  
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



#' Split dataset in a list of data frames
#' Create a cumulative sum
#' Filter to a time frame defined by time_unit_extent
#' Model data for each country using a linear regression of the cumulative count

linear_model_cml <- function(series, lst_dta, last_date, time_unit_extent = 12, min_sum = 100){
  
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
  tbl_doubling_time <- linear_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_modelled  = dates_extent, 
                         model = 'lm(log(cml) ~ date, data = dta)',
                         minimum_observations_sum = min_sum)))  
}



# Split dataset in a list of data frames and filter to a time frame defined by time_unit_extent
# Model data for each country based on a quasipoisson regression
# (quasipoisson distribution is used mostly because of the zero values)

quasipoisson_model_cnt <- function(series, lst_dta, last_date, time_unit_extent = 12, ma_window = 3, min_sum = 30){
  
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
    
    if (sum(dta[series]) > min_sum) {
      
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
  tbl_doubling_time <- quasipoisson_doubling_time(tbl_coeffs)
  
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

quasipoisson_model_cml <- function(series, lst_dta, last_date, time_unit_extent = 12, min_sum = 100){
  
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
  tbl_doubling_time <- quasipoisson_doubling_time(tbl_coeffs)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              parameters = list(time_unit_modelled  = dates_extent, 
                                minimum_observations_sum = min_sum)))   
}




# DOUBLING TIME

# Calculate linear doubling time
linear_doubling_time <- function(tbl_coeffs) {
  
  df_doubling_time <- tibble(iso_a3 = as.character(), 
                             est    = as.numeric(),
                             lwr    = as.numeric(),
                             upr    = as.numeric())
  
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


# Calculate quasipoisson doubling time
quasipoisson_doubling_time <- function(tbl_coeffs) {
  
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



ts_coeff <- function(series, lst_dta, time_unit_extent = 5, ma_window = 3, min_sum = 30){
  
  lst_coeffs  <- list()
  
  for (j in names(lst_dta)) {
    
    dta <- lst_dta[[j]] %>% 
      select(date, continent, region, country, country_ecdc, iso_a3, series) %>% 
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
