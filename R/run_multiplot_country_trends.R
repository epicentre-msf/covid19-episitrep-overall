
library(covidutils)
library(purrr)
library(dplyr)
library(ggplot2)

path.local.worldwide.graphs.country_trends <- fs::path(path.local.worldwide.graphs, "country_trends")
dir.create(path.local.worldwide.graphs.country_trends, showWarnings = FALSE, recursive = TRUE)

df_jhu <- covidutils::get_owid_jhcsse() %>% 
  filter(between(date, left = date_min_report, right = date_max_report))

df_trends <- covidutils::get_country_summaries(df_jhu, omit_past_days = 0)

iso_codes <- sort(unique(df_jhu$iso_a3))

iso_codes %>% 
  walk(~{
    cat(paste0(.x, "\n"))
    df_country <- df_jhu %>% filter(iso_a3 == .x)
    df_trends_country <- df_trends %>% filter(iso_a3 == .x)
    if (nrow(df_trends_country) > 0) {
      country_lab <- df_trends_country$country
      # only run if there's been >= 1 case in last 2 weeks
      if (df_trends_country$cases_14d > 0) {
        safe_plot <- purrr::safely(covidutils::country_plot)
        p <- safe_plot(
          df_country, 
          df_trends_country, 
          title_curve = "Since first cases reported",
          cases_lab = "Cases",
          deaths_lab = "Deaths",
          add_title = TRUE
        )
        if (!is.null(p$result)) {
          ggsave(
            fs::path(path.local.worldwide.graphs.country_trends, glue::glue("{.x}_{country_lab}_trends"), ext = "png"), 
            p$result, 
            dpi = 320, 
            width = 8, 
            height = 4
          )
        } else {
          print(p$error)
        }
      } else {
        cat("no cases in last 14 days\n")
      }
    } else {
      cat("no trends available\n")
    }
  })

if (0) {
  iso <- "FRA"
  df_country <- df_jhu %>% filter(iso_a3 == iso)
  df_trends_country <- df_trends %>% filter(iso_a3 == iso)
  safe_plot <- purrr::safely(covidutils::country_plot, otherwise = "failed")
  p <- safe_plot(df_country, df_trends_country, add_title = TRUE)
  p$result
}