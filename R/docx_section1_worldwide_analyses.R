if (Sys.getlocale(category = "LC_TIME") == "French_France.1252") {
  Sys.setlocale(category = "LC_TIME", locale = "English_United Kingdom.1252")
  Sys.setenv(LANG = "en_GB.UTF-8") 
}


# --- --- --- --- --- --- --- 
# I. Worldwide analysis ------------------------------------
# --- --- --- --- --- --- --- 

# Heading 1
# --- --- --- --- --- --- --- 
my_doc <- add_heading1(heading_text = 'Worldwide analysis') 



# --- --- --- --- --- --- --- 
# COUNT CASES ---------------
# --- --- --- --- --- --- --- 

## - Heading 2
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 cases')
my_doc <- add_end_section_continuous()


## - Text 
# --- --- --- --- --- --- --- 

## Total cases and deaths worldwide
my_doc <- add_par_normal(
  sprintf("As of the %s, %s Covid-19 cases and %s Covid-19 associated deaths were reported worldwide. The number of cases and deaths in the last %s days were %s and %s respectively.", 
          format(max(dta_jhu$date), "%d %B %Y"), 
          format(sum(dta_jhu$cases, na.rm = TRUE), big.mark = ","), 
          format(sum(dta_jhu$deaths, na.rm = TRUE), big.mark = ","), 
          period_trend, 
          n_cases_12d, 
          n_deaths_12d))

# 2
#my_doc <- add_par_normal(
#  sprintf('%s countries (%s) reported more than 100 thousand cases. The United States rached the 3 million cases threshold, and Brazil has crossed the 1.5 million mark. India is now third in number of cases reported with more than 700 thousand cases. %s African countries reported less than 50 cases (%s). %s reported more than 1,000 cases (%s) (Figure 1). The pandemic continues to accelerate with X.X million cases (XX%) and XXK deaths (XX%) of the total cases reportend in the last 12 days.', 
#          tbl_cases_count %>% call_countries_with(100000, Inf, "cases") %>% length() %>% Words(), 
#          tbl_cases_count %>% call_countries_with(100000, Inf, "cases") %>% combine_words(), 
#          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(0, 50, "cases") %>% length() %>% Words(), 
#          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(0, 50, "cases") %>% combine_words(), 
#          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(1000, Inf, "cases") %>% length() %>% Words(), 
#          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(1000, Inf, "cases") %>% combine_words()))



## Number of countries with increasing trend (12 days)
my_doc <- add_par_normal(
  sprintf('%s countries reported an increasing trend (compared to XXX last month) (Figure 1) (see ', 
  call_countries_increasing('cases') %>% length() %>% Words())) %>% 
  
  slip_in_text(style = 'Hyperlink',
               str = "html report",
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/addtional_episitrep_outputs_worldwide/") %>%
  
  slip_in_text(style = 'Normal char',
               str = '). Trends calculated on the last 30 days are available in the full worldwide analysis report.')
  

my_doc <- add_end_section_2columns()


## Number of countries with increasing trend per continent (12 days)
my_doc <- add_par_normal(
  sprintf('%s countries had an increasing trend in Africa (%s), %s countries in Asia (%s), %s countries in the Americas (%s), %s countries in Europe (%s).', 
  call_countries_increasing("cases", "Africa") %>% length() %>% Words(), 
  call_countries_increasing("cases", "Africa") %>% combine_words(), 
  call_countries_increasing("cases", "Asia") %>% length(), 
  call_countries_increasing("cases", "Asia") %>% combine_words(), 
  call_countries_increasing("cases", "Americas") %>% length(), 
  call_countries_increasing("cases", "Americas") %>% combine_words(),
  call_countries_increasing("cases", "Europe") %>% length(), 
  call_countries_increasing("cases", "Europe") %>% combine_words()))


# Countries with increasing trend in the 12 last days, and more
#  than 10 000 cases within the 12 last days. 
#  World + Africa
my_doc <- add_par_normal(
  sprintf("%s reported increasing trend while notifying over 10,000 cases in the last 12 days.
          In Africa, %s reported increasing trend and more than 1,000 cases.", 
          
          tbl_countries_increasing_cases %>% 
            filter(trend == "Increasing", cases_12d >= 10000) %>% 
            arrange(desc(coeff)) %>% pull(country) %>% combine_words(),
          
          tbl_countries_increasing_cases %>% 
            filter(trend == "Increasing", cases_12d >= 1000, continent == "Africa") %>% 
            arrange(desc(coeff)) %>% pull(country) %>% combine_words()
          )
  )


my_doc <- add_end_section_2columns()




## - Map Cases count and trend
my_doc <- add_figure_map_world_grid(
  object_name  = paste0('map_world_case_count_trend_grid', '_', week_report, '.png'),
  figure_title = glue("Mapping of Covid-19 cases counts and trends, period from {format(date_max_report - (period_trend - 1), '%d %B %Y')} to {format(date_max_report, '%d %B %Y')} ({period_trend} days)"))

my_doc <- add_end_section_continuous()



# 5 - Cumulative incidence worldwide
my_doc <- add_par_normal(
  sprintf("Since the beginning of the epidemic, countries presenting the highest cumulative incidences are in North and South America, Europe, and Middle East (Figure 2)."))



## Country reaching incidence thresholds within last month
## Thresshold 10 00 or 100 000

# Situation 30 days ago
tbl_inc_cum_30d_ago <- dta_jhu %>% 
  group_by(iso_a3, country, continent, region) %>% 
  arrange(date) %>% 
  filter(between(date, 
                 left = date_min_report,
                 right = date_max_report - 29)) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE)) %>% 
  left_join(df_pop_country %>% select(iso_a3, pop)) %>% 
  mutate(
    cases_ip_before = cases / pop * 100000)

country_over1000_cases_30d <- tbl_inc_prop %>% 
  select(iso_a3 : country, 
         case_ip_now = cases_ip) %>% 
  left_join(select(tbl_inc_cum_30d_ago, iso_a3, cases_ip_before), 
            by = "iso_a3") %>% 
  filter(cases_ip_before < 1000,
         case_ip_now >= 1000) 
 

my_doc <- add_par_normal(
  sprintf("During the last month, %s reached the threshold of 1,000 confirmed cases per 100,000 population (1%%).", 
          country_over1000_cases_30d %>% pull(country) %>% combine_words()
          )
  )



## - Map Case Incidence
my_doc <- add_figure_map_world(
  object_name  = glue("map_world_case_attack_rates_{week_report}.png"), 
  figure_title = glue("Cumulative incidence of Covid-19 reported cases since beginning of epidemic, per 100,000 population"),
  width = 7 * cm_to_in, 
  height = 4.5 * cm_to_in
  )

my_doc <- add_end_section_2columns(widths = c(7 * cm_to_in, 10 * cm_to_in))



# --- --- --- --- --- --- --- 
# COUNT DEATHS ---------
# --- --- --- --- --- --- --- 

## - Heading 
# --- --- --- --- --- --- --- 
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 associated deaths') 

my_doc <- add_end_section_continuous()

## - Text
# --- --- --- --- --- --- --- 

## 1 Cumulative deaths (general)
my_doc <- add_par_normal(
sprintf("Countries that reported the highest numbers of Covid-19 associated deaths are the USA, Brazil, India, Mexico as well as some countries in Europe, South America and Middle-East."))



## Countries crossing cumulative death threshold in last month
## Threshold 10 000 and 100 000.

# Situation 30 days ago (cumulative deaths per country)
deaths_30d_ago <- dta_jhu %>% 
  group_by(iso_a3, country, continent, region) %>% 
  arrange(date) %>% 
  filter(between(date, 
                 left = date_min_report,
                 right = date_max_report - 29)) %>% 
  summarise(cum_deaths_30d_ago = sum(deaths, na.rm = TRUE)) 


tbl_death_count_thresholds <- tbl_death_count %>% 
  select(iso_a3, cum_deaths_today = deaths) %>% 
  left_join(deaths_30d_ago, by = "iso_a3") %>% 
  mutate(
    threshold_10000 = case_when(
      cum_deaths_30d_ago < 10000 & cum_deaths_today > 10000 ~ TRUE,
      TRUE ~ FALSE),
    threshold_100000 = case_when(
      cum_deaths_30d_ago < 100000 & cum_deaths_today > 100000 ~ TRUE,
      TRUE ~ FALSE)
  )


list_countries_100000 <- tbl_death_count_thresholds %>% 
  filter(threshold_100000) %>% pull(country) 

list_countries_100000 <- ifelse(length(list_countries_100000) == 0, 
                                "no country", 
                                combine_words(list_countries_100000))

list_countries_10000 <- tbl_death_count_thresholds %>% 
  filter(threshold_10000) %>% pull(country) 

list_countries_10000 <- ifelse(length(list_countries_10000) == 0, 
                               "no country", 
                               combine_words(list_countries_10000))


my_doc <- add_par_normal(
  sprintf("In the past month, %s crossed the 100,000 Covid19 associated deaths mark, and %s crossed the 10,000 death mark (Figure 3 - Deaths count).",
          list_countries_100000,
          list_countries_10000))


## Number of countries witn increasing death trend
my_doc <- add_par_normal(
  sprintf("%s countries reported an increasing trend in death compared to four weeks ago (Figure 3 – Trends in deaths).", 
  length(call_countries_increasing('deaths')) %>% Words()))



# 3
my_doc <- my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = 'The full table of cases, deaths and trends can be found ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/tables_world_summary/") %>% 
  slip_in_text(style = 'Normal char', 
               str = ".") 

my_doc <- add_end_section_2columns()


## - Map trend in deaths
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_death_count_trend_grid_{week_report}.png'), 
  figure_title = glue('Mapping of Covid-19 associated deaths counts and trends, period from {format(date_max_report - (period_trend - 1), "%d %B %Y")} to {format(date_max_report, "%d %B %Y")} (12 days)'))




# --- --- --- --- --- --- --- 
# DOUBLING TIME -----------------
# --- --- --- --- --- --- --- 

## - Heading
my_doc <- add_heading2(heading_text = 'Doubling time in cases and deaths') 
my_doc <- add_end_section_continuous()

## - Text
# --- --- --- --- --- --- --- 
# 1

#my_doc <- my_doc %<>% 
#  body_add_par(style = 'Normal', 
#               value = "Doubling time calculation should take into account that most countries now reached an important number of cases,
#and sometimes are going through a second surge in cases. Therefore, doubling time calculation is now only considering cases and deaths reported in the last 12 days, instead of cumulative cases (method ") %>% 
#  slip_in_text(style = 'Hyperlink', 
#               str = "here", 
#               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/analysis_methods_2020-04-28.html") %>% 
#  slip_in_text(style = 'Normal char', 
#               str = ").")

my_doc <- my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = "Doubling time calculation is now only considering cases and deaths reported in the last 12 days, instead of cumulative cases (methods ") %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://reports.msf.net/secure/app_direct/covid19-additional-analysis/") %>% 
  slip_in_text(style = 'Normal char', 
               str = "), in order to account for high case numbers reached, and possible second surge in cases.") 



my_doc <- add_par_normal(
  sprintf("As a result, a sharp increasing of cases (doubling time of less than 12 days) is observed in %s countries this week, compared to XXX four weeks ago (Figure 4 and Table 1).", 
          length(call_countries_doubling('cases_est'))))

## Countries with doubling time in cases
## 
my_doc <- add_par_normal(
  sprintf("This week %s reported a doubling time in cases of less than 8 days. The other countries with a doubling time in cases of less than 12 days are %s.",
          combine_words(call_countries_doubling('cases_est', threshold = 8)), 
          combine_words(setdiff(call_countries_doubling('cases_est', threshold = 12),
                                call_countries_doubling('cases_est', threshold = 8)))))

# 3
my_doc <- add_par_normal(
  sprintf("%s reported a worrying increase in deaths.", 
          combine_words(call_countries_doubling('deaths_est', threshold = 12))))


my_doc <- add_end_section_2columns()



## - Map
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_doubling_grid_{week_report}.png'),
  figure_title = glue("Doubling time of cases and associated deaths estimated in the last {period_trend} days (only countries with increasing trends are displayed)"),
  width = 18.11, 
  height = 7.77)

# my_doc <- add_par_normal('')


## - Table
my_doc <- add_table(
  object_name = glue("gtbl_cfr_doubling_rank_{week_report}.png"), 
  table_title = glue("Countries with estimated doubling time of cases or deaths of less than {threshold_doubling_time} days"), 
  folder = "worldwide",
  width = 11.55 * cm_to_in, 
  height = 13.5 * cm_to_in)

my_doc <- add_end_section_continuous()

# 5
#my_doc <- add_par_normal(
#  sprintf("The following graph presents the relationship between current doubling time and time since the first significant increasing trend was reported."))

# 6
#my_doc <- add_par_normal(
#  sprintf("Countries represented in the lower right hand corner are those which are in the most worrying situation, representing a prolonged time with an increasing trend."))

# 7
#my_doc <- add_par_normal(
#  sprintf("Currently there is no country with a doubling time of less than 8 days. However it is of interest that NAME OF THE COUNTRIES are reporting an increasing trend for over 35 days (without discontinuing)."))

# 8
#my_doc <- add_par_normal(
#  sprintf("While Iraq (to check) still displays a increasing over 80 days after reporting the first increasing trend, it appears that this sharp increase in case in the country only started around mid-May, after a first small wave in cases in March/April."))

#my_doc <- add_end_section_2columns()

# Graph dots
#my_doc <- add_figure(
#  object_name = glue('dots_hotspot_cases_{week_report}.png'), 
#  figure_title = glue('Distribution of doubling time of Covid-19 cases according to length in days since the first significant increasing trend reported (only countries with doubling time of less than {threshold_doubling_time} days as of {format(date_max_report, "%d %B %Y")} are displayed)'), 
#  folder = 'worldwide')


# --- --- --- --- --- --- --- 
# TESTING
# --- --- --- --- --- --- --- 

# ## - Heading
# my_doc <- add_heading2(heading_text = 'Testing') 
# my_doc <- add_end_section_continuous()
# 
# # Text 
# # --- --- --- --- 
# # 1
# my_doc <- add_par_normal(
#   sprintf("A better insight into countries’ testing capacity would allow for a better interpretation of reported cases relative to actual ones. Figure 5 displays the relationship between the proportion of positive tests, and the cumulative number of cases per 100,000 population in the country (both in the last 12 days). The countries displayed in the right side of the graphic therefore have a higher proportion of positive tests, which may point to a possibly insufficient sensitivity of the testing strategy. Of these, countries with a recent high number of cases are facing an even more complicated situation. Only countries with a current increasing trend are displayed, being on a comparable stage of the epidemic."))
# 
# # 2
# my_doc <- add_par_normal(
#   sprintf("WHO has set the recommended goal of 5%% of positive samples to ensure acceptable sensitivity. This week ....TO BE COMPLETED. This could indicate that these countries may not detect all cases due to an insufficiently sensitive testing strategy. However, it is of interest that a non-negligible number of countries did not regularly report the number of tests performed, thus leading to potentially overestimated proportions."))
# 
# # 3
# my_doc <- add_par_normal(
#   sprintf("To note that XXXXX displayed a very high proportion of positive tests. These countries, as well as some others which reported high cumulative number of cases in the last 12 days (such as LIST OF COUNTRIES) are to be closely monitored as they are facing an important rise in cases while having possible gaps in testing strategy sensitivity (Figure 5)."))
# 
# my_doc <- add_end_section_2columns()
# 
# # - Graph
# my_doc <- add_figure(
#   object_name = glue('dots_prop_tests_{week_report}.png'), 
#   figure_title = glue('Relation between the number of tests carried out in the last 12 days (as proportion of positive tests) and the cumulative number of cases reported'), 
#   folder = 'worldwide')

