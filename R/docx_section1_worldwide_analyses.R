
# --- --- --- --- --- --- --- 
# I. Worldwide analysis
# --- --- --- --- --- --- --- 

# Heading 1
# --- --- --- --- --- --- --- 
my_doc <- add_heading1(heading_text = 'Worldwide analysis') 



# --- --- --- --- --- --- --- 
# COUNT CASES
# --- --- --- --- --- --- --- 

## - Heading 2
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 cases') 


my_doc <- add_end_section_continuous()


## - Text 
# --- --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf('As of %s, the total number of Covid-19 cases reported worldwide was %s, and the total Covid-19 associated deaths was %s.', 
          format(max(dta_ecdc$date), "%d %B %Y"), 
          format(sum(dta_ecdc$cases, na.rm = TRUE), big.mark = ","), 
          format(sum(dta_ecdc$deaths, na.rm = TRUE), big.mark = ",")))

# 2
my_doc <- add_par_normal(
  sprintf('%s countries (%s) reported more than 100 thousand cases. The United States rached the 3 million cases threshold, and Brazil has crossed the 1.5 million mark. India is now third in number of cases reported with more than 700 thousand cases. %s African countries reported less than 50 cases (%s). %s reported more than 1,000 cases (%s) (Figure 1). The pandemic continues to accelerate with X.X million cases (XX%) and XXK deaths (XX%) of the total cases reportend in the last 12 days.', 
          tbl_cases_count %>% call_countries_with(100000, Inf, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% call_countries_with(100000, Inf, "cases") %>% combine_words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(0, 50, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(0, 50, "cases") %>% combine_words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(1000, Inf, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with(1000, Inf, "cases") %>% combine_words()))

# 3
my_doc <- add_par_normal(
  sprintf('Most of countries worldwide have been reporting new cases with an increasing or stable trend. %s countries reported an increasing trend this week (compared to XX last week) (Figure 1). Trends calculated on the last 30 days are also available in the full worldwide analysis report.', 
  call_countries_increasing('cases') %>% length() %>% Words()))

# 4
my_doc <- add_par_normal(
  sprintf('%s countries had an increasing trend in Africa (%s). Also in Asia several countries reported an increasing trend (%s). %s countries of the Americas (%s) also reported an increasing trend.', 
  call_countries_increasing("cases", "Africa") %>% length() %>% Words(), 
  call_countries_increasing("cases", "Africa") %>% combine_words(), 
  call_countries_increasing("cases", "Asia") %>% combine_words(), 
  call_countries_increasing("cases", "Americas") %>% length() %>% Words(), 
  call_countries_increasing("cases", "Americas") %>% combine_words()))

my_doc <- add_end_section_2columns()

## - Map Cases count and trend
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_cases_count_trend_grid_{week_report}.png'),
  figure_title = glue('Mapping of number of Covid-19 cases and cases trends estimated during the period from {format(date_max_report - 11, "%d %B %Y")} to {format(date_max_report, "%d %B %Y")} (12 days)'))

my_doc <- add_end_section_continuous()

# 5 - Incidence
my_doc <- add_par_normal(
  sprintf('Since the beginning of the epidemic, countries presenting the highest cumulative incidences are in North and South America, Europe, and Middle East. In Africa roughly half of the countries remain below the threshold of 10 reported cases per 100,000 population, while the other half is below the threshold of 100 reported cases per 100,000 population, with the exception of Mauritania, Gabon and South Africa (Figure 2).'))

# 6
my_doc <- add_par_normal(
  sprintf('Since last week, NAME OF THE COUNTRIES reached the threshold of 10 confirmed cases per 100,000 population. Some others, including NAME OF THE COUNTRIES, are now over 100 confirmed cases per 100,000 population.'))

## - Map Case Incidence
my_doc <- add_figure_map_world(
  object_name  = glue("map_world_cases_attack_rates_{week_report}.png"), 
  figure_title = glue('Cumulative incidence of Covid-19 reported cases since beginning of epidemic, per 100,000 population'))


my_doc <- add_end_section_2columns(widths = c(7 * cm_to_in, 10 * cm_to_in))



# --- --- --- --- --- --- --- 
# COUNT DEATHS
# --- --- --- --- --- --- --- 

## - Heading 
# --- --- --- --- --- --- --- 
my_doc <- add_heading2(heading_text = 'Number and trends of Covid-19 associated deaths') 

my_doc <- add_end_section_continuous()

## - Text
# --- --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf('Countries that reported the highest numbers of Covid-19 associated deaths are the USA, Brazil and some countries in Europe. NAME OF THE COUNTRIES also reached the threshold of 10,000 Covid19 associated deaths in the last two weeks. About half of African countries are now reporting more than 100 deaths (Figure 3 - Deaths count).'))

# 2
my_doc <- add_par_normal(
  sprintf('More countries reported an increasing trend in death this week. This includes more countries of .... (Figure 3 – Trends in deaths).'))

# 3
my_doc <- my_doc %<>% 
  body_add_par(style = 'Normal', 
               value = 'The full table of cases, deaths and trends can be found ') %>% 
  slip_in_text(style = 'Hyperlink', 
               str = "here", 
               hyperlink = "https://msfintl-my.sharepoint.com/:f:/g/personal/francesco_grandesso_epicentre_msf_org/Ek1jIZ1ghe5GqibjBM3GnpsBJyTF7QBYmFY6bEhgSGVHAA?e=bewh3q/") %>% 
  slip_in_text(style = 'Normal char', 
               str = ".") 

my_doc <- add_end_section_2columns()

## - Map
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_deaths_count_trend_grid_{week_report}.png'), 
  figure_title = glue('Mapping of number of Covid-19 associated deaths and deaths trends estimated during the period from {format(date_max_report - 11, "%d %B %Y")} to {format(date_max_report, "%d %B %Y")} (12 days)'))



# --- --- --- --- --- --- --- 
# DOUBLING TIME 
# --- --- --- --- --- --- --- 

## - Heading
my_doc <- add_heading2(heading_text = 'Doubling time in cases and deaths') 
my_doc <- add_end_section_continuous()

## - Text
# --- --- --- --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("A sharp increasing of cases (doubling time of less than 12 days) is observed in %s countries this week, compared to XX last week (Figure 4 and Table 1).", 
          length(call_countries_doubling('cases_est'))))

# 2
my_doc <- add_par_normal(
  sprintf("This week %s reported a doubling time in cases of less than 8 days. The other countries with a doubling time in cases of less than 12 days are %s.",
          combine_words(call_countries_doubling('cases_est', threshold = 8)), 
          combine_words(setdiff(call_countries_doubling('cases_est', threshold = 12), call_countries_doubling('cases_est', threshold = 8)))))

# 3
my_doc <- add_par_normal(
  sprintf("Also %s reported a worrying increase in deaths.", 
          combine_words(call_countries_doubling('deaths_est', threshold = 12))))

# 4
my_doc <- add_par_normal(
  sprintf("MORE TEXT HERE REGARDING THE DOUBLING TIME AND THE NAIVE CFR (Table 1)."))

my_doc <- add_end_section_2columns()

## - Map
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_doubling_grid_{week_report}.png'),
  figure_title = glue('Doubling time of the number of Covid-19 cases and associated deaths estimated in the last 12 days (only countries with increasing trends are displayed)'),
  width = 18.11, 
  height = 7.77)

my_doc <- add_par_normal('')

## - Table
my_doc <- add_table(
  object_name = glue("gtbl_cfr_doubling_rank_{week_report}.png"), 
  table_title = glue('Countries with estimated cases or deaths doubling time of less than {threshold_doubling_time} days'), 
  folder = "worldwide", 
  width = 13.55 * cm_to_in, 
  height = 5.01 * cm_to_in)

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

## - Heading
my_doc <- add_heading2(heading_text = 'Testing') 
my_doc <- add_end_section_continuous()

# Text 
# --- --- --- --- 
# 1
my_doc <- add_par_normal(
  sprintf("A better insight into countries’ testing capacity would allow for a better interpretation of reported cases relative to actual ones. Figure 5 displays the relationship between the proportion of positive tests, and the cumulative number of cases per 100,000 population in the country (both in the last 12 days). The countries displayed in the right side of the graphic therefore have a higher proportion of positive tests, which may point to a possibly insufficient sensitivity of the testing strategy. Of these, countries with a recent high number of cases are facing an even more complicated situation. Only countries with a current increasing trend are displayed, being on a comparable stage of the epidemic."))

# 2
my_doc <- add_par_normal(
  sprintf("WHO has set the recommended goal of 5%% of positive samples to ensure acceptable sensitivity. This week ....TO BE COMPLETED. This could indicate that these countries may not detect all cases due to an insufficiently sensitive testing strategy. However, it is of interest that a non-negligible number of countries did not regularly report the number of tests performed, thus leading to potentially overestimated proportions."))

# 3
my_doc <- add_par_normal(
  sprintf("To note that XXXXX displayed a very high proportion of positive tests. These countries, as well as some others which reported high cumulative number of cases in the last 12 days (such as LIST OF COUNTRIES) are to be closely monitored as they are facing an important rise in cases while having possible gaps in testing strategy sensitivity."))

my_doc <- add_end_section_2columns()

# - Graph
my_doc <- add_figure(
  object_name = glue('dots_prop_tests_{week_report}.png'), 
  figure_title = glue('Relation between the number of tests carried out in the last 12 days (as proportion of positive tests) and the cumulative number of cases reported'), 
  folder = 'worldwide')

