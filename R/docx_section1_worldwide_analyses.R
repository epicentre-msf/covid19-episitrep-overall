
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
my_doc <- add_par_normal(
  sprintf('As of %s, the total number of Covid-19 cases reported worldwide was %s, and the total Covid-19 associated deaths was %s.', 
          format(max(df_ecdc$date), "%d %B %Y"), 
          format(sum(df_ecdc$cases, na.rm = TRUE), big.mark = ","), 
          format(sum(df_ecdc$deaths, na.rm = TRUE), big.mark = ",")))


my_doc <- add_par_normal(
  sprintf('%s countries (%s) reported more than 100 thousand cases.
  The United States reported more than 2 million cases and Brazil has crossed the one million mark this week.
  %s African countries reported less than 50 cases (%s).
  %s reported more than 1,000 cases (%s) (Figure 1 - Cases count).', 
          tbl_cases_count %>% call_countries_with_more(100000, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% call_countries_with_more(100000, "cases") %>% combine_words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_less(50, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_less(50, "cases") %>% combine_words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_more(1000, "cases") %>% length() %>% Words(), 
          tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_more(1000, "cases") %>% combine_words()))


my_doc <- add_par_normal(
  sprintf('Most of countries worldwide have been reporting new cases with an increasing or stable trend. %s countries reported an increasing trend this week (compared to 41 last week).', 
  call_countries_increasing('cases') %>% length() %>% Words()))


my_doc <- add_par_normal(
  sprintf('%s countries had an increasing trend in Africa (%s). Also in Asia several countries reported an increasing trend (%s). %s countries of the Americas (%s) also reported an increasing trend.', 
  call_countries_increasing("cases", "Africa") %>% length() %>% Words(), 
  call_countries_increasing("cases", "Africa") %>% combine_words(), 
  call_countries_increasing("cases", "Asia") %>% combine_words(), 
  call_countries_increasing("cases", "Americas") %>% length() %>% Words(), 
  call_countries_increasing("cases", "Americas") %>% combine_words()))


my_doc <- add_end_section_2columns()


## - Map 1
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_cases_grid_{week_report}.png'),
  figure_title = glue('Mapping of number of Covid-19 cases and cases trends estimated during the period from {format(date_max_report - 11, "%d %B %Y")} to {format(date_max_report, "%d %B %Y")} (12 days)'))


my_doc <- add_end_section_continuous()


my_doc <- add_par_normal(
  sprintf('Since the beginning of the epidemic, countries presenting the highest cumulative incidences remain mostly developed countries (North America, Europe, Middle East, and some countries in central and South America). Most countries in Asia and Africa remain below the threshold of 10 confirmed cases per 100,000 population (Figure 2).'))


my_doc <- add_par_normal(
  sprintf('Since last week, NAME OF THE COUNTRIES reached the threshold of 10 confirmed cases per 100,000 population. Some others, including NAME OF THE COUNTRIES, are now over 100 confirmed cases per 100,000 population.'))


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
  sprintf('Similarly to the number of cases, countries that reported the highest numbers of Covid-19 associated deaths are the USA, Brazil and some countries in Europe. NAME OF THE COUNTRIES also reached the threshold of 10,000 Covid19 associated deaths.'))

# 2
my_doc <- add_par_normal(
  sprintf('An increasing number of African countries are now reporting more than 50 deaths. NAME OF THE COUNTRIES are now reporting over 10,000 deaths.'))

# 3
my_doc <- add_par_normal(
  sprintf('More/Less (?) countries reported an increasing trend in death this week. This includes more countries of .... (Figure 3 – Trends in deaths).'))

# 4
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
  object_name  = glue('map_world_deaths_grid_{week_report}.png'), 
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
  sprintf("A sharp increasing of cases (doubling time of less than %s days) is observed in: Africa : (%s), %s; Asia: (%s), %s; Americas: (%s), %s (Figure 4 and Table 1). Countries with a particularly worrying doubling time (about or less than 4 days) are ....", 
  threshold_doubling_time, 
  length(call_countries_doubling('cases_est', 'Africa')), 
  ifelse(length(combine_words(call_countries_doubling('cases_est', 'Africa')))!=0, combine_words(call_countries_doubling('cases_est', 'Africa')), 'None'), 
  length(call_countries_doubling('cases_est', 'Asia')), 
  ifelse(length(combine_words(call_countries_doubling('cases_est', 'Asia')))!=0, combine_words(call_countries_doubling('cases_est', 'Asia')), 'None'), 
  length(call_countries_doubling('cases_est', 'Americas')), 
  ifelse(length(combine_words(call_countries_doubling('cases_est', 'Americas')))!=0, combine_words(call_countries_doubling('cases_est', 'Americas')), 'None')))

# 2
my_doc <- add_par_normal(
  sprintf("No country this week reported a doubling time in cases of less than 8 days. However Ethiopia reported a worrying sharp increase in deaths and should remain closely monitored."))

# 3
my_doc <- add_par_normal(
  sprintf("Similarly as last week, Guatemala presents a naïve CFR of 3.9%% which appears higher than expected, which may be an indicator that the number of cases are underestimated. Iraq also displays a naïve CFR of 3.0%% (Table 1)."))


my_doc <- add_end_section_2columns()


## - Map
my_doc <- add_figure_map_world_grid(
  object_name  = glue('map_world_doubling_grid_{week_report}.png'),
  figure_title = glue('Doubling time of the number of Covid-19 cases and associated deaths estimated in the last 12 days (only countries with increasing trends are displayed)'))


my_doc <- add_par_normal('')


## - Table
my_doc <- add_table(
  object_name = glue("gtbl_cfr_doubling_rank_{week_report}.png"), 
  table_title = glue('Countries with estimated cases or deaths doubling time of less than {threshold_doubling_time} days'), 
  folder = "worldwide", 
  width = 13.55 * cm_to_in, 
  height = 5.01 * cm_to_in)


my_doc <- add_end_section_continuous()


my_doc <- add_par_normal(
  sprintf("The following graph presents the relationship between current doubling time and time since the first significant increasing trend was reported."))


my_doc <- add_par_normal(
  sprintf("Countries represented in the lower right hand corner are those which are in the most worrying situation, representing a prolonged time with an increasing trend."))

my_doc <- add_par_normal(
  sprintf("Currently there is no country with a doubling time of less than 8 days. However it is of interest that NAME OF THE COUNTRIES are reporting an increasing trend for over 35 days (without discontinuing)."))


my_doc <- add_par_normal(
  sprintf("While Iraq (to check) still displays a increasing over 80 days after reporting the first increasing trend, it appears that this sharp increase in case in the country only started around mid-May, after a first small wave in cases in March/April."))


my_doc <- add_end_section_2columns()


my_doc <- add_figure(
  object_name = glue('dots_hotspot_cases_{week_report}.png'), 
  figure_title = glue('Distribution of doubling time of Covid-19 cases according to length in days since the first significant increasing trend reported (only countries with doubling time of less than {threshold_doubling_time} days as of {format(date_max_report, "%d %B %Y")} are displayed)'), 
  folder = 'worldwide')



# --- --- --- --- --- --- --- 
# TEASTING
# --- --- --- --- --- --- --- 

## - Heading
my_doc <- add_heading2(heading_text = 'Testing') 
my_doc <- add_end_section_continuous()

# 1
my_doc <- add_par_normal(
  sprintf("Having a better insight into countries’ testing capacity would allow for a better interpretation of reported cases relative to actual ones. Figure 6 displays the relationship between the proportion of positive tests, and the cumulative number of cases per 100,000 population in the country (both in the last 12 days). The countries displayed in the right side of the graphic therefore have a higher proportion of positive tests, which may point to a possibly insufficient sensitivity of the testing strategy. Of these, countries with a recent high number of cases are facing an even more complicated situation. Only countries with a current increasing trend are displayed, thus being on a comparable stage of the epidemic."))

# 2
my_doc <- add_par_normal(
  sprintf("It is interesting to note that about half of the countries are reporting a proportion of positive tests higher than 10%%. This could indicate that most countries may not detect all cases due to an insufficiently sensitive testing strategy. However, it is of interest that a non-negligible number of countries did not regularly report the number of tests performed, thus leading to potentially overestimated proportions."))

# 3
my_doc <- add_par_normal(
  sprintf("This week no country displayed a proportion of positive test higher than about 30%%."))

# 4
my_doc <- add_par_normal(
  sprintf("Countries with a high recent cumulative incidence and high proportion of positive (NAME OF THE COUNTRIES) are to be closely monitored as they are facing an important rise in cases while having possible gaps in testing strategy sensitivity. Chile appears to be in this situation for the fourth consecutive week."))


my_doc <- add_end_section_2columns()


# - Graph
# --- --- --- --- --- --- --- 
my_doc <- add_figure(
  object_name = glue('dots_prop_tests_{week_report}.png'), 
  figure_title = glue('Relation between the number of tests carried out in the last 12 days (as ratio of the number of confirmed cases) and the cumulative number of cases reported'), 
  folder = 'worldwide')

