
source(file.path(path.R, "utils_vis.R")       , encoding = "UTF-8")


txt_world_cases <- list(
  
  par1 = glue('At the date of {format(max(df_ecdc$date), "%d %B %Y")} the total number of Covid-19 cases reported worldwide is {format(sum(df_ecdc$cases, na.rm = TRUE), big.mark = ",")}, and the total Covid-19 associated deaths is {format(sum(df_ecdc$deaths, na.rm = TRUE), big.mark = ",")}.'), 
  
  par2 = glue('{tbl_cases_count %>% call_countries_with_more(100000, "cases") %>% length() %>% Words()} countries ({tbl_cases_count %>% call_countries_with_more(100000, "cases") %>% combine_words()}) reported more than 100 thousand cases.
  The United States reported more than 1.9 million cases.
  {tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_less(50, "cases") %>% length() %>% Words()} African countries reported less than 50 cases ({tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_less(50, "cases") %>% combine_words()}).
  {tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_more(1000, "cases") %>% length() %>% Words()} reported more than 1,000 cases ({tbl_cases_count %>% filter(continent == "Africa") %>% call_countries_with_more(1000, "cases") %>% combine_words()}) (Figure 1 - Cases count).'), 
  
  par3 = glue('Most of countries worldwide have been reporting new cases with an increasing or stable trend.'), 
              
  par4 = glue('countries that reported an increasing trend:
  Africa, {length(call_countries_increasing("cases", "Africa"))}, {combine_words(call_countries_increasing("cases", "Africa"))};
  Asia, {length(call_countries_increasing("cases", "Asia"))}, {combine_words(call_countries_increasing("cases", "Asia"))};
  Americas, {length(call_countries_increasing("cases", "Americas"))}, {combine_words(call_countries_increasing("cases", "Americas"))}.'), 
  
  par5 = glue('Since the beginning of the epidemic, countries presenting the highest cumulative incidences mostly remain developed countries (North America, Europe, Middle East, and some countries in central and South America). Most countries in Asia and Africa remain under the threshold of 10 confirmed cases per 100,000 population (Figure 2).'), 
  
  par6 = glue('Since last week, several countries, including ..., reached the threshold of 10 confirmed cases per 100,000 population. Some others, including ...., are now over 100 confirmed cases per 100,000 population.'))



txt_world_deaths <- list(
  
  par1 = glue('Similarly to the number of cases, countries that reported the highest numbers of Covid-19 associated deaths are the USA, Brazil and some countries in Europe.'), 
  
  par2 = glue('The great majority of African countries are still reporting less than 50 deaths so far.'), 
  
  par3 = glue('Countries with an increasing trend in deaths are {combine_words(call_countries_increasing("deaths"))} (Figure 3 – Trends in deaths).'))



txt_world_doubling <- list(
  
  par1 = glue("A sharp increasing of cases (doubling time of less than {threshold_doubling_time} days) is observed in; 
              Africa : ({length(call_countries_doubling('cases_est', 'Africa'))}), {combine_words(call_countries_doubling('cases_est', 'Africa'))};
              Asia: ({length(call_countries_doubling('cases_est', 'Asia'))}), {combine_words(call_countries_doubling('cases_est', 'Asia'))};
              Americas: ({length(call_countries_doubling('cases_est', 'Americas'))}), {combine_words(call_countries_doubling('cases_est', 'Americas'))};
              (Figure 4 and Table 1). Counries with a particularly worrying doubling time (about or less than 4 days) are ...."), 
  
  par2 = glue('Countries that reported specifically high naïve CFR, which may be an indicator that number of cases are particularly underestimated are ... (Table 1).'), 
  
  
  par3 = glue('The following graph presents the relation between current doubling time and time since the first significant increasing trend was reported. Countries represented in the lower right corner are those to stand in the most worrying situation, representing a prolonged time in increasing trend.'), 
  
  par4 = glue('Currently no country with a doubling time of less than 6 days was over 15 days of reporting increasing trend; however it is of interest that ... should be closely monitored as both appeared to increase case without discontinuing since first increasing trend was reported.'))


txt_msf_general <- list(
  
  par1 = glue('As of {format(date_max_report , "%d %B %Y")}, we received data from {words(nb_msf_sites)} MSF project sites representing all 5 OCs, for a total of {nb_msf_obs} patients consulted and/or admitted, including {nb_msf_confirmed}. While implementation of the linelist is still ongoing, number of patients captured seem to remain low (Table 2).'), 
  
  par2 = glue('Confirmed cases consulted/admitted to MSF facilities remain relatively young, with a median age of XX years. Males are over-represented among those. XX (YY%) reported at least one comorbidity (Table 3).'), 
  
  par3 = glue('Seventeen (3.1%) confirmed, probable, or suspected cases with known outcome died, while 193 (46%) were cured (Table 4). Median age among deceased patients is 62 years, with the youngest being 25.'), 
  
  par4 = glue('Two confirmed cases presented no symptoms. Symptoms of the remaining patients are presented in Table 5.'))


