

txt_world_cases <- list(
  
  par1 = glue('At the date of {format(max(df_ecdc$date), "%d %B %Y")} the total number of Covid-19 cases reported worldwide is {format(sum(df_ecdc$cases, na.rm = TRUE), big.mark = ",")}, and the total Covid-19 associated deaths is {format(sum(df_ecdc$deaths, na.rm = TRUE), big.mark = ",")}.'), 
  
  par2 = glue('{Words(length(pull(tbl_countries_with_more_100k_cases, country)))} countries ({combine_words(pull(tbl_countries_with_more_100k_cases, country))}) reported more than 100 thousand cases. The United States reported more than 1.5 million cases. Few African countries reported less than 50 cases (Figure 1 - Cases count) with the majority of them reporting between 100 and 1,000 cases.'), 
  
  par3 = glue('Most of countries worldwide have been reporting new cases with an increasing or stable trend.'), 
              
  par4 = glue('Compare to last week, less African countries reported an increasing trend of cases ({combine_words(select_increasing("cases", "Africa") %>% pull(country))}), but more Asian and Middle-East countries did so ({combine_words(select_increasing("cases", "Asia") %>% pull(country))}). while Australia is back to a stable trend.'), 
  
  
  par5 = glue('Several countries in the Americas ({combine_words(select_increasing("cases", "Americas") %>% pull(country))}) reported also an increasing trend (Figure 1 - Trends in cases). Though Brazil appears with a stable trend, it may be due to outlier numbers in daily counts.'), 
  
  par6 = glue('Since the beginning of the epidemic, countries presenting the highest cumulative incidences mostly remain developed countries (North America, Europe, Middle East, and some countries in central and South America). Most countries in Asia and Africa remain under the threshold of 10 confirmed cases per 100,000 population (Figure 2).'), 
  
  par7 = glue('Since last week, several countries, including Philippines, Cameroon, Bangladesh, Paraguay, Tajikistan and Guatemala, reached the threshold of 10 confirmed cases per 100,000 population. Some others, including Brazil, Haiti, Oman, are now over 100 confirmed cases per 100,000 population.')
  
)



txt_world_deaths <- list(
  
  par1 = glue('Similarly to the number of cases, countries that reported the highest numbers of Covid-19 associated deaths are the USA, Brazil and some countries in Europe.'), 
  
  par2 = glue('The great majority of African countries are still reporting less than 50 deaths so far.'), 
  
  par3 = glue('Countries with an increasing trend in deaths are {select_increasing("deaths") %>% pull(country) %>% combine_words()} (Figure 3 – Trends in deaths).')
  
)



txt_world_doubling <- list(
  
  par1 = glue('A sharp increasing of cases (doubling time of less than {threshold_doubling_time} days) is observed in {words(length(african_countries_short_doubling_time))} African countries ({combine_words(african_countries_short_doubling_time)}), {words(length(asian_countries_short_doubling_time))} Asian countries ({combine_words(asian_countries_short_doubling_time)}) and Haiti (Figure 4 and Table 1). Yemen and Zambia appear with a particularly worrying doubling time in cases of about 4 days.'), 
  
  par2 = glue('A high number of cases and sharp increasing of deaths was also observed in Kuwait, where situation should remain monitored. (Table 1 and Figures 3 and 4).'), 
  
  par3 = glue('Yemen, Tchad and Haiti reported specifically high naïve CFR, which may be an indicator that number of cases are particularly underestimated (Table 1).'), 
  
  
  par4 = glue('The following graph presents the relation between current doubling time and time since the first significant increasing trend was reported. Countries represented in the lower right corner are those to stand in the most worrying situation, representing a prolonged time in increasing trend. '), 
  
  par5 = glue('Currently no country with a doubling time of less than 6 days was over 15 days of reporting increasing trend; however it is of interest that Gabon (over 30 days) and Sudan (over 20 days) should be closely monitored as both appeared to increase case without discontinuing since first increasing trend was reported. Zambia is also approaching 10 days with an increasing trend with a current worrying doubling time of about 4 days.'), 
  
  par6 = glue('More text here regarding the graphic below. We need first to explain what the graphic is about and how to interpret. 
To Anais: I cannot think about it now. I will think tomorrow and let you think.')
  
)


txt_msf_general <- list(
  
  par1 = glue('As of {format(msf_date_max , "%d %B %Y")}, we received data from {words(nb_msf_sites)} MSF project sites representing all 5 OCs, for a total of {nb_msf_obs} patients consulted and/or admitted, including {nb_msf_confirmed}. While implementation of the linelist is still ongoing, number of patients captured seem to remain low (Table 2).'), 
  
  par2 = glue('Confirmed cases consulted/admitted to MSF facilities remain relatively young, with a median age of 37 years. Males are over-represented among those. 115 (43%) reported at least one comorbidity (Table 3).'), 
  
  par3 = glue('Seventeen (3.1%) confirmed, probable, or suspected cases with known outcome died, while 193 (46%) were cured (Table 4). Median age among deceased patients is 62 years, with the youngest being 25.'), 
  
  par4 = glue('Two confirmed cases presented no symptoms. Symptoms of the remaining patients are presented in Table 5.')
  
)


