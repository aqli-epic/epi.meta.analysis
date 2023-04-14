
# app.R helper file-------------------------------------------------------------

## libraries
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(readr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapsf)
library(colorspace)
library(shiny)
library(shinydashboard)
library(DT)

# global variables
who_pm2.5_guideline <- 5

# global operations
`%notin%` <- Negate(`%in%`)

# epi studies analysis raw sheet
epi <- readxl::read_xlsx("./data-raw/pm2.5_distribution/AQLI_Epidemiology Literature Research.xlsx", sheet = "AnalysisDatasetPM2.5MortalityAn")


#> change default columns types
epi$cohort_size <- as.numeric(epi$cohort_size)
epi$study_start_year<- as.numeric(epi$study_start_year)
epi$study_end_year <- as.numeric(epi$study_end_year)
epi$pm2.5_exposure_ll <- as.numeric(epi$pm2.5_exposure_ll)
epi$pm2.5_exposure_ul <- as.numeric(epi$pm2.5_exposure_ul)
epi$mean_pm2.5 <- as.numeric(epi$mean_pm2.5)
epi$sd_pm2.5 <- as.numeric(epi$sd_pm2.5)
epi$cohort_age_ll <- as.numeric(epi$cohort_age_ll)
epi$cohort_age_ul <- as.numeric(epi$cohort_age_ul)

# AQLI color file
aqli_color <- read_csv("./data-raw/pm2.5_distribution/gadm2_aqli2021_vit.csv")
aqli_color <- aqli_color %>%
  dplyr::mutate(objectid_color = objectid_gadm2)

# getting a country continent file
country_continent <- read_csv("./data-raw/pm2.5_distribution/country_continent.csv")

# adding a continent column to the color file
aqli_color <- aqli_color %>%
  left_join(country_continent, by = "country") %>%
  select(objectid_color:iso_alpha3, continent, everything())

#> setting report parameters
min_sample_size <- 1000
min_study_duration_in_years <- 1

#> Filtering the epi database (minimum cohort size: >1000, minimum study duration: > 1 year): commented for now.
epi <- epi %>%
  mutate(study_duration = (study_end_year - study_start_year) + 1) %>%
  filter(cohort_size >= min_sample_size, study_duration >= min_study_duration_in_years, !is.na(cohort_uid)) %>%
  distinct(cohort_uid, .keep_all = TRUE)

# creating this temp dataset for the  continent panel dataset
epi_tmp <- epi %>%
  mutate(continent = ifelse(country == "China", "China", continent),
         continent = ifelse(continent == "Asia", "Asia - China", continent))

# continent list (specifucally for the continent panel graph, that contains the Asia and Asia -  China panels, and considers them as continents in a
# separate epi_tmp dataset, that is specially created for this graph)
continent_list <- c("China", "Asia - China", "Africa", "North America", "South America", "Europe", "Oceania")

# missing continents
missing_continents_logical <- continent_list %notin% unique(epi_tmp$continent)
missing_continents <- continent_list[missing_continents_logical]

#> Calculations needed for all sections above the "Results section".

# percent of population not in compliance with the WHO guideline
num_people_above_who <- aqli_color %>%
  filter(pm2020 > who_pm2.5_guideline) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
  unlist()

# world population
world_pop <- aqli_color %>%
  summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
  unlist()

percent_people_above_who <- round((num_people_above_who/world_pop)*100, 1)

# pop pollution graph with 2 buckets: function.

pop_pol_graph_2_buckets <- function(aqli_color, epi, thresh_ll, thresh_ul, pm2.5_col_name){

  if(thresh_ll > thresh_ul){
    stop("Lower Limit should be less than or equal to the Upper limit! Please try again!")
  }

  # aqli color population in ordered pm2.5 buckets
  aqli_color_grp_pm2.5_buckets <- aqli_color %>%
    dplyr::filter(!is.na(!!as.symbol(pm2.5_col_name))) %>%
    dplyr::mutate(region = ifelse((!!as.symbol(pm2.5_col_name) >= thresh_ll) & (!!as.symbol(pm2.5_col_name) <= thresh_ul), stringr::str_c(thresh_ll, "-", thresh_ul), !!as.symbol(pm2.5_col_name)),
           region = ifelse(!!as.symbol(pm2.5_col_name) > thresh_ul, stringr::str_c(">", thresh_ul), region),
           region = ifelse(!!as.symbol(pm2.5_col_name) < thresh_ll, stringr::str_c("<", thresh_ll), region)) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(order_pollution_group = ifelse(region == stringr::str_c("<", thresh_ll), 1, 0),
           order_pollution_group = ifelse(region == stringr::str_c(thresh_ll, "-", thresh_ul), 2, order_pollution_group),
           order_pollution_group = ifelse(region == stringr::str_c(">", thresh_ul), 3, order_pollution_group)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tot_pop_prop = (tot_pop/sum(tot_pop))*100)


  # epi total number of studies in ordered pollution buckets (initially filtering out pooled studies, will add back in using a for loop)
  epi_num_studies_grp_pm2.5_buckets <- epi %>%
    dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0) %>%
    dplyr::group_by(paper_uid) %>%
    dplyr::summarise(mean_pm2.5 = mean(mean_pm2.5, na.rm = TRUE)) %>%
    dplyr::mutate(region = ifelse(mean_pm2.5 >= thresh_ll & mean_pm2.5 <= thresh_ul, stringr::str_c(thresh_ll, "-", thresh_ul), mean_pm2.5),
           region = ifelse(mean_pm2.5 > thresh_ul, stringr::str_c(">", thresh_ul), region),
           region = ifelse(mean_pm2.5 < thresh_ll, stringr::str_c("<", thresh_ll), region)) %>%
    dplyr::mutate(order_pollution_group = ifelse(region == stringr::str_c("<", thresh_ll), 1, 0),
           order_pollution_group = ifelse(region == stringr::str_c(thresh_ll, "-", thresh_ul), 2, order_pollution_group),
           order_pollution_group = ifelse(region == stringr::str_c(">", thresh_ul), 3, order_pollution_group)) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(tot_studies = dplyr::n(), order_pollution_group = order_pollution_group[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tot_studies_prop = (tot_studies/sum(tot_studies))*100)

  #> bar chart version

  # creating a master dataset for both population and studies data
  pop_epi_studies_data <- aqli_color_grp_pm2.5_buckets %>%
    dplyr::left_join(epi_num_studies_grp_pm2.5_buckets, by = c("region", "order_pollution_group")) %>%
    dplyr::select(region,  order_pollution_group, tot_pop_prop, tot_studies_prop) %>%
    tidyr::pivot_longer(cols = tot_pop_prop:tot_studies_prop, names_to = "type_of_prop", values_to = "val") %>%
    dplyr::mutate(type_of_prop = ifelse(type_of_prop == "tot_pop_prop", "Percent Population in PM₂.₅ bucket", "Percent Studies in PM₂.₅ bucket"))

pop_num_studies_in_pollution_buckets_graph <- pop_epi_studies_data %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(region, order_pollution_group), y = val, fill = type_of_prop), position = position_dodge(), width = 0.4) +
  ggplot2::scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  ggplot2::scale_fill_manual(values = c("tot_pop_prop" = "grey", "tot_studies_prop" = "cornflowerblue"), labels = c("Proportion of World Population in Bucket", "Proportion of Studies Completed in Bucket")) +
  ggplot2::labs(x = expression("Mean" ~ PM[2.5] ~ "bucket (in µg/m³)"),  y = "Percentage", fill = "",
       caption = stringr::str_wrap("*This graph 'only' takes into account the PM₂.₅ specific studies. For multi-country (pooled) studies, it averages the mean PM2.5 values, across all countries."), width = 10) +
 # ggplot2::geom_text(mapping = aes(x = forcats::fct_reorder(region, order_pollution_group), y = val, label = paste0(round(val, 1), "%")), position = ggplot2::position_dodge2(width = 1, preserve = "single"), vjust = -0.5, hjust = -0.2, size = 3) +
ggthemes::theme_hc() +
  ggplot2::theme(axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot") +
  viridis::scale_fill_viridis(discrete = TRUE)

return(pop_num_studies_in_pollution_buckets_graph)



}
