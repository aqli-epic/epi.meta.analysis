
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
aqli_color <- read_csv("./data-raw/pm2.5_distribution/color.csv")

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
  filter(cohort_size > min_sample_size, study_duration > min_study_duration_in_years)

# continent list
continent_list <- c("Asia", "Africa", "North America", "South America", "Europe", "Oceania")

# missing continents
missing_continents_logical <- continent_list %notin% unique(epi$continent)
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
