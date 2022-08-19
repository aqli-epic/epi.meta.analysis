
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

# epi studies analysis raw sheet
# epi <- readxl::read_xlsx("./data-raw/pm2.5_distribution/AQLI_Epidemiology Literature Research.xlsx", sheet = "AnalysisDatasetPM2.5MortalityAn")

# AQLI color file
# aqli_color <- read_csv("./data-raw/pm2.5_distribution/color.csv")

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

#> add useful columns and filter out some studies (e.g. pooled studies, meta analysis)

epi <- epi %>%
  mutate(study_duration = (study_end_year - study_start_year) + 1)

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
