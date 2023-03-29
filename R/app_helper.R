
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
  filter(cohort_size >= min_sample_size, study_duration >= min_study_duration_in_years)

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
    ggplot2::geom_col(mapping = aes(x = fct_reorder(region, order_pollution_group), y = val, fill = type_of_prop), position = position_dodge(), width = 0.4) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
    ggplot2::scale_fill_manual(values = c("tot_pop_prop" = "grey", "tot_studies_prop" = "cornflowerblue"), labels = c("Proportion of World Population in Bucket", "Proportion of Studies Completed in Bucket")) +
    ggplot2::labs(x = expression("Mean" ~ PM[2.5] ~ "bucket (in µg/m³)"),  y = "Percentage", fill = "",
         caption = stringr::str_wrap("*This graph 'only' takes into account the PM₂.₅ specific studies. For multi-country (pooled) studies, it averages the mean PM2.5 values, across all countries."), width = 10) +
  ggthemes::theme_hc() +
    ggplot2::theme(axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"),
          plot.caption = element_text(size = 8, hjust = 0),
          plot.caption.position = "plot") +
    viridis::scale_fill_viridis(discrete = TRUE)

  return(pop_num_studies_in_pollution_buckets_graph)

}

# # # test chunk
#
#  library(ggplot2)
#  library(grid)
#
#  # create a sample dataset
#  df <- data.frame(x = 1:10, y = rnorm(10), group = rep(letters[1:5], each = 2))
#
#  # create a facetted plot with 5 panels in a row
#  p <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~group, ncol = 5) + theme_hc()
#
#  # convert the plot object to a grob object
#  g <- ggplotGrob(p)
#
#  # get the panel grobs
#  panels <- getGrob(g, "panel", grep = TRUE)
#
#  # add a horizontal line at y = 0 on top of each panel
#  for (i in 1:5) {
#    vp <- viewport(layout.pos.row = i, layout.pos.col = 1)
#    pushViewport(vp)
#    grid.lines(x = c(unit(0.1, "npc"), unit(1, "npc")),
#               y = unit(0.8, "npc"),
#               gp = gpar(col = "darkred", lwd = 1),
#             arrow = arrow(length = unit(0.15, "in"), type = "closed", ends = "last",
#                           angle = 30))
#    popViewport()
#  }
#
# # display the plot
#
#
#  grid.draw(g)
#
#
# # ##################
#
#  library(ggplot2)
#
#  # create a sample dataset with groups
#  df <- data.frame(x = rnorm(1000), group = sample(c("A", "B", "C"), 1000, replace = TRUE))
#
#  # define the label data for each facet with different coordinates
#  label_data <- data.frame(
#    group = unique(df$group),
#    label = c("Group A", "Group B", "Group C"),
#    x = c(0.5, 1.5, -0.5),
#    y = c(20, 15, 10)
#  )
#
#  # create the histogram with labels
#  ggplot(df, aes(x)) +
#    geom_histogram(aes(y = ..density..), binwidth = 0.5, color = "black", fill = "white") +
#    facet_wrap(~ group, ncol = 2, scales = "free") +
#    geom_text(data = label_data, aes(x = x, y = y, label = label), size = 5, color = "red") +
#    labs(y = "Density")
#
#
#
#
#
#
#
#  ##########################
#
#
#
#
# plt_summarise <- aqli_color %>%
#   filter(continent == "Asia") %>%
#   filter(country != "China") %>%
#   mutate(pop_weights = population/sum(population, na.rm = TRUE),
#          pm2021_pop_weighted = pm2021*pop_weights) %>%
#   summarise(avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
#             tot_pop = sum(population, na.rm = TRUE))
#
#
#
# epi_tmp <- epi %>%
# mutate(continent = ifelse(country == "China", "China", continent),
#        continent = ifelse(continent == "Asia", "Asia - China", continent))
#
# continent_wise_study_duration_graph_summary_table <-
#   epi_tmp %>%
#   dplyr::filter(continent != "NA", !is.na(continent), study_duration != "NA", !is.na(study_duration)) %>%
#   dplyr::group_by(paper_uid, continent) %>%
#   dplyr::summarise(average_study_duration = mean(study_duration, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(order_continent = ifelse(continent == "China", 1, 0),
#                 order_continent = ifelse(continent == "Asia - China", 2, order_continent),
#                 order_continent = ifelse(continent == "Europe", 3, order_continent),
#                 order_continent = ifelse(continent == "North America", 4, order_continent),
#                 order_continent = ifelse(continent == "South America", 5, order_continent),
#                 order_continent = ifelse(continent == "Africa", 6, order_continent),
#                 order_continent = ifelse(continent == "Oceania", 7, order_continent)) %>%
#   dplyr::mutate(panel_annotation = ifelse(continent == "Asia - China (3161.5 million people)", "High Pollution", ""),
#                 panel_annotation = ifelse(continent == "North America (589.2 million people)", "Low Pollution", panel_annotation)) %>%
#   dplyr::mutate(annotation_x = ifelse(panel_annotation == "Asia - China (3161.5 million people)", 20, NA),
#                 annotation_x = ifelse(panel_annotation == "North America (589.2 million people)", 2, annotation_x),
#                 annotation_y = ifelse(panel_annotation %in% c("Asia - China (3161.5 million people)", "North America (589.2 million people)"), 7.5, NA))
#
#
# annotate_df <- tibble(x = c(20, 2, 5, 5, 5, 5), y = c(7, 7, 5, 5, 5, 5),
#                       continent = c("Asia - China (3161.5 million people)", "North America (589.2 million people)",
#                                     "China (1402.5 million people)",
#                                     "Africa (1373.8 million people)",
#                                     "South America (429.2 million people)",
#                                     "Europe (740.8 million people)"),
#                       label_annotate = c("High Pollution", "Low Pollution", "", "", "", ""))
#
# plt_cont_2 <- continent_wise_study_duration_graph_summary_table %>%
#   filter(continent != "Oceania") %>%
# #  mutate(continent = as.factor(continent)) %>%
#   add_row(paper_uid = -1, continent = "Africa", average_study_duration = NA, order_continent = 6) %>%
#   add_row(paper_uid = -2, continent = "South America", average_study_duration = NA, order_continent = 5) %>%
#   mutate(order_continent = ifelse(continent == "China", 2, order_continent),
#       order_continent = ifelse(continent == "Asia - China", 1, order_continent),
#          order_continent = ifelse(continent == "Europe", 5, order_continent),
#          order_continent = ifelse(continent == "North America", 6, order_continent),
#          order_continent = ifelse(continent == "South America", 4, order_continent),
#       order_continent = ifelse(continent == "Africa", 3, order_continent)) %>%
#   mutate(continent = ifelse(continent == "China", "China (1402.5 million people)", continent),
#          continent = ifelse(continent == "Africa", "Africa (1373.8 million people)", continent),
#          continent = ifelse(continent == "North America", "North America (589.2 million people)", continent),
#          continent = ifelse(continent == "South America", "South America (429.2 million people)", continent),
#          continent = ifelse(continent == "Europe", "Europe (740.8 million people)", continent),
#          continent = ifelse(continent == "Asia - China", "Asia - China (3161.5 million people)", continent)) %>%
#   dplyr::mutate(panel_annotation = ifelse(continent == "Asia - China (3161.5 million people)", "High Pollution", ""),
#                 panel_annotation = ifelse(continent == "North America (589.2 million people)", "Low Pollution", panel_annotation)) %>%
#   dplyr::mutate(annotation_x = ifelse(panel_annotation == "High Pollution", 20, NA),
#                 annotation_x = ifelse(panel_annotation == "Low Pollution", 2, annotation_x),
#                 annotation_y = ifelse(panel_annotation %in% c("High Pollution", "Low Pollution"), 7.5, NA)) %>%
#   ggplot2::ggplot(mapping = aes(x = average_study_duration)) +
#   ggplot2::geom_histogram(mapping = ggplot2::aes(y = ..count.., fill = continent, group = continent), color = "white", position = "identity", binwidth = 2) +
#   ggplot2::labs(x = "Study Duration", y = "Count",
#                 caption = stringr::str_wrap("*This graph displays distribution by continent (and for China, Asia - China) for all studies (not PM2.5 specific)."), width = 10,
#                 title = expression("Continent (and China) wise Study Duration Distribution and 2021 Annual Average" ~ PM[2.5] ~ "(in µg/m³)")) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
#  ggthemes::theme_hc() +
#   ggplot2::theme(legend.title = element_blank()) +
#   ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
#   geom_text(data = annotate_df, mapping = aes(x = x, y = y, label = as.factor(label_annotate), group = as.factor(continent))) +
#   ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
#                  axis.line.x = ggplot2::element_line(color = "black"),
#                  plot.caption = ggplot2::element_text(hjust = 0, size = 7),
#                  plot.title = element_text(size = 14, hjust = 0.5),
#                  plot.subtitle = element_text(size = 8, hjust = 0.5),
#                  strip.background = element_rect(fill = "floralwhite"),
#                  legend.background = element_rect(color = "black")) +
#   scale_fill_manual(values = c("North America (589.2 million people)" = "#FFBA00", "Europe (740.8 million people)" = "#FF9600",
#                                "South America (429.2 million people)" = "#FF6908", "Africa (1373.8 million people)" = "#E63D23", "China (1402.5 million people)" = "#BD251C",
#                                "Asia - China (3161.5 million people)" = "#8C130E")) +
#   scale_y_continuous(breaks = seq(0, 10, 2))
#   viridis::scale_fill_viridis(discrete = TRUE)
#
#
#
#
# plt_cont_grob <- ggplotGrob(plt_cont)
#
# # get the panel grobs
# panels <- getGrob(plt_cont_grob, "panel", grep = TRUE)
#
# # add a horizontal line at y = 0 on top of each panel
# for (i in 1:6) {
#   vp <- viewport(layout.pos.row = i, layout.pos.col = 1)
#   pushViewport(vp)
#   grid.lines(x = c(unit(0.1, "npc"), unit(1, "npc")),
#              y = unit(0.82, "npc"),
#              gp = gpar(col = "darkred", lwd = 1),
#              arrow = arrow(length = unit(0.15, "in"), type = "closed", ends = "last",
#                            angle = 30))
#   popViewport()
# }
#
# # display the plot
# grid.draw(g)
#
#
# ggsave("./continent_wise_study_duration_and_pol_v2.pdf", plt, height = 10, width = 16)
#
# #
#
# #################
#
#
# plot_data <- epi %>%
#   group_by(continent, country) %>%
#   summarise(count_studies = n(), .groups = "keep")
#
#
#
# plot_data %>%
#   ggplot() +
#   geom_col(mapping = aes(x = continent, y = count_studies, fill = continent), position = position_dodge2(preserve = "single"))
#
# plot_data %>%
#   filter(continent == "Asia") %>%
#   ggplot() +
#   geom_bar(mapping = aes(x = forcats::fct_reorder(country, count_studies, .desc = TRUE), y = count_studies), stat = "identity")
#
# +
#   facet_grid(continent~., scales = "free_y") +
#   scale_x_discrete(
#     "country",
#     limits = c(africa_countries, europe_countries, north_america_countries, south_america_countries, asia_countries, oceania_countires),
#     breaks = c(africa_countries, europe_countries, north_america_countries, south_america_countries, asia_countries, oceania_countires)
#   ) +
#   theme(axis.text.x = element_text(size = 7, angle = 45))
#
#
#
#
#
#
#
#
# plt1 <- ggplot(plot_data %>% filter(continent == "Asia"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "Asia (24 times was part of a epi study)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
# plt2 <- ggplot(plot_data %>% filter(continent == "Africa"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "Africa (0)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
# plt3 <- ggplot(plot_data %>% filter(continent == "South America"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "South America (0)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
# plt4 <- ggplot(plot_data %>% filter(continent == "Europe"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "Europe (15)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
# plt5 <- ggplot(plot_data %>% filter(continent == "North America"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "North America (49)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
#
# plt6 <- ggplot(plot_data %>% filter(continent == "Oceania"), aes(area = count_studies, fill = country, label = count_studies)) +
#   geom_treemap() +
#   viridis::scale_fill_viridis(discrete = TRUE) +
#   geom_treemap_text(color = "white") +
#   labs(title = "Oceania (0)", fill = "Country") +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(color = "black"),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "white"))
#
# plt_final <- gridExtra::grid.arrange(plt1, plt2, plt3, plt4, plt5, plt6, nrow = 1, top = "Number of times a country has been included in an epi study (Continent Wise)")
#
# plt_final + ggtitle("foo")
#
#
# ggsave("./treemap_continent_wise_num_times_country_incl_in_epi_study.pdf", plt_final, height = 10, width = 40)
#
#
#
# ###########
#
#
#
#
#
# epi %>%
#   ggplot() +
#   geom_hex(mapping = aes(x = continent, y = country))
#
# africa_countries <- epi %>%
#   filter(continent == "Africa") %>%
#   pull(country) %>%
#   unique()
#
# north_america_countries <- epi %>%
#   filter(continent == "North America") %>%
#   pull(country) %>%
#   unique()
#
# south_america_countries <- epi %>%
#   filter(continent == "South America") %>%
#   pull(country) %>%
#   unique()
#
# europe_countries <- epi %>%
#   filter(continent == "Europe") %>%
#   pull(country) %>%
#   unique()
#
# oceania_countires <- epi %>%
#   filter(continent == "Oceania") %>%
#   pull(country) %>%
#   unique()
#
# asia_countries <- epi %>%
#   filter(continent == "Asia") %>%
#   pull(country) %>%
#   unique()
#
# plot_data <- epi %>%
#   group_by(continent, country) %>%
#   summarise(count_studies = n(), .groups = "keep")
#
#
#
# plot_data %>%
#   ggplot() +
#   geom_col(mapping = aes(x = continent, y = count_studies, fill = continent), position = position_dodge2(preserve = "single"))
#
# plot_data %>%
#   ggplot() +
#   geom_bar(mapping = aes(x = country, y = count_studies, fill = country), stat = "identity") +
#   facet_grid(continent~., scales = "free_y") +
#   scale_x_discrete(
#     "country",
#     limits = c(africa_countries, europe_countries, north_america_countries, south_america_countries, asia_countries, oceania_countires),
#     breaks = c(africa_countries, europe_countries, north_america_countries, south_america_countries, asia_countries, oceania_countires)
#   ) +
#   theme(axis.text.x = element_text(size = 7, angle = 45))
#
#
# plot_data %>%
#   ggplot() +
#   geom_col(mapping = aes(x = country, y = count_studies, group = continent, fill = continent), position = position_dodge2(preserve = "single"))
