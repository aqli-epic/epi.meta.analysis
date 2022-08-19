## shiny app script for epi.meta.analysis--------------------------------------

print("foo0")
# load app.R helper file
# source("./R/app_helper.R")
print("foo1/2")

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
library(googlesheets4)
library(shiny)
library(shinydashboard)
library(plotly)
library(magrittr)
library(DT)
library(forcats)
library(rnaturalearth)

# global variables

who_pm2.5_guideline <- 5

# epi studies analysis raw sheet
# epi <- readxl::read_xlsx("./data-raw/pm2.5_distribution/AQLI_Epidemiology Literature Research.xlsx", sheet = "AnalysisDatasetPM2.5MortalityAn")

# AQLI color file
# aqli_color <- read_csv("./data-raw/pm2.5_distribution/color.csv")

print("foo")
epi <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1AljEJhNPLWX_8xRbT_HJuERBpbQt_QGixgJ9jEzFyQw/edit#gid=2082201996", sheet = "[AnalysisDataset]PM2.5MortalityAnalysisDataset")
print("foo1")

#> change default columns types
epi$cohort_size <- as.numeric(epi$cohort_size)
print("foo2")
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



#------------------------------------------------------------------------------


# setting global options
options(shiny.maxRequestSize = 900*1024^2)

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "all.RData")
load("all.RData", .GlobalEnv)

ui <- shinydashboard::dashboardPage(
  skin = "black",
 shinydashboard::dashboardHeader(
    title = "AQ Epi Studies"
  ),
 shinydashboard::dashboardSidebar(
   shinydashboard::sidebarMenu(
     shinydashboard::menuItem(tabName = "blog_post_graphs", text = "Blog Post Graphs", icon = icon("chart-bar")),
     shinydashboard::menuItem(tabName = "other_content", text = "Other Content", icon = icon("pencil"))
    )
  ),
 shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    shinydashboard::tabItem("blog_post_graphs",
     shiny::fluidRow(
      shinydashboard::valueBoxOutput("num_papers", width = 3),
      shinydashboard::valueBoxOutput("cohort_size_range",  width = 3),
      shinydashboard::valueBoxOutput("study_publishing_year_range",  width = 3),
      shinydashboard::valueBoxOutput("num_of_countries_covered",  width = 3)
       ),
       shiny::fluidRow(
        shiny::tags$hr(),
        shinydashboard::box(width = 2,
         shiny::sliderInput("pm2.5_bucket", "PM2.5 Range", 0, 120, value = c(0, 120))
           ),
        shinydashboard::box(
           width = 4, status = "info", solidHeader = TRUE,
           title = "PM2.5 exposure Range and Global Population Distribution",
           plotly::plotlyOutput("pm2.5_expo_glob_pop_graph")
         ),
        shinydashboard::box(
           width = 2,
           shiny::selectInput("countries_fig2", "Countries", choices = epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% unlist() %>% unique(), multiple = TRUE, selected = "USA")
         ),
        shinydashboard::box(
           width = 4, status = "info", solidHeader = TRUE,
           title = "Country Wise Mean PM2.5 Distribution",
           plotly::plotlyOutput("mean_pm2.5_dist_country_wise_graph")
         )
       ),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(
           width = 5,
          shiny::selectInput("countries_fig3", "Countries", choices = c("all", "United States", epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% dplyr::filter(country != "USA") %>% unlist() %>% unique()), multiple = TRUE, selected = "all"),
          shiny::dataTableOutput("geographic_dist_studies_table")
         ),
         shinydashboard::box(
           status = "info", solidHeader = TRUE,
           title = "Geographic Distribution of Studies",
           width = 7,
           plotly::plotlyOutput("geographic_dist_studies")
         )
       ),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(
           width = 4,
           shiny::sliderInput("year_range", "Year Range", min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE), value = c(min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE))),
           DT::dataTableOutput("aq_epi_studies_over_time")
         ),
         shinydashboard::box(
           width = 8, status = "info", solidHeader = TRUE,
           title = "AQ Epi Studies Over Time",
           plotly::plotlyOutput("epi_studies_over_time_graph")
         )
       ),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(
           width = 3,
           shiny::selectInput("continent_list", "Continents", choices = c(epi %>% dplyr::filter(!is.na(continent)) %>% dplyr::select(continent) %>% unlist() %>% unique(), "Africa"), multiple = TRUE, selected = "North America"),
           hr(),
           shiny::selectInput("plot_type_fig5", "Plot Type", choices = c("Histogram (Frequency)", "Histogram (Density)"), selected = "Histogram (Frequency)")
         ),
         shinydashboard::box(status = "info", solidHeader = TRUE,
           title = "Epi Studies Duration Distribution (Continent Wise): Histogram",
           width = 9,
           plotly::plotlyOutput("continent_wise_dist_duration_study_graph")
         )
       ),
       shiny::tags$hr(),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(width = 12,
             status = "info", solidHeader = TRUE,
             title = "Other Graphs")
       ),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(
           width = 2,
           status = "info",
           solidHeader = TRUE,
           shiny::selectInput("plot_type_fig6", "Plot Type", choices = c("Histogram", "Density"), selected = "Histogram")
         ),

         shinydashboard::box(title = "Lower and Upper Limit Distributions of PM2.5 exposure range",
           width = 5,
             status = "info",
             solidHeader = TRUE,
             plotly::plotlyOutput("pm2.5_ll_ul_dist_graph")),

         shinydashboard::box(title = "Lower and Upper Limit Distributions of Age Range covered in the analysis",
             width = 5,
             status = "info",
             solidHeader = TRUE,
             plotly::plotlyOutput("age_ll_ul_graph"))
       ),
       shiny::tags$hr(),
       shiny::fluidRow(
         shinydashboard::box(
           width = 2,
           status = "info",
           solidHeader = TRUE,
           shiny::selectInput("countries_list_fig7", "Countries", choices = c("all", epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% unlist() %>% unique()), multiple = TRUE, selected = "all")
         ),
         shinydashboard::box(title = "Country Wise Distribution of Log Cohort Size",
             width = 5,
             status = "info",
             solidHeader = TRUE,
             plotly::plotlyOutput("country_wise_dist_log_cohort_size_graph")),
         shinydashboard::box(title = "Country Wise Distribution of Study Duration",
             width = 5,
             status = "info",
             solidHeader = TRUE,
             plotly::plotlyOutput("country_wise_dist_study_duration"))
       )

      ),
      shinydashboard::tabItem("other_content")

      )
    )
  )


# server------------------------------------------------------------------------
server <- function(input, output) {

  #> Summary Stats

  # number of papers value box
  output$num_papers <- shinydashboard::renderValueBox({
   # num_papers
    num_papers <- as.numeric(nrow(epi))

    shinydashboard::valueBox(
      value = shiny::tags$p(stringr::str_c(num_papers, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Number of Papers Analyzed",
      icon = shiny::icon("file-circle-check fa-2xs")
    )
  })

  # cohort size range value box
  output$cohort_size_range <- shinydashboard::renderValueBox({
    # cohort size range
    cohort_size_ll <- min(epi$cohort_size, na.rm = TRUE)
    cohort_size_ul_millions <- round(max(epi$cohort_size, na.rm = TRUE)/1000000, 1)
    final_cohort_range_string <- stringr::str_c(cohort_size_ll, "-", cohort_size_ul_millions, "million", sep = " ")

    shinydashboard::valueBox(
      value = tags$p(stringr::str_c(final_cohort_range_string, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Cohort Size Range",
      icon = shiny::icon("people-line fa-2xs")
    )
  })

  # study publishing value box
  output$study_publishing_year_range <- shinydashboard::renderValueBox({
    # study duration range
    study_publishing_min <- min(epi$publishing_year, na.rm = TRUE)
    study_publishing_max <-  max(epi$publishing_year, na.rm = TRUE)
    final_study_duration_string <- stringr::str_c(study_publishing_min, "-", study_publishing_max, sep = " ")

    shinydashboard::valueBox(
      value = tags$p(stringr::str_c(final_study_duration_string, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Study Publishing Year Range",
      icon = shiny::icon("calendar-days fa-2xs")
    )



  })

  # Number of countries covered value box

  output$num_of_countries_covered <- shinydashboard::renderValueBox({

    num_countries_covered <- epi %>% filter(!is.na(country)) %>% select(country) %>% unlist() %>% unique() %>% length()

    shinydashboard::valueBox(
      value = tags$p(stringr::str_c(num_countries_covered, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Number of Countries Covered",
      icon = shiny::icon("earth-americas fa-2xs")
    )

  })

  #> pm2.5 and global population distribution graph
  output$pm2.5_expo_glob_pop_graph <- plotly::renderPlotly({



  # percent population in pollution buckets
  tot_pop_in_bucket <-  aqli_color %>%
      dplyr::filter(pm2020 >= input$pm2.5_bucket[1], pm2020 <= input$pm2.5_bucket[2]) %>%
      dplyr::summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
    unlist()

  world_pop <- aqli_color %>%
    dplyr::summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
    unlist()

  percent_pop_in_bucket <- round((tot_pop_in_bucket/world_pop)*100, 1)

  # percent studies in the given pollution bucket
  tot_studies_in_bucket <- epi %>%
    dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA") %>%
    dplyr::filter(mean_pm2.5 >= input$pm2.5_bucket[1], mean_pm2.5 <= input$pm2.5_bucket[2]) %>%
    nrow()

  tot_studies_overall <- nrow(epi %>%
                                dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA"))

  percent_studies_in_bucket <- round((tot_studies_in_bucket/tot_studies_overall)*100, 1)

  # creating a dataframe for plotting
  pm2.5_expo_glob_pop_df <- tibble::tibble(pm2.5_bucket = c(stringr::str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³"),
                                                       stringr::str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³")),
                                   prop_in_bucket = c(percent_pop_in_bucket, percent_studies_in_bucket),
                                   type_of_prop = c("Percent Population in PM2.5 bucket", "Percent Studies in PM2.5 bucket"))

  pm2.5_expo_glob_pop_df$type_of_prop <- as.factor(pm2.5_expo_glob_pop_df$type_of_prop)

  plt <- pm2.5_expo_glob_pop_df %>%
  ggplot2::ggplot() +
    ggplot2::geom_col(mapping = ggplot2::aes(x = pm2.5_bucket, y = prop_in_bucket, fill = type_of_prop), position = position_dodge(), width = 0.4) +
  ggplot2::scale_fill_manual(values = c("Percent Population in PM2.5 bucket" = "grey", "Percent Studies in PM2.5 bucket" = "cornflowerblue"), labels = c("Proportion of World Population in Bucket", "Proportion of Studies Completed in Bucket")) +
  ggplot2::labs(x = "Mean PM2.5 bucket (in µg/m³)",  y = "Percentage", fill = "") +
  ggthemes::theme_hc() +
  ggplot2::theme(axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))


  return(plotly::ggplotly(plt))

  })

#> Country wise PM2.5 distribution Graph


  output$mean_pm2.5_dist_country_wise_graph <- plotly::renderPlotly({

    # sanity checks
    if(length(input$countries_fig2) == 0){
      stop("Please select atleast one country to proceed.")
    }

    mean_pm2.5_country_wise_graph <- epi %>%
      dplyr::filter(!is.na(mean_pm2.5) | mean_pm2.5 != "NA", country %in% input$countries_fig2) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mean_pm2.5, fill = country), alpha = 0.5, color = "black", position = "identity") +
      ggplot2::labs(x = "Mean PM2.5 concentration (in µg/m³)") +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

    return(plotly::ggplotly(mean_pm2.5_country_wise_graph))
  })

#> Geographic Distribution of Studies Graph

  output$geographic_dist_studies <-  plotly::renderPlotly({
    # generate summary dataset for total number of studies conducted in a given country
    epi_country_count <- epi %>%
      dplyr::select(country) %>%
    dplyr::count(country) %>%
      dplyr::filter(country != "NA")

    # making country names match before joining epi file with color file
    epi_country_count$country <- stringr::str_replace(epi_country_count$country, "USA", "United States")

    #> plot choropleth map

    # get data ready for the choropleth map
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


    # making country names match before joining epi file with color file
    world$name_long <- stringr::str_replace(world$name_long, "Russian Federation", "Russia")
    world$name_long <- stringr::str_replace(world$name_long, "Lao PDR", "Laos")
    world$name_long <- stringr::str_replace(world$name_long, "Dem. Rep. Korea", "North Korea")
    world$name_long <- stringr::str_replace(world$name_long, "Republic of Korea", "South Korea")
    world$name_long <- stringr::str_replace(world$name_long, "Vatican", "Vatican City")
    world$name_long <- stringr::str_replace(world$name_long, "Brunei Darussalam", "Brunei")
    world$name_long <- stringr::str_replace(world$name_long, "Somaliland", "Somalia")

    # joining world shape file with the epi conutry wise count dataset
    world_shp_epi <- world %>%
      dplyr::left_join(epi_country_count, by = c("name_long" = "country")) %>%
      dplyr::select(name_long, n, geometry) %>%
     dplyr::rename(num_studies = n, country = name_long)

    # color 2020 collapse to country level to get the PM2.5 pollution layer (first layer of the map)
    aqli_color_country <- aqli_color %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm2020_pop_weighted = pm2020*pop_weights) %>%
      dplyr::summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE))

    # joining the world shape + epi joined file with the aqli color file
    world_shp_epi_color <- world_shp_epi %>%
      dplyr::left_join(aqli_color_country, by = "country") %>%
      dplyr::filter(country != "Antarctica") %>%
      dplyr::select(country, num_studies, avg_pm2.5_2020, geometry)

    # In the total number of studies column, replacing NAs with 0s
    world_shp_epi_color <- world_shp_epi_color %>%
      dplyr::mutate(num_studies = ifelse(is.na(num_studies) == TRUE, 0, num_studies))

    # getting centroids for countries after correcting for problematic polygons
    world_shp_epi_color_for_centroids <<- sf::st_make_valid(world_shp_epi_color)
    country_wise_centroids <<- sf::st_centroid(world_shp_epi_color_for_centroids, of_largest_polygon = TRUE)

    # plotting the choropleth

    if(length(input$countries_fig3) == 0){
      stop("Please select atleast one country to proceed.")
    } else if(("all" %in% input$countries_fig3) & (length(input$countries_fig3)) > 1){
      stop("Please choose either multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_fig3){
      # options(repr.plot.width = 50, repr.plot.height = 50)
        geographic_dist_graph <- world_shp_epi_color %>%
          ggplot2::ggplot() +
          ggplot2::geom_sf(mapping = ggplot2::aes(fill = avg_pm2.5_2020), color = "white") +
          colorspace::scale_fill_continuous_sequential(palette = "YlOrRd") +
          ggplot2::geom_sf(data = country_wise_centroids %>% dplyr::filter(num_studies != 0), mapping = ggplot2::aes(size = num_studies), col = "grey", ) +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::scale_size_binned() +
          ggplot2::scale_size(range = c(0, 8)) +
        ggthemes::theme_map()
        return(plotly::ggplotly(geographic_dist_graph))

      } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)) {
        geographic_dist_graph <- world_shp_epi_color %>%
          filter(country %in% input$countries_fig3) %>%
          ggplot2::ggplot() +
          ggplot2::geom_sf(mapping = ggplot2::aes(fill = avg_pm2.5_2020), color = "white") +
          colorspace::scale_fill_continuous_sequential(palette = "YlOrRd") +
          ggplot2::geom_sf(data = country_wise_centroids %>% dplyr::filter(num_studies != 0, country %in% input$countries_fig3), mapping = ggplot2::aes(size = num_studies), col = "grey") +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::scale_size_binned() +
          ggplot2::scale_size(range = c(0, 8)) +
          ggthemes::theme_map()
        return(plotly::ggplotly(geographic_dist_graph))
      }

    })

#> Geographic Distribution of Studies table

output$geographic_dist_studies_table <- shiny::renderDataTable({
  if(length(input$countries_fig3) == 0){
    stop("Please select atleast one country to proceed.")
  } else if(("all" %in% input$countries_fig3) & (length(input$countries_fig3)) > 1){
    stop("Please either choose one/multiple countries, or 'all', but not both")
  }

  if("all" %in% input$countries_fig3){
   dt_all <- country_wise_centroids %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(num_studies != 0)
   DT::datatable(dt_all, options = list(pageLength = 6))
  } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)){
   dt_filter <- country_wise_centroids %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(num_studies !=0, country %in% input$countries_fig3)
   DT::datatable(dt_filter, options = list(pageLength = 6))
  }
})

#> Epi studies over time graph

output$epi_studies_over_time_graph <- plotly::renderPlotly({
  epi %>%
    dplyr::group_by(publishing_year) %>%
    dplyr::summarise(total_studies = dplyr::n()) %>%
    dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = publishing_year, y = total_studies), lwd = 1.2, color = "cornflowerblue") +
    ggplot2::labs(x = "Year", y = "Total Number of Studies") +
    ggthemes::theme_hc() +
    ggplot2::theme(axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black")) %>%
    return()
})

#> Epi studies over time table

output$aq_epi_studies_over_time <- renderDataTable({
  dt_filter_epi_over_time <-   epi %>%
    dplyr::group_by(publishing_year) %>%
    dplyr::summarise(total_studies = dplyr::n()) %>%
    dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2])
  DT::datatable(dt_filter_epi_over_time, options = list(pageLength = 6))

})

#> Continent Wise Distribution of Duration of Study graph

output$continent_wise_dist_duration_study_graph <- plotly::renderPlotly({
  epi_with_africa <- epi %>%
    dplyr::filter(continent != "NA") %>%
    dplyr::add_row(continent = "Africa")

  # adding an order panel in the epi aftica dataset
  epi_with_africa <- epi_with_africa %>%
    dplyr::mutate(order_continent = ifelse(continent == "Asia", 1, 0),
           order_continent = ifelse(continent == "Europe", 2, order_continent),
           order_continent = ifelse(continent == "North America", 3, order_continent),
           order_continent = ifelse(continent == "South America", 4, order_continent),
           order_continent = ifelse(continent == "Africa", 5, order_continent))


  # plot the figure with empty Africa panel

  if(length(input$continent_list) == 0){
    stop("Please select atleast one continent to proceed.")
  }
  if(input$plot_type_fig5 == "Histogram (Frequency)"){
    continent_wise_study_duration_graph <- epi_with_africa %>%
      dplyr::filter(continent %in% input$continent_list) %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = study_duration, fill = continent), alpha = 0.5, color = "white", position = "identity", binwidth = 2) +
      ggplot2::labs(x = "Study Duration") +
      ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
      ggthemes::theme_hc() +
      ggplot2::theme(legend.title = element_blank()) +
      ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  } else if (input$plot_type_fig5 == "Histogram (Density)"){
    continent_wise_study_duration_graph <- epi_with_africa %>%
      dplyr::filter(continent %in% input$continent_list) %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = study_duration, y = ..density.., fill = continent), alpha = 0.5, color = "white", binwidth = 2) +
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = study_duration, y = ..density.., fill = continent), alpha = 0.5, color = "white", binwidth = 2) +
      ggplot2::labs(x = "Study Duration") +
      ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
      ggthemes::theme_hc() +
      ggplot2::theme(legend.title = element_blank()) +
      ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  }


   plotly::ggplotly(continent_wise_study_duration_graph)

})

#> Distribution of lower and upper limits of PM2.5 exposure range

output$pm2.5_ll_ul_dist_graph <- plotly::renderPlotly({
  # create long dataset
  epi_long <- epi %>%
  tidyr::pivot_longer(cols = contains("pm2.5_exposure"), names_to =  "exposure_type", values_to = "exposure_value") %>%
    dplyr::select(exposure_type, exposure_value) %>%
    dplyr::filter(!is.na(exposure_value))

  if(input$plot_type_fig6 == "Histogram"){
    epi_long %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type), position = "identity", alpha = 0.4, color = "white") +
      ggplot2::scale_x_continuous(breaks = seq(0, 250, 10)) +
      ggplot2::labs(x = "PM2.5 concentration (in µg/m³)") +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  } else if(input$plot_type_fig6 == "Density"){
    epi_long %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type), alpha = 0.6, position = "identity") +
      ggplot2::scale_x_continuous(breaks = seq(0, 250, 10)) +
      ggplot2::labs(x = "PM2.5 concentration (in µg/m³)") +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))


  }



})

#> Country Wise Distribution of Log Cohort Size Graph

output$country_wise_dist_log_cohort_size_graph <- plotly::renderPlotly({

  if(length(input$countries_list_fig7) == 0){
    stop("Please select atleast one country to proceed.")
  } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
    stop("Please either choose one/multiple countries, or 'all', but not both")
  }

  if("all" %in% input$countries_list_fig7){
    epi %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()

  } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
    epi %>%
      dplyr::filter(country %in% input$countries_list_fig7) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()
  }



})

#> Distribution of lower limit and upper limit of ages covered in this analysis

output$age_ll_ul_graph <- plotly::renderPlotly({

  if(input$plot_type_fig6 == "Histogram"){
    epi %>%
      tidyr::pivot_longer(cols = contains("cohort_age"), names_to =  "age_dist_type", values_to = "age_value") %>%
      dplyr::select(age_dist_type, age_value) %>%
      dplyr::filter(!is.na(age_value)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = age_value, fill = age_dist_type), alpha = 0.5, position = "identity", color = "white") +
      ggplot2::scale_x_continuous(breaks = seq(0, 90, 5)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 15, 2)) +
      ggplot2::labs(x = "Age") +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()

  } else if(input$plot_type_fig6 == "Density"){
    epi %>%
      tidyr::pivot_longer(cols = contains("cohort_age"), names_to =  "age_dist_type", values_to = "age_value") %>%
      dplyr::select(age_dist_type, age_value) %>%
      dplyr::filter(!is.na(age_value)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = age_value, fill = age_dist_type), alpha = 0.6, color = "black") +
      ggplot2::scale_x_continuous(breaks = seq(0, 90, 5)) +
      ggplot2::labs(x = "Age") +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()
  }

})

#> Country wise distribution of study duration

output$country_wise_dist_study_duration <- plotly::renderPlotly({
  if(length(input$countries_list_fig7) == 0){
    stop("Please select atleast one country to proceed.")
  } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
    stop("Please either choose one/multiple countries, or 'all', but not both")
  }

  if("all" %in% input$countries_list_fig7){
    epi %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, alpha = 0.5)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()
  } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
    epi %>%
      dplyr::filter(country %in% input$countries_list_fig7) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, alpha = 0.5)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()
  }


})

}

shiny::shinyApp(ui, server)

