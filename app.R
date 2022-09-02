## shiny app script for epi.meta.analysis--------------------------------------

# load app.R helper file
# source("./R/app_helper.R")

# libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(magrittr)
library(DT)
library(forcats)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinycssloaders)
library(waiter)

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
                                  shinycssloaders::withSpinner(plotly::plotlyOutput("pm2.5_expo_glob_pop_graph"))
                                ),
                                shinydashboard::box(
                                  width = 2,
                                  shiny::selectInput("countries_fig2", "Countries", choices = epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% unlist() %>% unique(), multiple = TRUE, selected = "USA")
                                ),
                                shinydashboard::box(
                                  width = 4, status = "info", solidHeader = TRUE,
                                  title = "Country Wise Mean PM2.5 Distribution",
                                  shinycssloaders::withSpinner(plotly::plotlyOutput("mean_pm2.5_dist_country_wise_graph"))
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
                                  shinycssloaders::withSpinner(plotly::plotlyOutput("geographic_dist_studies"))
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
                                  shinycssloaders::withSpinner(plotly::plotlyOutput("epi_studies_over_time_graph"))
                                )
                              ),
                              shiny::tags$hr(),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 3,
                                  shiny::selectInput("continent_list", "Continents", choices = continent_list, multiple = TRUE, selected = "North America"),
                                  hr(),
                                  shiny::selectInput("plot_type_fig5", "Plot Type", choices = c("Histogram (Frequency)", "Histogram (Density)"), selected = "Histogram (Frequency)")
                                ),
                                shinydashboard::box(status = "info", solidHeader = TRUE,
                                                    title = "Epi Studies Duration Distribution (Continent Wise): Histogram",
                                                    width = 9,
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("continent_wise_dist_duration_study_graph"))
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
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("pm2.5_ll_ul_dist_graph"))
                                ),

                                shinydashboard::box(title = "Lower and Upper Limit Distributions of Age Range covered in the analysis",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("age_ll_ul_graph")))
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
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("country_wise_dist_log_cohort_size_graph"))),
                                shinydashboard::box(title = "Country Wise Distribution of Study Duration",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("country_wise_dist_study_duration")))
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
    # num_papers (not counting different countries in a pooled study separately, but including all pollutants)
    num_papers <- as.numeric(length(unique(epi$paper_uid)))

    shinydashboard::valueBox(
      value = shiny::tags$p(stringr::str_c(num_papers, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Number of Papers Analyzed",
      icon = shiny::icon("file-circle-check fa-2xs")
    )
  })

  # cohort size range value box
  output$cohort_size_range <- shinydashboard::renderValueBox({
    # cohort size range (counting different studies in a pooled study separately, and including all pollutants)
    cohort_size_ll <- epi %>%
      dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
      dplyr::select(cohort_size) %>%
      min(na.rm = TRUE) %>%
      unlist()
    cohort_size_ul <- epi %>%
      dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
      dplyr::select(cohort_size) %>%
      max(na.rm = TRUE) %>%
      unlist()
    cohort_size_ul_millions <- round(cohort_size_ul/1000000, 1)
    final_cohort_range_string <- stringr::str_c(cohort_size_ll, "-", cohort_size_ul_millions, "million", sep = " ")

    shinydashboard::valueBox(
      value = tags$p(stringr::str_c(final_cohort_range_string, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Cohort Size Range",
      icon = shiny::icon("people-line fa-2xs")
    )
  })

  # study publishing value box
  output$study_publishing_year_range <- shinydashboard::renderValueBox({
    # study duration range (count multiple countries in a pooled study to be a single country, include all pollutants)
    study_publishing_min <- min(epi$publishing_year, na.rm = TRUE)
    study_publishing_max <- ifelse(max(epi$publishing_year, na.rm = TRUE) < 2022, "Present", max(epi$publishing_year, na.rm = TRUE))
    final_study_duration_string <- stringr::str_c(study_publishing_min, "-", study_publishing_max, sep = " ")

    shinydashboard::valueBox(
      value = tags$p(stringr::str_c(final_study_duration_string, "", sep = ""), style = "font-size: 75%;"),
      subtitle = "Study Publishing Year Range",
      icon = shiny::icon("calendar-days fa-2xs")
    )



  })

  # Number of countries covered value box

  output$num_of_countries_covered <- shinydashboard::renderValueBox({

    num_countries_covered <- epi %>% filter(!is.na(country), country != "NA") %>% select(country) %>% unlist() %>% unique() %>% length()

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
      dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0) %>%
      dplyr::group_by(paper_uid) %>%
      dplyr::summarise(mean_pm2.5 = mean(mean_pm2.5, na.rm = TRUE)) %>%
      dplyr::filter(mean_pm2.5 >= input$pm2.5_bucket[1], mean_pm2.5 <= input$pm2.5_bucket[2]) %>%
      nrow()

    tot_studies_overall <- nrow(epi %>%
                                  dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
                                  dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0))

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
      ggplot2::labs(x = "Mean PM2.5 bucket (in µg/m³)",  y = "Percentage", fill = "",
                    caption = stringr::str_wrap("*This graph 'only' takes into account the PM2.5 specific studies. For multi-country (pooled) studies, it averages the mean PM2.5 values, across all countries."), width = 17) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                     axis.line.x = ggplot2::element_line(color = "black"),
                     plot.caption = ggplot2::element_text(hjust = 0))


    return(plotly::ggplotly(plt))

  })

  #> Country wise PM2.5 distribution Graph


  output$mean_pm2.5_dist_country_wise_graph <- plotly::renderPlotly({

    # sanity checks
    if(length(input$countries_fig2) == 0){
      stop("Please select atleast one country to proceed.")
    }

    mean_pm2.5_country_wise_graph <- epi %>%
      dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0, country %in% input$countries_fig2) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mean_pm2.5, fill = country), alpha = 0.5, color = "black", position = "identity") +
      ggplot2::labs(x = "Mean PM2.5 concentration (in µg/m³)",
                    caption = stringr::str_wrap("*This graph 'only' takes into account the PM2.5 specific studies. For multi-country (pooled) studies, it averages the mean PM2.5 values, across all countries."), width = 10) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                     axis.line.x = ggplot2::element_line(color = "black"),
                     plot.caption = ggplot2::element_text(hjust = 0))

    return(plotly::ggplotly(mean_pm2.5_country_wise_graph))
  })

  #> Geographic Distribution of Studies Graph

  output$geographic_dist_studies <-  plotly::renderPlotly({
    # generate summary dataset for total number of studies conducted in a given country
    epi_country_count <- epi %>%
      dplyr::select(country) %>%
      dplyr::count(country) %>%
      dplyr::filter(country != "NA", !is.na(country), !is.na(n))

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
      ggplot2::labs(caption = stringr::str_wrap("*The size of the circles is directly proportional to the total number of times a given country has been included in a study (any study, not PM2.5 specific)."), width = 10) +
        ggthemes::theme_map() +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
      return(plotly::ggplotly(geographic_dist_graph))

    } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)) {
      geographic_dist_graph <- world_shp_epi_color %>%
        dplyr::filter(country %in% input$countries_fig3) %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(mapping = ggplot2::aes(fill = avg_pm2.5_2020), color = "white") +
        colorspace::scale_fill_continuous_sequential(palette = "YlOrRd") +
        ggplot2::geom_sf(data = country_wise_centroids %>% dplyr::filter(num_studies != 0, country %in% input$countries_fig3), mapping = ggplot2::aes(size = num_studies), col = "grey") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_size_binned() +
        ggplot2::scale_size(range = c(0, 8)) +
       ggplot2::labs(caption = stringr::str_wrap("*The size of the circles is directly proportional to the total number of times a given country has been included in a study (any study, not PM2.5 specific)."), width = 10)
        ggthemes::theme_map() +
          ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
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
      dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
      dplyr::group_by(publishing_year) %>%
      dplyr::summarise(total_studies = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2], total_studies != "NA", !is.na(total_studies)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = publishing_year, y = total_studies), lwd = 1.2, color = "cornflowerblue") +
      ggplot2::labs(x = "Year", y = "Total Number of Studies",
                    caption = stringr::str_wrap("*This graph displays the total number of AQ epi studies (all studies, not PM2.5 specific) published over time."), width = 10) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                     axis.line.x = ggplot2::element_line(color = "black"),
                     plot.caption = ggplot2::element_text(hjust = 0)) %>%
      return()
  })

  #> Epi studies over time table

  output$aq_epi_studies_over_time <- renderDataTable({
    dt_filter_epi_over_time <-   epi %>%
      dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
      dplyr::group_by(publishing_year) %>%
      dplyr::summarise(total_studies = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2], total_studies != "NA", !is.na(total_studies))
    DT::datatable(dt_filter_epi_over_time, options = list(pageLength = 6))

  })

  #> Continent Wise Distribution of Duration of Study graph



  output$continent_wise_dist_duration_study_graph <- plotly::renderPlotly({


    # adding logic to check for missing continents and automatically adding missing panels for that

    if(sum(missing_continents_logical) == 0){
      continent_wise_study_duration_graph_summary_table <- epi %>%
        dplyr::filter(continent != "NA", !is.na(continent), study_duration != "NA", !is.na(study_duration)) %>%
        dplyr::group_by(paper_uid, continent) %>%
        dplyr::summarise(average_study_duration = mean(study_duration, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(order_continent = ifelse(continent == "Asia", 1, 0),
               order_continent = ifelse(continent == "Europe", 2, order_continent),
               order_continent = ifelse(continent == "North America", 3, order_continent),
               order_continent = ifelse(continent == "South America", 4, order_continent),
               order_continent = ifelse(continent == "Africa", 5, order_continent),
               order_continent = ifelse(continent == "Oceania", 6, order_continent))
    } else{
      for(i in 1:length(missing_continents)){
        if(i == 1){
          continent_wise_study_duration_graph_summary_table <- epi %>%
            dplyr::filter(continent != "NA", !is.na(continent), study_duration != "NA", !is.na(study_duration)) %>%
            dplyr::group_by(paper_uid, continent) %>%
            dplyr::summarise(average_study_duration = mean(study_duration, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
          dplyr::add_row(continent = missing_continents[i])
        } else{
          continent_wise_study_duration_graph_summary_table <- continent_wise_study_duration_graph_summary_table %>%
            dplyr::add_row(continent = missing_continents[i])
        }

      }
      continent_wise_study_duration_graph_summary_table <- continent_wise_study_duration_graph_summary_table %>%
        mutate(order_continent = ifelse(continent == "Asia", 1, 0),
               order_continent = ifelse(continent == "Europe", 2, order_continent),
               order_continent = ifelse(continent == "North America", 3, order_continent),
               order_continent = ifelse(continent == "South America", 4, order_continent),
               order_continent = ifelse(continent == "Africa", 5, order_continent),
               order_continent = ifelse(continent == "Oceania", 6, order_continent))
    }


    #----------------



    # plot the figure with empty Africa panel

    if(length(input$continent_list) == 0){
      stop("Please select atleast one continent to proceed.")
    }
    if(input$plot_type_fig5 == "Histogram (Frequency)"){
      continent_wise_study_duration_graph <- continent_wise_study_duration_graph_summary_table %>%
        dplyr::filter(continent %in% input$continent_list) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = average_study_duration, fill = continent), alpha = 0.5, color = "white", position = "identity", binwidth = 2) +
        ggplot2::labs(x = "Study Duration",
                      caption = stringr::str_wrap("*This graph displays distribution distribution by continent for all studies (not PM2.5 specific)"), width = 10) +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
        ggthemes::theme_hc() +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0))

    } else if (input$plot_type_fig5 == "Histogram (Density)"){
      continent_wise_study_duration_graph <- continent_wise_study_duration_graph_summary_table %>%
        dplyr::filter(continent %in% input$continent_list) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = average_study_duration, y = ..density.., fill = continent), alpha = 0.5, color = "white", binwidth = 2) +
        ggplot2::labs(x = "Study Duration",
                      caption = stringr::str_wrap("*This graph displays distribution distribution by continent for all studies (not PM2.5 specific)"), width = 10) +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
        ggthemes::theme_hc() +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0))

    }


    plotly::ggplotly(continent_wise_study_duration_graph)

  })

  #> Distribution of lower and upper limits of PM2.5 exposure range

  # Note: This is PM2.5 specific and different countries in a pooled study are counted separately.

  output$pm2.5_ll_ul_dist_graph <- plotly::renderPlotly({
    # create long dataset
    epi_long <- epi %>%
      dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0) %>%
      tidyr::pivot_longer(cols = contains("pm2.5_exposure"), names_to =  "exposure_type", values_to = "exposure_value") %>%
      dplyr::select(paper_uid, exposure_type, exposure_value) %>%
      dplyr::filter(!is.na(exposure_value))

    if(input$plot_type_fig6 == "Histogram"){
      epi_long %>%
        dplyr::group_by(paper_uid, exposure_type) %>%
        dplyr::summarise(exposure_value = ifelse(exposure_type == "pm2.5_exposure_ll", min(exposure_value, na.rm = TRUE), max(exposure_value, na.rm = TRUE))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type), position = "identity", alpha = 0.4, color = "white") +
        ggplot2::labs(x = "PM2.5 concentration (in µg/m³)",
                      caption = stringr::str_wrap("*This graph takes into account only those PM2.5 studies that have atleast one of 'pm2.5 lower limit value', 'pm2.5 upper limit value'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0))

    } else if(input$plot_type_fig6 == "Density"){
      epi_long %>%
        dplyr::group_by(paper_uid, exposure_type) %>%
        dplyr::summarise(exposure_value = ifelse(exposure_type == "pm2.5_exposure_ll", min(exposure_value, na.rm = TRUE), max(exposure_value, na.rm = TRUE))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type), alpha = 0.6, position = "identity") +
        ggplot2::labs(x = "PM2.5 concentration (in µg/m³)",
                      caption = stringr::str_wrap("*This graph takes into account only those PM2.5 studies that have atleast one of 'pm2.5 lower limit value', 'pm2.5 upper limit value'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0))


    }



  })

  #> Country Wise Distribution of Log Cohort Size Graph

  # Different countries in a pooled study are counted separately, but this is not a PM2.5 specific graph.

  output$country_wise_dist_log_cohort_size_graph <- plotly::renderPlotly({

    if(length(input$countries_list_fig7) == 0){
      stop("Please select atleast one country to proceed.")
    } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
      stop("Please either choose one/multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_list_fig7){
      epi %>%
        dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
        ggthemes::theme_hc() +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM2.5 studies) in which a cohort/sample size is available."), width = 10) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()

    } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
      epi %>%
        dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
        dplyr::filter(country %in% input$countries_list_fig7) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM2.5 studies) in which a cohort/sample size is available."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()
    }


  })

  #> Distribution of lower limit and upper limit of ages covered in this analysis

  # This is not a PM2.5 specific graph and different countries in a cohort study are counted separately.

  output$age_ll_ul_graph <- plotly::renderPlotly({

    if(input$plot_type_fig6 == "Histogram"){
      epi %>%
        dplyr::filter(((!is.na(cohort_age_ll)) | (cohort_age_ll != "NA")) & ((!is.na(cohort_age_ul)) | (cohort_age_ul != "NA"))) %>%
        tidyr::pivot_longer(cols = contains("cohort_age"), names_to =  "age_dist_type", values_to = "age_value") %>%
        dplyr::select(age_dist_type, age_value) %>%
        dplyr::filter(!is.na(age_value)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = age_value, fill = age_dist_type), alpha = 0.5, position = "identity", color = "white") +
        ggplot2::scale_x_continuous(breaks = seq(0, 90, 5)) +
        ggplot2::scale_y_continuous(breaks = seq(0, 15, 2)) +
        ggplot2::labs(x = "Age",
                      caption = stringr::str_wrap("*This graph takes into account those studies  (can include non-PM2.5 studies) that have atleast one of 'age lower limit', 'age upper limit'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()

    } else if(input$plot_type_fig6 == "Density"){
      epi %>%
        dplyr::filter(((!is.na(cohort_age_ll)) | (cohort_age_ll != "NA")) & ((!is.na(cohort_age_ul)) | (cohort_age_ul != "NA"))) %>%
        tidyr::pivot_longer(cols = contains("cohort_age"), names_to =  "age_dist_type", values_to = "age_value") %>%
        dplyr::select(age_dist_type, age_value) %>%
        dplyr::filter(!is.na(age_value)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = age_value, fill = age_dist_type), alpha = 0.6, color = "black") +
        ggplot2::scale_x_continuous(breaks = seq(0, 90, 5)) +
        ggplot2::labs(x = "Age",
                      caption = stringr::str_wrap("*This graph takes into account those studies  (can include non-PM2.5 studies) that have atleast one of 'age lower limit', 'age upper limit'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()
    }

  })

  #> Country wise distribution of study duration

  # This is not a PM2.5 specific graph and in this all countries in a given pooled study are counted separately.

  # Country wise distribution

  output$country_wise_dist_study_duration <- plotly::renderPlotly({
    if(length(input$countries_list_fig7) == 0){
      stop("Please select atleast one country to proceed.")
    } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
      stop("Please either choose one/multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_list_fig7){
      epi %>%
        dplyr::filter(!is.na(study_duration), study_duration != "NA") %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, alpha = 0.5)) +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
        ggthemes::theme_hc() +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM2.5 studies) in which study duration is available."), width = 10 ) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()
    } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
      epi %>%
        dplyr::filter(!is.na(study_duration), study_duration != "NA") %>%
        dplyr::filter(country %in% input$countries_list_fig7) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, alpha = 0.5)) +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM2.5 studies) in which study duration is available."), width = 10 ) +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0)) %>%
        return()
    }


  })

}

shiny::shinyApp(ui, server)

