## shiny app script for epi.meta.analysis--------------------------------------

# load app.R helper file
# source("app_helper.R")

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
library(colorspace)

# setting global options
options(shiny.maxRequestSize = 900*1024^2)

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "all.RData")
load("all.RData", .GlobalEnv)


#----------------------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  skin = "black",
  shinydashboard::dashboardHeader(
    title = "AQ Epi Studies"
  ),
  shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(tabName = "blog_post_graphs_1", text = "Blog Post Graphs", icon = icon("chart-bar")),
      shinydashboard::menuItem(tabName = "underlying_data_table_app_1", text = "Underlying Data", icon = icon("database")),
      shinydashboard::menuItem(tabName = "other_content_1", text = "Other Content", icon = icon("pencil"))
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem("blog_post_graphs_1",
                              shiny::fluidRow(
                                shinydashboard::valueBoxOutput("num_papers", width = 3),
                                shinydashboard::valueBoxOutput("cohort_size_range_1",  width = 3),
                                shinydashboard::valueBoxOutput("study_publishing_year_range",  width = 3),
                                shinydashboard::valueBoxOutput("num_of_countries_covered",  width = 3)
                              ),
                              shiny::fluidRow(
                                shiny::tags$hr(),
                                shinydashboard::box(width = 3,
                                                    shiny::sliderInput("pm2.5_bucket_2_buckets_ll", "PM₂.₅ Range (LL)", 0, 120, value = 0)
                                ),
                                shinydashboard::box(width = 3,
                                                    shiny::sliderInput("pm2.5_bucket_2_buckets_ul", "PM₂.₅ Range (UL)", 0, 120, value = 15)
                                ),
                                shinydashboard::box(
                                  width = 6, status = "info", solidHeader = TRUE,
                                  title = "PM₂.₅ exposure Range and Global Population Distribution with 2 buckets",
                                  shinycssloaders::withSpinner(shiny::plotOutput("pm2.5_expo_glob_pop_graph_2_buckets"))
                                )
                              ),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 3,
                                  shiny::selectInput("countries_fig2", "Countries", choices = epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% unlist() %>% unique(), multiple = TRUE, selected = "USA")
                                ),
                                shinydashboard::box(
                                  width = 9, status = "info", solidHeader = TRUE,
                                  title = expression("Country Wise Mean PM₂.₅ Distribution"),
                                  shinycssloaders::withSpinner(shiny::plotOutput("mean_pm2.5_dist_country_wise_graph"))
                                )
                              ),
                              shiny::tags$hr(),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 2,
                                  shiny::selectInput("countries_fig3", "Countries", choices = c("all", "United States", epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% dplyr::filter(country != "USA") %>% unlist() %>% unique()), multiple = TRUE, selected = "all"),
                                  #shiny::dataTableOutput("geographic_dist_studies_table")
                                ),
                                shinydashboard::box(
                                  status = "info", solidHeader = TRUE,
                                  title = "Geographic Distribution of Studies and Annual Average PM₂.₅ pollution",
                                  width = 10,
                                  shinycssloaders::withSpinner(shiny::plotOutput("geographic_dist_studies"))
                                )
                              ),
                              shiny::tags$hr(),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 3,
                                  shiny::sliderInput("year_range", "Year Range", min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE), value = c(min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE)), sep = ""),
                                  #DT::dataTableOutput("aq_epi_studies_over_time")
                                ),
                                shinydashboard::box(
                                  width = 9, status = "info", solidHeader = TRUE,
                                  title = "AQ Epi Studies Over Time",
                                  shinycssloaders::withSpinner(shiny::plotOutput("epi_studies_over_time_graph"))
                                )
                              ),
                              shiny::tags$hr(),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 2,
                                  shiny::selectInput("continent_list", "Continents", choices = c("North America (589.2 million people)",
                                                                                                 "Europe (740.8 million people)",
                                                                                                 "Asia - China (3161.5 million people)",
                                                                                                 "Africa (1373.8 million people)",
                                                                                                 "South America (429.2 million people)",
                                                                                                 "China (1402.5 million people)"), multiple = TRUE, selected = "North America (589.2 million people)"),
                                  hr(),
                                  shiny::selectInput("plot_type_fig5", "Plot Type", choices = c("Histogram (Frequency)", "Histogram (Density)"), selected = "Histogram (Frequency)")
                                ),
                                shinydashboard::box(status = "info", solidHeader = TRUE,
                                                    title = "Epi Studies Duration Distribution (Continent Wise): Histogram",
                                                    width = 10,
                                                    shinycssloaders::withSpinner(shiny::plotOutput("continent_wise_dist_duration_study_graph"))
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

                                shinydashboard::box(title = "Lower and Upper Limit Distributions of PM₂.₅ exposure range",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(shiny::plotOutput("pm2.5_ll_ul_dist_graph"))
                                ),

                                shinydashboard::box(title = "Lower and Upper Limit Distributions of Age Range covered in the analysis",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(shiny::plotOutput("age_ll_ul_graph")))
                              ),
                              shiny::tags$hr(),
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 2,
                                  status = "info",
                                  solidHeader = TRUE,
                                  shiny::selectInput("countries_list_fig7", "Countries", choices = c("all", epi %>% dplyr::filter(!is.na(country)) %>% dplyr::select(country) %>% unlist() %>% unique()), multiple = TRUE, selected = "USA")
                                ),
                                shinydashboard::box(title = "Country Wise Distribution of Cohort Size",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("country_wise_dist_log_cohort_size_graph_1"))),
                                shinydashboard::box(title = "Country Wise Distribution of Study Duration",
                                                    width = 5,
                                                    status = "info",
                                                    solidHeader = TRUE,
                                                    shinycssloaders::withSpinner(plotly::plotlyOutput("country_wise_dist_study_duration_1")))
                              ),
                              shiny::tags$br()

                              # shiny::fluidRow(
                              #   shiny::tags$hr(),
                              #   shinydashboard::box(width = 4,
                              #                       shiny::sliderInput("pm2.5_bucket", "PM₂.₅ Range", 0, 120, value = c(0, 15))
                              #   ),
                              #   shinydashboard::box(
                              #     width = 8, status = "info", solidHeader = TRUE,
                              #     title = "PM₂.₅ exposure Range and Global Population Distribution",
                              #     shinycssloaders::withSpinner(shiny::plotOutput("pm2.5_expo_glob_pop_graph"))
                              #   )
                              # )

      ),
      shinydashboard::tabItem("underlying_data_table_app_1",
                              # shiny::fluidRow(
                              #  shinycssloaders::withSpinner(shiny::tableOutput("underlying_data_table_3"))
                              # )
                              ),
      shinydashboard::tabItem("other_content_1")

    )
  )
)


# server------------------------------------------------------------------------
server <- function(input, output, session) {

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
  output$cohort_size_range_1 <- shinydashboard::renderValueBox({
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
    cohort_size_ul_billions <- round((cohort_size_ul/1000000000), 1)
    final_cohort_range_string <- stringr::str_c(cohort_size_ll, "-", cohort_size_ul_billions, "billion", sep = " ")

      shinydashboard::valueBox(
        value = shiny::tags$p(stringr::str_c(final_cohort_range_string, "", sep = ""), style = "font-size: 75%;"),
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


  #> PM2.5 exposure and Global Population distribution with 2 buckets
  output$pm2.5_expo_glob_pop_graph_2_buckets <- shiny::renderPlot({
    return(pop_pol_graph_2_buckets(aqli_color, epi, input$pm2.5_bucket_2_buckets_ll, input$pm2.5_bucket_2_buckets_ul, "pm2021"))
  })

  #> Country wise PM2.5 distribution Graph


  output$mean_pm2.5_dist_country_wise_graph <- shiny::renderPlot({

    # sanity checks
    if(length(input$countries_fig2) == 0){
      stop("Please select atleast one country to proceed.")
    }

    mean_pm2.5_country_wise_graph <- epi %>%
      dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0, country %in% input$countries_fig2) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mean_pm2.5, fill = country, group = country), alpha = 0.5, color = "black", position = "identity") +
      ggplot2::labs(x = expression("Mean PM₂.₅ concentration (in µg/m³)"),
                    y = "Density",
                    fill = "Country",
                    caption = stringr::str_wrap("*This graph 'only' takes into account the PM₂.₅ specific studies. For multi-country (pooled) studies, it averages the mean PM₂.₅ values, across all countries."), width = 10) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                     axis.line.x = ggplot2::element_line(color = "black"),
                     plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                     legend.position = "bottom") +
      viridis::scale_fill_viridis(discrete = TRUE)

    return(mean_pm2.5_country_wise_graph)
  })

  #> Geographic Distribution of Studies Graph

  output$geographic_dist_studies <-  shiny::renderPlot({
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
    world$name_long <- stringr::str_replace(world$name_long, "Mexico", "México")
    world$name_long <- stringr::str_replace(world$name_long, "Republic of Congo", "Republic of the Congo")
    world$name_long <- stringr::str_replace(world$name_long, "Czech Republic", "Czechia")

    # joining world shape file with the epi conutry wise count dataset
    world_shp_epi <- world %>%
      dplyr::left_join(epi_country_count, by = c("name_long" = "country")) %>%
      dplyr::select(name_long, n, geometry) %>%
      dplyr::rename(num_studies = n, country = name_long)

    # color 2020 collapse to country level to get the PM2.5 pollution layer (first layer of the map)
    aqli_color_country <- aqli_color %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    pm2021_pop_weighted = pm2021*pop_weights) %>%
      dplyr::summarise(avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE))

    # joining the world shape + epi joined file with the aqli color file
    world_shp_epi_color <- world_shp_epi %>%
      dplyr::left_join(aqli_color_country, by = "country") %>%
      dplyr::filter(country != "Antarctica") %>%
      dplyr::select(country, num_studies, avg_pm2.5_2021, geometry)

    # In the total number of studies column, replacing NAs with 0s
    world_shp_epi_color <- world_shp_epi_color %>%
      dplyr::mutate(num_studies = ifelse(is.na(num_studies) == TRUE, 0, num_studies))

    # getting centroids for countries after correcting for problematic polygons
    world_shp_epi_color_for_centroids <<- sf::st_make_valid(world_shp_epi_color)
    country_wise_centroids <<- sf::st_centroid(world_shp_epi_color_for_centroids, of_largest_polygon = TRUE)

    # adding an identifier column for countries where no studies have happened
    country_wise_centroids$color_circle = ifelse(country_wise_centroids$num_studies == 0,  "No Studies", "Atleast 1 study")

    # creating a dataframe out of the latitude, longitude coordinates of the country centroids, so that we can plot it
    # using geom_point.
    coord_df <- dplyr::as_tibble(sf::st_coordinates(country_wise_centroids))

    # reassigning the X and Y coordinates to x_coord and y_coord
    country_wise_centroids$x_coord <- coord_df$X
    country_wise_centroids$y_coord <- coord_df$Y

    # moving the geometry column to the end and making sure that the entire dataset is a sf object
    country_wise_centroids <- country_wise_centroids %>%
      dplyr::select(-geometry, geometry) %>%
    sf::st_as_sf()

    # without geometry version of country wise centroids dataset and adding a label_plt column
    country_wise_centroids_sans_geom <- country_wise_centroids %>%
    sf::st_drop_geometry() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(label_plt = ifelse(num_studies == 0, "", num_studies))


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
        ggplot2::geom_sf(mapping = ggplot2::aes(fill = avg_pm2.5_2021, group = avg_pm2.5_2021), color = "darkgrey") +
        ggplot2::geom_sf(data = world_shp_epi_color %>% dplyr::filter(num_studies > 0) %>% sf::st_as_sf(), color = "black", size = 6, fill = "transparent") +
        colorspace::scale_fill_continuous_sequential(palette = "YlOrRd", name = expression("Annual Average" ~ PM[2.5] ~ "in 2021 (in µg/m³)")) +
        ggplot2::geom_jitter(data = country_wise_centroids_sans_geom, mapping = aes(x = x_coord, y = y_coord, size = num_studies, color = color_circle)) +
        ggplot2::geom_text(data = country_wise_centroids_sans_geom, mapping = aes(x = x_coord, y = y_coord, label = label_plt), size = 3, color = "white", size = 2) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_size(breaks = c(1, 5, 10, 20, 30), range = c(1, 12),
                            guide = ggplot2::guide_legend(override.aes = list(color = "darkgrey"))) +
        ggplot2::labs(caption = stringr::str_wrap("*The size of the circles is directly proportional to the total number of times a given country has been included
                                                in a study (any study, not necessarily PM2.5 specific - mentioned as the number in the circle). Countries with atleast 1 study are highlighted (black borders)."), width = 30,
                      size = "Number of Studies") +
        ggplot2::scale_color_manual(values = c("No Studies" = "cadetblue3", "Atleast 1 studys" = "darkolivegreen"), name = "") +
        ggthemes::theme_map() +
       ggplot2::ggtitle(expression("Country wise distribution of epi studies and Annual Average" ~PM[2.5] ~ "pollution (in µg/m³)")) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.position = "bottom",
                       legend.background = ggplot2::element_rect(color = "black"),
                       legend.justification = c(0.5, 3),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 16))
      return(geographic_dist_graph)

    } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)) {
      geographic_dist_graph <- world_shp_epi_color %>%
        dplyr::filter(country %in% input$countries_fig3) %>%
        ggplot2::ggplot() +
        ggplot2::geom_sf(mapping = ggplot2::aes(fill = avg_pm2.5_2021, group = avg_pm2.5_2021), color = "darkgrey") +
        ggplot2::geom_sf(data = world_shp_epi_color %>% dplyr::filter(num_studies > 0, country %in% input$countries_fig3) %>% sf::st_as_sf(), color = "black", size = 6, fill = "transparent") +
        colorspace::scale_fill_continuous_sequential(palette = "YlOrRd", name = expression("Annual Average" ~ PM[2.5] ~ "in 2021 (in µg/m³)")) +
        ggplot2::geom_jitter(data = country_wise_centroids_sans_geom %>% dplyr::filter(country %in% input$countries_fig3), mapping = aes(x = x_coord, y = y_coord, size = num_studies, color = color_circle)) +
        ggplot2::geom_text(data = country_wise_centroids_sans_geom %>% dplyr::filter(country %in% input$countries_fig3), mapping = aes(x = x_coord, y = y_coord, label = label_plt), size = 3) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_size(breaks = c(1, 5, 10, 20, 30), range = c(1, 12),
                            guide = ggplot2::guide_legend(override.aes = list(color = "darkgrey"))) +
        ggplot2::labs(caption = stringr::str_wrap("*The size of the circles is directly proportional to the total number of times a given country has been included
                                                in a study (any study, not necessarily PM2.5 specific - mentioned as the number in the circle). Countries with atleast 1 study are highlighted (black borders)."), width = 30,
                      size = "Number of Studies") +
        ggplot2::scale_color_manual(values = c("No Studies" = "cadetblue3", "Atleast 1 studys" = "darkolivegreen"), name = "") +
        ggthemes::theme_map() +
        ggplot2::ggtitle(expression("Country wise distribution of epi studies and Annual Average" ~PM[2.5] ~ "pollution (in µg/m³)")) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.position = "bottom",
                       legend.background = ggplot2::element_rect(color = "black"),
                       legend.justification = c(0.5, 3),
                       plot.title = ggplot2::element_text(hjust = 0.5, size = 16))

      return(geographic_dist_graph)
    }

  })

  #> Geographic Distribution of Studies table

  # output$geographic_dist_studies_table <- shiny::renderDataTable({
  #   if(length(input$countries_fig3) == 0){
  #     stop("Please select atleast one country to proceed.")
  #   } else if(("all" %in% input$countries_fig3) & (length(input$countries_fig3)) > 1){
  #     stop("Please either choose one/multiple countries, or 'all', but not both")
  #   }
  #
  #   if("all" %in% input$countries_fig3){
  #     dt_all <- country_wise_centroids %>%
  #       sf::st_drop_geometry() %>%
  #       dplyr::filter(num_studies != 0)
  #     DT::datatable(dt_all, options = list(pageLength = 6))
  #   } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)){
  #     dt_filter <- country_wise_centroids %>%
  #       sf::st_drop_geometry() %>%
  #       dplyr::filter(num_studies !=0, country %in% input$countries_fig3)
  #     DT::datatable(dt_filter, options = list(pageLength = 6))
  #   }
  # })

  #> Epi studies over time graph

  output$epi_studies_over_time_graph <- shiny::renderPlot({
    epi %>%
      dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
      dplyr::group_by(publishing_year) %>%
      dplyr::summarise(total_studies = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2], total_studies != "NA", !is.na(total_studies)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = publishing_year, y = total_studies), lwd = 1.2, color = "cornflowerblue") +
      ggplot2::labs(x = "Year", y = "Total Number of Studies",
                    caption = stringr::str_wrap("*This graph displays the total number of AQ epi studies (all studies, not PM₂.₅ specific) published over time."), width = 10) +
      ggthemes::theme_hc() +
      ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                     axis.line.x = ggplot2::element_line(color = "black"),
                     plot.caption = ggplot2::element_text(hjust = 0, size = 7)) %>%
      return()
  })

  #> Epi studies over time table

  # output$aq_epi_studies_over_time <- shiny::renderDataTable({
  #   dt_filter_epi_over_time <-   epi %>%
  #     dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
  #     dplyr::group_by(publishing_year) %>%
  #     dplyr::summarise(total_studies = dplyr::n()) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2], total_studies != "NA", !is.na(total_studies))
  #   DT::datatable(dt_filter_epi_over_time, options = list(pageLength = 6))
  #
  # })

  #> Continent Wise Distribution of Duration of Study graph



  output$continent_wise_dist_duration_study_graph <- shiny::renderPlot({

    # creating a copy of epi database, with China listed as a separate continent, so that we can plot the China panel and China - Asia panel


    # adding logic to check for missing continents and automatically adding missing panels for that
    if(sum(missing_continents_logical) == 0){
      continent_wise_study_duration_graph_summary_table <-   epi_tmp %>%
        dplyr::filter(continent != "NA", !is.na(continent), study_duration != "NA", !is.na(study_duration)) %>%
        dplyr::group_by(paper_uid, continent) %>%
        dplyr::summarise(average_study_duration = mean(study_duration, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(continent != "Oceania") %>%
        #dplyr::mutate(continent = as.factor(continent)) %>%
        dplyr::mutate(order_continent = ifelse(continent == "China", 2, 0),
                      order_continent = ifelse(continent == "Asia - China", 1, order_continent),
                      order_continent = ifelse(continent == "Europe", 5, order_continent),
                      order_continent = ifelse(continent == "North America", 6, order_continent),
                      order_continent = ifelse(continent == "South America", 4, order_continent),
                      order_continent = ifelse(continent == "Africa", 3, order_continent)) %>%
        dplyr::mutate(continent = ifelse(continent == "China", "China (1402.5 million people)", continent),
                      continent = ifelse(continent == "Africa", "Africa (1373.8 million people)", continent),
                      continent = ifelse(continent == "North America", "North America (589.2 million people)", continent),
                      continent = ifelse(continent == "South America", "South America (429.2 million people)", continent),
                      continent = ifelse(continent == "Europe", "Europe (740.8 million people)", continent),
                      continent = ifelse(continent == "Asia - China", "Asia - China (3161.5 million people)", continent))

    } else{
      for(i in 1:length(missing_continents)){
        if(i == 1){
          continent_wise_study_duration_graph_summary_table <- epi_tmp %>%
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
        dplyr::filter(continent != "Oceania") %>%
       # dplyr::mutate(continent = as.factor(continent)) %>%
        dplyr::mutate(order_continent = ifelse(continent == "China", 2, 0),
                      order_continent = ifelse(continent == "Asia - China", 1, order_continent),
                      order_continent = ifelse(continent == "Europe", 5, order_continent),
                      order_continent = ifelse(continent == "North America", 6, order_continent),
                      order_continent = ifelse(continent == "South America", 4, order_continent),
                      order_continent = ifelse(continent == "Africa", 3, order_continent)) %>%
        dplyr::mutate(continent = ifelse(continent == "China", "China (1402.5 million people)", continent),
                      continent = ifelse(continent == "Africa", "Africa (1373.8 million people)", continent),
                      continent = ifelse(continent == "North America", "North America (589.2 million people)", continent),
                      continent = ifelse(continent == "South America", "South America (429.2 million people)", continent),
                      continent = ifelse(continent == "Europe", "Europe (740.8 million people)", continent),
                      continent = ifelse(continent == "Asia - China", "Asia - China (3161.5 million people)", continent))
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
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = average_study_duration, fill = continent, group = continent), color = "white", position = "identity", binwidth = 2) +
        ggplot2::labs(x = "Study Duration", y = "Count",
                      caption = stringr::str_wrap("*This graph displays distribution by continent (and for China, Asia - China) for all studies (not PM2.5 specific)."), width = 10,
                      title = expression("Continent (and China) wise Study Duration Distribution and 2021 Annual Average" ~ PM[2.5] ~ "(in µg/m³)"),
                      subtitle = "(Ordered from most polluted to least polluted)") +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
        ggthemes::theme_hc() +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       plot.title = element_text(size = 14, hjust = 0.5),
                       plot.subtitle = element_text(size = 8, hjust = 0.5),
                       strip.background = element_rect(fill = "floralwhite"),
                       strip.text = element_text(size = 8),
                       legend.background = element_rect(color = "black")) +
        ggplot2::scale_fill_manual(values = c("North America (589.2 million people)" = "#FFBA00", "Europe (740.8 million people)" = "#FF9600",
                                     "South America (429.2 million people)" = "#FF6908", "Africa (1373.8 million people)" = "#E63D23", "China (1402.5 million people)" = "#BD251C",
                                     "Asia - China (3161.5 million people)" = "#8C130E")) +
        ggplot2::scale_y_continuous(breaks = seq(0, 10, 2))

    } else if (input$plot_type_fig5 == "Histogram (Density)"){
      continent_wise_study_duration_graph <- continent_wise_study_duration_graph_summary_table %>%
        dplyr::filter(continent %in% input$continent_list) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = average_study_duration, y = ..density.., fill = continent, group = continent), color = "white", binwidth = 2) +
        ggplot2::labs(x = "Study Duration", y = "Density",
                      caption = stringr::str_wrap("*This graph displays distribution by continent (and for China, Asia - China) for all studies (not PM2.5 specific)."), width = 10,
                      title = expression("Continent (and China) wise Study Duration Distribution and 2021 Annual Average" ~ PM[2.5] ~ "(in µg/m³)"),
                      subtitle = "(Ordered from most polluted to least polluted)") +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
        ggthemes::theme_hc() +
        ggplot2::theme(legend.title = element_blank()) +
        ggplot2::facet_wrap(~forcats::fct_reorder(continent, order_continent), nrow = 1) +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       plot.title = element_text(size = 14, hjust = 0.5),
                       plot.subtitle = element_text(size = 8, hjust = 0.5),
                       strip.background = element_rect(fill = "floralwhite"),
                       strip.text = element_text(size = 8),
                       legend.background = element_rect(color = "black")) +
        ggplot2::scale_fill_manual(values = c("North America (589.2 million people)" = "#FFBA00", "Europe (740.8 million people)" = "#FF9600",
                                              "South America (429.2 million people)" = "#FF6908", "Africa (1373.8 million people)" = "#E63D23", "China (1402.5 million people)" = "#BD251C",
                                              "Asia - China (3161.5 million people)" = "#8C130E"))
    }


    return(continent_wise_study_duration_graph)

  })

  #> Distribution of lower and upper limits of PM2.5 exposure range

  # Note: This is PM2.5 specific and different countries in a pooled study are counted separately.

  output$pm2.5_ll_ul_dist_graph <- shiny::renderPlot({
    # create long dataset
    epi_long <- epi %>%
      dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0) %>%
      tidyr::pivot_longer(cols = contains("pm2.5_exposure"), names_to =  "exposure_type", values_to = "exposure_value") %>%
      dplyr::select(paper_uid, exposure_type, exposure_value) %>%
      dplyr::filter(!is.na(exposure_value)) %>%
      dplyr::mutate(exposure_type = ifelse(exposure_type == "pm2.5_exposure_ll", "PM₂.₅ exposure in µg/m³ (Lower Limit)", "PM₂.₅ exposure in µg/m³ (Upper Limit)"))

    if(input$plot_type_fig6 == "Histogram"){
      epi_long %>%
        dplyr::group_by(paper_uid, exposure_type) %>%
        dplyr::summarise(exposure_value = ifelse(exposure_type == "pm2.5_exposure_ll", min(exposure_value, na.rm = TRUE), max(exposure_value, na.rm = TRUE))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type, group = exposure_type), position = "identity", alpha = 0.4, color = "white") +
        ggplot2::labs(x = "PM₂.₅ concentration (in µg/m³)",
                      y = "Count",
                      fill = "Exposure Type",
                      caption = stringr::str_wrap("*This graph takes into account only those PM₂.₅ studies that have atleast one of 'pm2.5 lower limit value', 'pm2.5 upper limit value'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.key = element_rect(color = "black"),
                       legend.text = element_text(size = 8)) +
        viridis::scale_fill_viridis(discrete = TRUE)

    } else if(input$plot_type_fig6 == "Density"){
      epi_long %>%
        dplyr::group_by(paper_uid, exposure_type) %>%
        dplyr::summarise(exposure_value = ifelse(exposure_type == "pm2.5_exposure_ll", min(exposure_value, na.rm = TRUE), max(exposure_value, na.rm = TRUE))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = exposure_value, fill = exposure_type, group = exposure_type), alpha = 0.6, position = "identity") +
        ggplot2::labs(x = "PM₂.₅ concentration (in µg/m³)",
                      y = "Density",
                      fill = "Exposure Type",
                      caption = stringr::str_wrap("*This graph takes into account only those PM₂.₅ studies that have atleast one of 'pm2.5 lower limit value', 'pm2.5 upper limit value'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.text = element_text(size = 8)) +
        viridis::scale_fill_viridis(discrete = TRUE)


    }



  })

  #> Country Wise Distribution of Cohort Size Graph

  # Different countries in a pooled study are counted separately, but this is not a PM2.5 specific graph.

  output$country_wise_dist_log_cohort_size_graph_1 <- plotly::renderPlotly({

    if(length(input$countries_list_fig7) == 0){
      stop("Please select atleast one country to proceed.")
    } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
      stop("Please either choose one/multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_list_fig7){
      epi %>%
        dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = cohort_size, fill = country, group = country), alpha = 0.5) +
        ggplot2::scale_x_log10() +
        ggthemes::theme_hc() +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM₂.₅ studies) in which a cohort/sample size is available."), width = 10,
                      x = "Cohort Size",
                      y = "Density",
                      fill = "Country") +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.background = element_rect(),
                       legend.title = element_text(hjust = 0.5)) +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()

    } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
      epi %>%
        dplyr::filter(!is.na(cohort_size), cohort_size != "NA") %>%
        dplyr::filter(country %in% input$countries_list_fig7) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = cohort_size, fill = country, group = country), alpha = 0.5) +
        ggplot2::scale_x_log10() +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM₂.₅ studies) in which a cohort/sample size is available."), width = 10,
                      x = "Cohort Size",
                      y = "Density",
                      fill = "Country") +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.background = element_rect(),
                       legend.title = element_text(hjust = 0.5)) +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()
    }


  })

  #> Distribution of lower limit and upper limit of ages covered in this analysis

  # This is not a PM2.5 specific graph and different countries in a cohort study are counted separately.

  output$age_ll_ul_graph <- shiny::renderPlot({

    epi_age_dist <- epi %>%
      dplyr::filter(((!is.na(cohort_age_ll)) | (cohort_age_ll != "NA")) & ((!is.na(cohort_age_ul)) | (cohort_age_ul != "NA"))) %>%
      tidyr::pivot_longer(cols = contains("cohort_age"), names_to =  "age_dist_type", values_to = "age_value") %>%
      dplyr::select(age_dist_type, age_value) %>%
      dplyr::filter(!is.na(age_value)) %>%
      dplyr::mutate(age_dist_type = ifelse(age_dist_type == "cohort_age_ll", "Cohort Age (Lower Limit)", "Cohort Age (Upper Limit)"))

    if(input$plot_type_fig6 == "Histogram"){
      epi_age_dist %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = age_value, fill = age_dist_type, group = age_dist_type), alpha = 0.5, position = "identity", color = "white") +
        ggplot2::scale_x_continuous(breaks = seq(0, 130, 20)) +
        ggplot2::scale_y_continuous(breaks = seq(0, 15, 2)) +
        ggplot2::labs(x = "Age",
                      fill = "Age Distribution Type",
                      caption = stringr::str_wrap("*This graph takes into account those studies  (can include non-PM₂.₅ studies) that have atleast one of 'age lower limit', 'age upper limit'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.text = element_text(size = 8)) +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()

    } else if(input$plot_type_fig6 == "Density"){
      epi_age_dist %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = age_value, fill = age_dist_type, group = age_dist_type), alpha = 0.6, color = "black") +
        ggplot2::scale_x_continuous(breaks = seq(0, 130, 20)) +
        ggplot2::labs(x = "Age",
                      caption = stringr::str_wrap("*This graph takes into account those studies  (can include non-PM₂.₅ studies) that have atleast one of 'age lower limit', 'age upper limit'."), width = 10) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.text = element_text(size = 8)) +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()
    }

  })

  #> Country wise distribution of study duration

  # This is not a PM2.5 specific graph and in this all countries in a given pooled study are counted separately.

  # Country wise distribution

  output$country_wise_dist_study_duration_1 <- plotly::renderPlotly({
    if(length(input$countries_list_fig7) == 0){
      stop("Please select atleast one country to proceed.")
    }

    if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
      stop("Please either choose one/multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_list_fig7){
      epi %>%
        dplyr::filter(!is.na(study_duration), study_duration != "NA") %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, group = country),  alpha = 0.5) +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
        ggthemes::theme_hc() +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM₂.₅ studies) in which study duration is available."), width = 10,
                      fill = "Country",
                      x = "Study Duration",
                      y = "Density") +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.background = element_rect(),
                       legend.title = element_text(hjust = 0.5)) +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()
    } else if ((("all" %in% input$countries_list_fig7) == FALSE) & (length(input$countries_list_fig7) >= 1)){
      epi %>%
        dplyr::filter(!is.na(study_duration), study_duration != "NA") %>%
        dplyr::filter(country %in% input$countries_list_fig7) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(mapping = ggplot2::aes(x = study_duration, fill = country, group = country), alpha = 0.5) +
        ggplot2::labs(caption = stringr::str_wrap("*This graph takes into account only those studies (can include non-PM₂.₅ studies) in which study duration is available."), width = 10,
                      x = "Study Duration",
                      y = "Density",
                      fill = "Country") +
        ggplot2::scale_x_continuous(breaks = seq(0, 40, 5)) +
        ggthemes::theme_hc() +
        ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
                       axis.line.x = ggplot2::element_line(color = "black"),
                       plot.caption = ggplot2::element_text(hjust = 0, size = 7),
                       legend.background = element_rect(),
                       legend.title = element_text(hjust = 0.5))  +
        viridis::scale_fill_viridis(discrete = TRUE) %>%
        return()
    }


  })

  #> pm2.5 and global population distribution graph
  # output$pm2.5_expo_glob_pop_graph <- shiny::renderPlot({
  #
  #   # percent population in pollution buckets
  #   tot_pop_in_bucket <-  aqli_color %>%
  #     dplyr::filter(pm2020 >= input$pm2.5_bucket[1], pm2020 <= input$pm2.5_bucket[2]) %>%
  #     dplyr::summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
  #     unlist()
  #
  #   world_pop <- aqli_color %>%
  #     dplyr::summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
  #     unlist()
  #
  #   percent_pop_in_bucket <- round((tot_pop_in_bucket/world_pop)*100, 1)
  #
  #   # percent studies in the given pollution bucket
  #   tot_studies_in_bucket <- epi %>%
  #     dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0) %>%
  #     dplyr::group_by(paper_uid) %>%
  #     dplyr::summarise(mean_pm2.5 = mean(mean_pm2.5, na.rm = TRUE)) %>%
  #     dplyr::filter(mean_pm2.5 >= input$pm2.5_bucket[1], mean_pm2.5 <= input$pm2.5_bucket[2]) %>%
  #     nrow()
  #
  #   tot_studies_overall <- nrow(epi %>%
  #                                 dplyr::distinct(paper_uid, .keep_all = TRUE) %>%
  #                                 dplyr::filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA", non_pm2.5 == 0))
  #
  #   percent_studies_in_bucket <- round((tot_studies_in_bucket/tot_studies_overall)*100, 1)
  #
  #   # creating a dataframe for plotting
  #   pm2.5_expo_glob_pop_df <- tibble::tibble(pm2.5_bucket = c(stringr::str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³"),
  #                                                             stringr::str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³")),
  #                                            prop_in_bucket = c(percent_pop_in_bucket, percent_studies_in_bucket),
  #                                            type_of_prop = c("Percent Population in PM2.5 bucket", "Percent Studies in PM2.5 bucket"))
  #
  #   pm2.5_expo_glob_pop_df$type_of_prop <- as.factor(pm2.5_expo_glob_pop_df$type_of_prop)
  #
  #    pm2.5_expo_glob_pop_df %>%
  #     ggplot2::ggplot() +
  #     ggplot2::geom_col(mapping = ggplot2::aes(x = pm2.5_bucket, y = prop_in_bucket, fill = type_of_prop, group = type_of_prop), position = position_dodge(), width = 0.4) +
  #     ggplot2::scale_fill_manual(values = c("Percent Population in PM2.5 bucket" = "grey", "Percent Studies in PM2.5 bucket" = "cornflowerblue"), labels = c("Proportion of World Population in Bucket", "Proportion of Studies Completed in Bucket")) +
  #     ggplot2::labs(x = expression("Mean" ~ PM[2.5] ~ "bucket (in µg/m³)"),  y = "Percentage", fill = "",
  #                   caption = stringr::str_wrap("*This graph 'only' takes into account the PM₂.₅ specific studies. For multi-country (pooled) studies, it averages the mean PM2.5 values, across all countries."), width = 17) +
  #     ggthemes::theme_hc() +
  #     ggplot2::theme(axis.line.y = ggplot2::element_line(color = "black"),
  #                    axis.line.x = ggplot2::element_line(color = "black"),
  #                    plot.caption = ggplot2::element_text(hjust = 0, size = 7),
  #                    legend.position = "bottom",
  #                    axis.title.x = element_text(size = 11),
  #                    axis.title.y = element_text(size = 11),
  #                    legend.text = element_text(size = 9))
  #    viridis::scale_fill_viridis(discrete = TRUE) %>%
  #      return()
  #
  # })


  #> Render the underlying data as the Data Table
  # output$underlying_data_table_3 <- shiny::renderTable({
  #   # epi
  #   })



}

shiny::shinyApp(ui, server)

