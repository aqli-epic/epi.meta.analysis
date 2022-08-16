## shiny app script for epi.meta.analysis--------------------------------------

# load app.R helper file
source("./R/app_helper.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "AQ Epi Studies Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "blog_post_graphs", text = "Blog Post Graphs", icon = icon("chart-bar")),
      menuItem(tabName = "other_content", text = "Other Content", icon = icon("pencil"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("blog_post_graphs",
       fluidRow(
         valueBoxOutput("num_papers", width = 3),
         valueBoxOutput("cohort_size_range",  width = 3),
         valueBoxOutput("study_publishing_year_range",  width = 3),
         valueBoxOutput("num_of_countries_covered",  width = 3)
       ),
       fluidRow(
         hr(),
         box(width = 2,
           sliderInput("pm2.5_bucket", "PM2.5 Range", 0, 120, value = c(0, 120))
           ),
         box(
           width = 4, status = "info", solidHeader = TRUE,
           title = "PM2.5 exposure Range and Global Population Distribution",
           plotlyOutput("pm2.5_expo_glob_pop_graph")
         ),
         box(
           width = 2,
           selectInput("countries_fig2", "Countries", choices = epi %>% filter(!is.na(country)) %>% select(country) %>% unlist() %>% unique(), multiple = TRUE, selected = "USA")
         ),
         box(
           width = 4, status = "info", solidHeader = TRUE,
           title = "Country Wise Mean PM2.5 Distribution",
           plotlyOutput("mean_pm2.5_dist_country_wise_graph")
         )
       ),
       hr(),
       fluidRow(
         box(
           width = 5,
           selectInput("countries_fig3", "Countries", choices = c("all", "United States", epi %>% filter(!is.na(country)) %>% select(country) %>% filter(country != "USA") %>% unlist() %>% unique()), multiple = TRUE, selected = "all"),
           dataTableOutput("geographic_dist_studies_table")
         ),
         box(
           status = "info", solidHeader = TRUE,
           title = "Geographic Distribution of Studies",
           width = 7,
           plotlyOutput("geographic_dist_studies")
         )
       ),
       hr(),
       fluidRow(
         box(
           width = 4,
           sliderInput("year_range", "Year Range", min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE), value = c(min(epi$publishing_year, na.rm = TRUE), max(epi$publishing_year, na.rm = TRUE))),
           dataTableOutput("aq_epi_studies_over_time")
         ),
         box(
           width = 8, status = "info", solidHeader = TRUE,
           title = "AQ Epi Studies Over Time",
           plotlyOutput("epi_studies_over_time_graph")
         )
       ),
       hr(),
       fluidRow(
         box(
           width = 3,
           selectInput("continent_list", "Continents", choices = c(epi %>% filter(!is.na(continent)) %>% select(continent) %>% unlist() %>% unique(), "Africa"), multiple = TRUE, selected = "North America"),
           hr(),
           selectInput("plot_type_fig5", "Plot Type", choices = c("Histogram (Frequency)", "Histogram (Density)"), selected = "Histogram (Frequency)")
         ),
         box(status = "info", solidHeader = TRUE,
           title = "Epi Studies Duration Distribution (Continent Wise): Histogram",
           width = 9,
           plotlyOutput("continent_wise_dist_duration_study_graph")
         )
       ),
       hr(),
       hr(),
       fluidRow(
         box(width = 12,
             status = "info", solidHeader = TRUE,
             title = "Other Graphs")
       ),
       hr(),
       fluidRow(
         box(
           width = 2,
           status = "info",
           solidHeader = TRUE,
           selectInput("plot_type_fig6", "Plot Type", choices = c("Histogram", "Density"), selected = "Histogram")
         ),

         box(title = "Lower and Upper Limit Distributions of PM2.5 exposure range",
           width = 10,
             status = "info",
             solidHeader = TRUE,
             plotlyOutput("pm2.5_ll_ul_dist_graph"))
       ),
       hr(),
       fluidRow(
         box(
           width = 2,
           status = "info",
           solidHeader = TRUE,
           selectInput("countries_list_fig7", "Countries", choices = c("all", epi %>% filter(!is.na(country)) %>% select(country) %>% unlist() %>% unique()), multiple = TRUE, selected = "all")
         ),
         box(title = "Country Wise Distribution of Log Cohort Size",
             width = 10,
             status = "info",
             solidHeader = TRUE,
             plotlyOutput("country_wise_dist_log_cohort_size_graph"))
       )

      ),
      tabItem("other_content")

      )
    )
  )


# server------------------------------------------------------------------------
server <- function(input, output) {

  #> Summary Stats

  # number of papers value box
  output$num_papers <- renderValueBox({
   # num_papers
    num_papers <- as.numeric(nrow(epi))

    valueBox(
      value = formatC(num_papers),
      subtitle = "Number of Papers Analyzed",
      icon = icon("file-circle-check")
    )
  })

  # cohort size range value box
  output$cohort_size_range <- renderValueBox({
    # cohort size range
    cohort_size_ll <- min(epi$cohort_size, na.rm = TRUE)
    cohort_size_ul_millions <- round(max(epi$cohort_size, na.rm = TRUE)/1000000, 1)
    final_cohort_range_string <- stringr::str_c(cohort_size_ll, "-", cohort_size_ul_millions, "million", sep = " ")

    valueBox(
      value = formatC(final_cohort_range_string),
      subtitle = "Cohort Size Range",
      icon = icon("people-line")
    )
  })

  # study publishing value box
  output$study_publishing_year_range <- renderValueBox({
    # study duration range
    study_publishing_min <- min(epi$publishing_year, na.rm = TRUE)
    study_publishing_max <-  max(epi$publishing_year, na.rm = TRUE)
    final_study_duration_string <- stringr::str_c(study_publishing_min, "-", study_publishing_max, sep = " ")

    valueBox(
      value = formatC(final_study_duration_string),
      subtitle = "Study Publishing Year Range",
      icon = icon("calendar-days")
    )



  })

  # Number of countries covered value box

  output$num_of_countries_covered <- renderValueBox({

    num_countries_covered <- epi %>% filter(!is.na(country)) %>% select(country) %>% unlist() %>% unique() %>% length()

    valueBox(
      value = formatC(num_countries_covered),
      subtitle = "Number of Countries Covered",
      icon = icon("earth-americas")
    )

  })

  #> pm2.5 and global population distribution graph
  output$pm2.5_expo_glob_pop_graph <- renderPlotly({



  # percent population in pollution buckets
  tot_pop_in_bucket <-  aqli_color %>%
      filter(pm2020 >= input$pm2.5_bucket[1], pm2020 <= input$pm2.5_bucket[2]) %>%
      summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
    unlist()

  world_pop <- aqli_color %>%
    summarise(tot_pop = sum(population, na.rm = TRUE)) %>%
    unlist()

  percent_pop_in_bucket <- round((tot_pop_in_bucket/world_pop)*100, 1)

  # percent studies in the given pollution bucket
  tot_studies_in_bucket <- epi %>%
    filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA") %>%
    filter(mean_pm2.5 >= input$pm2.5_bucket[1], mean_pm2.5 <= input$pm2.5_bucket[2]) %>%
    nrow()

  tot_studies_overall <- nrow(epi %>%
                                filter(!is.na(mean_pm2.5), mean_pm2.5 != "NA"))

  percent_studies_in_bucket <- round((tot_studies_in_bucket/tot_studies_overall)*100, 1)

  # creating a dataframe for plotting
  pm2.5_expo_glob_pop_df <- tibble(pm2.5_bucket = c(str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³"),
                                                       str_c(input$pm2.5_bucket[1], "-", input$pm2.5_bucket[2], "µg/m³")),
                                   prop_in_bucket = c(percent_pop_in_bucket, percent_studies_in_bucket),
                                   type_of_prop = c("Percent Population in PM2.5 bucket", "Percent Studies in PM2.5 bucket"))

  pm2.5_expo_glob_pop_df$type_of_prop <- as.factor(pm2.5_expo_glob_pop_df$type_of_prop)

  plt <- pm2.5_expo_glob_pop_df %>%
    ggplot() +
    geom_col(mapping = aes(x = pm2.5_bucket, y = prop_in_bucket, fill = type_of_prop), position = position_dodge(), width = 0.4) +
    scale_fill_manual(values = c("Percent Population in PM2.5 bucket" = "grey", "Percent Studies in PM2.5 bucket" = "cornflowerblue"), labels = c("Proportion of World Population in Bucket", "Proportion of Studies Completed in Bucket")) +
    labs(x = "Mean PM2.5 bucket (in µg/m³)",  y = "Percentage", fill = "") +
    theme_hc() +
    theme(axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black"))


  return(plotly::ggplotly(plt))

  })

#> Country wise PM2.5 distribution Graph


  output$mean_pm2.5_dist_country_wise_graph <- renderPlotly({

    # sanity checks
    if(length(input$countries_fig2) == 0){
      stop("Please select atleast one country to proceed.")
    }

    mean_pm2.5_country_wise_graph <- epi %>%
      filter(!is.na(mean_pm2.5) | mean_pm2.5 != "NA", country %in% input$countries_fig2) %>%
      ggplot() +
      geom_density(mapping = aes(x = mean_pm2.5, fill = country), alpha = 0.5, color = "black", position = "identity") +
      labs(x = "Mean PM2.5 concentration (in µg/m³)") +
      theme_hc() +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

    return(ggplotly(mean_pm2.5_country_wise_graph))
  })

#> Geographic Distribution of Studies Graph

  output$geographic_dist_studies <- renderPlotly({
    # generate summary dataset for total number of studies conducted in a given country
    epi_country_count <- epi %>%
      select(country) %>%
      count(country) %>%
      filter(country != "NA")

    # making country names match before joining epi file with color file
    epi_country_count$country <- str_replace(epi_country_count$country, "USA", "United States")

    #> plot choropleth map

    # get data ready for the choropleth map
    world <- ne_countries(scale = "medium", returnclass = "sf")


    # making country names match before joining epi file with color file
    world$name_long <- str_replace(world$name_long, "Russian Federation", "Russia")
    world$name_long <- str_replace(world$name_long, "Lao PDR", "Laos")
    world$name_long <- str_replace(world$name_long, "Dem. Rep. Korea", "North Korea")
    world$name_long <- str_replace(world$name_long, "Republic of Korea", "South Korea")
    world$name_long <- str_replace(world$name_long, "Vatican", "Vatican City")
    world$name_long <- str_replace(world$name_long, "Brunei Darussalam", "Brunei")
    world$name_long <- str_replace(world$name_long, "Somaliland", "Somalia")

    # joining world shape file with the epi conutry wise count dataset
    world_shp_epi <- world %>%
      left_join(epi_country_count, by = c("name_long" = "country")) %>%
      select(name_long, n, geometry) %>%
      rename(num_studies = n, country = name_long)

    # color 2020 collapse to country level to get the PM2.5 pollution layer (first layer of the map)
    aqli_color_country <- aqli_color %>%
      group_by(country) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm2020_pop_weighted = pm2020*pop_weights) %>%
      summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE))

    # joining the world shape + epi joined file with the aqli color file
    world_shp_epi_color <- world_shp_epi %>%
      left_join(aqli_color_country, by = "country") %>%
      filter(country != "Antarctica") %>%
      select(country, num_studies, avg_pm2.5_2020, geometry)

    # In the total number of studies column, replacing NAs with 0s
    world_shp_epi_color <- world_shp_epi_color %>%
      mutate(num_studies = ifelse(is.na(num_studies) == TRUE, 0, num_studies))

    # getting centroids for countries after correcting for problematic polygons
    world_shp_epi_color_for_centroids <- st_make_valid(world_shp_epi_color)
    country_wise_centroids <- st_centroid(world_shp_epi_color_for_centroids, of_largest_polygon = TRUE)

    # plotting the choropleth

    if(length(input$countries_fig3) == 0){
      stop("Please select atleast one country to proceed.")
    } else if(("all" %in% input$countries_fig3) & (length(input$countries_fig3)) > 1){
      stop("Please choose either multiple countries, or 'all', but not both")
    }

    if("all" %in% input$countries_fig3){
      options(repr.plot.width = 50, repr.plot.height = 50)
        geographic_dist_graph <- world_shp_epi_color %>%
          ggplot() +
          geom_sf(mapping = aes(fill = avg_pm2.5_2020), color = "white") +
          scale_fill_continuous_sequential(palette = "YlOrRd") +
          geom_sf(data = country_wise_centroids %>% filter(num_studies != 0), mapping = aes(size = num_studies), col = "grey", ) +
          theme(legend.position = "bottom") +
          scale_size_binned() +
          scale_size(range = c(0, 8)) +
          theme_map()
        return(ggplotly(geographic_dist_graph))

      } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)) {
        geographic_dist_graph <- world_shp_epi_color %>%
          filter(country %in% input$countries_fig3) %>%
          ggplot() +
          geom_sf(mapping = aes(fill = avg_pm2.5_2020), color = "white") +
          scale_fill_continuous_sequential(palette = "YlOrRd") +
          geom_sf(data = country_wise_centroids %>% filter(num_studies != 0, country %in% input$countries_fig3), mapping = aes(size = num_studies), col = "grey") +
          theme(legend.position = "bottom") +
          scale_size_binned() +
          scale_size(range = c(0, 8)) +
          theme_map()
        return(ggplotly(geographic_dist_graph))
      }

    })

#> Geographic Distribution of Studies table

output$geographic_dist_studies_table <- renderDataTable({
  if(length(input$countries_fig3) == 0){
    stop("Please select atleast one country to proceed.")
  } else if(("all" %in% input$countries_fig3) & (length(input$countries_fig3)) > 1){
    stop("Please choose either multiple countries, or 'all', but not both")
  }

  if("all" %in% input$countries_fig3){
   dt_all <- country_wise_centroids %>%
      st_drop_geometry() %>%
      filter(num_studies != 0)
   DT::datatable(dt_all, options = list(pageLength = 6))
  } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)){
   dt_filter <- country_wise_centroids %>%
      st_drop_geometry() %>%
      filter(num_studies !=0, country %in% input$countries_fig3)
   DT::datatable(dt_filter, options = list(pageLength = 6))
  }
})

#> Epi studies over time graph

output$epi_studies_over_time_graph <- renderPlotly({
  epi %>%
    group_by(publishing_year) %>%
    summarise(total_studies = n()) %>%
    filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2]) %>%
    ggplot() +
    geom_line(mapping = aes(x = publishing_year, y = total_studies), lwd = 1.2, color = "cornflowerblue") +
    labs(x = "Year", y = "Total Number of Studies") +
    theme_hc() +
    theme(axis.line.y = element_line(color = "black"),
          axis.line.x = element_line(color = "black")) %>%
    return()
})

#> Epi studies over time table

output$aq_epi_studies_over_time <- renderDataTable({
  dt_filter_epi_over_time <-   epi %>%
    group_by(publishing_year) %>%
    summarise(total_studies = n()) %>%
    filter(publishing_year >= input$year_range[1], publishing_year <= input$year_range[2])
  DT::datatable(dt_filter_epi_over_time, options = list(pageLength = 6))

})

#> Continent Wise Distribution of Duration of Study graph

output$continent_wise_dist_duration_study_graph <- renderPlotly({
  epi_with_africa <- epi %>%
    filter(continent != "NA") %>%
    add_row(continent = "Africa")

  # adding an order panel in the epi aftica dataset
  epi_with_africa <- epi_with_africa %>%
    mutate(order_continent = ifelse(continent == "Asia", 1, 0),
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
      filter(continent %in% input$continent_list) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = study_duration, fill = continent), alpha = 0.5, color = "white", position = "identity", binwidth = 2) +
      labs(x = "Study Duration") +
      scale_x_continuous(breaks = seq(0, 40, 5)) +
      theme_hc() +
      theme(legend.title = element_blank()) +
      facet_wrap(~fct_reorder(continent, order_continent), nrow = 1) +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  } else if (input$plot_type_fig5 == "Histogram (Density)"){
    continent_wise_study_duration_graph <- epi_with_africa %>%
      filter(continent %in% input$continent_list) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = study_duration, y = ..density.., fill = continent), alpha = 0.5, color = "white", binwidth = 2) +
      geom_histogram(mapping = aes(x = study_duration, y = ..density.., fill = continent), alpha = 0.5, color = "white", binwidth = 2) +
      labs(x = "Study Duration") +
      scale_x_continuous(breaks = seq(0, 40, 5)) +
      theme_hc() +
      theme(legend.title = element_blank()) +
      facet_wrap(~fct_reorder(continent, order_continent), nrow = 1) +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  }


   ggplotly(continent_wise_study_duration_graph)

})

#> Distribution of lower and upper limits of PM2.5 exposure range

output$pm2.5_ll_ul_dist_graph <- renderPlotly({
  # create long dataset
  epi_long <- epi %>%
    pivot_longer(cols = contains("pm2.5_exposure"), names_to =  "exposure_type", values_to = "exposure_value") %>%
    select(exposure_type, exposure_value) %>%
    filter(!is.na(exposure_value))

  if(input$plot_type_fig6 == "Histogram"){
    epi_long %>%
      ggplot() +
      geom_histogram(mapping = aes(x = exposure_value, fill = exposure_type), position = "identity", alpha = 0.4, color = "white") +
      scale_x_continuous(breaks = seq(0, 250, 10)) +
      labs(x = "PM2.5 concentration (in µg/m³)") +
      theme_hc() +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))

  } else if(input$plot_type_fig6 == "Density"){
    epi_long %>%
      ggplot() +
      geom_density(mapping = aes(x = exposure_value, fill = exposure_type), alpha = 0.6, position = "identity") +
      scale_x_continuous(breaks = seq(0, 250, 10)) +
      labs(x = "PM2.5 concentration (in µg/m³)") +
      theme_hc() +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"))


  }



})

#> Country Wise Distribution of Log Cohort Size Graph

output$country_wise_dist_log_cohort_size_graph <- renderPlotly({

  if(length(input$countries_list_fig7) == 0){
    stop("Please select atleast one country to proceed.")
  } else if(("all" %in% input$countries_list_fig7) & (length(input$countries_list_fig7)) > 1){
    stop("Please choose either multiple countries, or 'all', but not both")
  }

  if("all" %in% input$countries_list_fig7){
    epi %>%
      ggplot() +
      geom_density(mapping = aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
      theme_hc() +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()

  } else if ((("all" %in% input$countries_fig3) == FALSE) & (length(input$countries_fig3) >= 1)){
    epi %>%
      filter(country %in% input$countries_list_fig7) %>%
      ggplot() +
      geom_density(mapping = aes(x = log10(cohort_size), fill = country), alpha = 0.5) +
      theme_hc() +
      theme(axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black")) %>%
      return()
  }




})

}

shinyApp(ui, server)

