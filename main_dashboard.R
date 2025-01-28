library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(tidyr)
library(lubridate)
library(sf)
library(leaflet.extras)
library(geojsonio)
library(viridis)
library(scales)

data <- read_excel("armenia_data_cleaned.xlsx", 
                   col_types = c("numeric", "numeric", "numeric", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", "text", 
                                 "text", "text", "numeric", "text", "text", 
                                 "text", "text", "numeric", "numeric", "numeric", 
                                 "text", "text", "text", "numeric", "text", "numeric"))

data$event_date <- as.Date(data$event_date, origin = "1899-12-30")
data$month <- format(data$event_date, "%B")
data$year <- year(data$event_date)

regions_geojson <- geojsonio::geojson_read("am.json", what = "sp")
regions_sf <- st_as_sf(regions_geojson)

protest_categories <- tribble(
  ~sub_event_type,                  ~category,
  "Peaceful protest",               "Peaceful",
  "Violent demonstration",          "Non Peaceful",
  "Protest with intervention",      "Non Peaceful",
  "Excessive force against protesters", "Non Peaceful",
  "Mob violence",                   "Non Peaceful"
)

actor_name_mapping <- c(
  "BHK: Prosperous Armenia Party" = "BHK",
  "Students (Armenia); ARFD: Armenian Revolutionary Federation" = "Students ARFD",
  "ARFD: Armenian Revolutionary Federation" = "ARFD",
  "ARFD: Armenian Revolutionary Federation; BHK: Prosperous Armenia Party; HHK: Republican Party of Armenia; Homeland Party" = "ARFD, BHK, HHK, Homeland Party",
  "ARFD: Armenian Revolutionary Federation; BHK: Prosperous Armenia Party; Former Government of Armenia (2018-); Government of Armenia (2018-); HHK: Republican Party of Armenia; Homeland Party" = "ARFD, BHK,  Formers, HHK",
  "Former Government of Armenia (2018-); Government of Armenia (2018-); HHK: Republican Party of Armenia; Homeland Party" = "ARFD, BHK, Former Gov, HHK",
  "Refugees/IDPs (Azerbaijan); Armenian Group (Azerbaijan)" = "Refugees/IDPs",
  "ARFD: Armenian Revolutionary Federation; HHK: Republican Party of Armenia" = "ARFD, HHK",
  "Refugees/IDPs (Azerbaijan); Students (Armenia); Armenian Group (Azerbaijan); Protesters (Azerbaijan)" = "Refugees/IDPs, Students (Armenia)",
  "Armenian Apostolic Church; Tavush for the Motherland" = "Armenian Apostolic Church"
)



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; padding: 5px; margin-top: -5px;",
      "Political Events in Armenia"
    ),
    titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Data Summary", tabName = "data_summary", icon = icon("chart-pie")),
      menuItem("Event Types", tabName = "event_types", icon = icon("archive")),
      menuItem("Regional Analysis", tabName = "regional_analysis", icon = icon("map-marker-alt")),
      menuItem("Regional Heatmap", tabName = "regional_heatmap", icon = icon("map-marked-alt")),
      menuItem("Protest Events Overview", tabName = "protest_events", icon = icon("chart-bar")),
      menuItem("Protestor Groups", tabName = "protestor_groups", icon = icon("flag")),
      menuItem("Protest Crowd", tabName = "protest_crowd", icon = icon("users"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-green .main-header .logo {
          background-color: #2c8e3d;
        }
        .skin-green .main-header .navbar {
          background-color: #2ca02c;
        }
        .box {
          background-color: #e6f2e6;
          border-top-color: #2c8e3d;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "intro",
              box(width = 12,
                  title = "Research Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  HTML("
                 <div style='padding: 20px;'>
                    <h3 style='color: #2c8e3d; margin-bottom: 20px;'>Political Events Analysis in Armenia</h3>
                    
                    <div style='margin-bottom: 25px;'>
                        <h4 style='color: #444; margin-bottom: 15px;'>About Political Events</h4>
                        <p style='line-height: 1.6; text-align: justify;'>
                            Political events encompass a wide range of activities that shape a country's social and political landscape, 
                            from peaceful demonstrations to instances of civil unrest. These events serve as important indicators of 
                            societal change, public sentiment, and political dynamics within a region. In Armenia, these events have 
                            played a crucial role in the country's recent history.
                        </p>
                    </div>
                    
                    <div style='margin-bottom: 25px;'>
                        <h4 style='color: #444; margin-bottom: 15px;'>Study Context</h4>
                        <p style='line-height: 1.6; text-align: justify;'>
                            The time frame includes various significant events, such as the Velvet revolution. 
                            By using a data visualization approach, this study compares statistical patterns, 
                            graphical representations, and temporal changes to examine the political events in Armenia.
                        </p>
                    </div>
                    
                    <div style='margin-bottom: 25px;'>
                        <h4 style='color: #444; margin-bottom: 15px;'>Research Objectives</h4>
                        <ul style='line-height: 1.6;'>
                            <li>Analyze spatial and temporal patterns of political events</li>
                            <li>Examine regional variations in violence</li>
                            <li>Study event type distributions and their implications</li>
                            <li>Understand the underlying logic of political events considering timeframe and geopolitics</li>
                        </ul>
                    </div>
                    
                    <div class='methodology'>
                        <h4 style='color: #444; margin-bottom: 15px;'>Methodology</h4>
                        <p style='line-height: 1.6; text-align: justify;'>
                            By presenting data in a visual format, the research shows the power of visuals in uncovering 
                            trends and correlations that might be challenging to understand in traditional text-based analyses.
                        </p>
                    </div>
                 </div>
            ")
              )
      ),
      
      # Event Types Tab
      tabItem(tabName = "event_types",
              fluidRow(
                infoBoxOutput("protest_info"),
                infoBoxOutput("battle_info"),
                infoBoxOutput("explosions_info"),
                infoBoxOutput("riots_info"),
                infoBoxOutput("strategic_info"),
                infoBoxOutput("violence_info"),
                
                box(width = 12,
                    title = "Events Over Years",
                    plotlyOutput("events_over_years")
                ),
                box(width = 12,
                    title = "Event Types Over Time",
                    plotlyOutput("event_type_trend")
                )
              )
      ),
      
      tabItem(tabName = "protest_events",
              fluidRow(
                infoBoxOutput("peaceful_protest_box", width = 4),
                infoBoxOutput("violent_demo_box", width = 4),
                infoBoxOutput("protest_intervention_box", width = 4),
                infoBoxOutput("excessive_force_box", width = 6),
                infoBoxOutput("mob_violence_box", width = 6)
              ),
              fluidRow(
                box(title = "Distribution of Peaceful-Non Peaceful Events by Year", width = 12, 
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("distribution_plot")),
                box(title = "Proportional Categorized Protests by Year", width = 12, 
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("proportional_plot"))
              )
      ),
      
      tabItem(tabName = "protestor_groups",
              fluidRow(
                box(width = 12,
                    title = "Unique Protestors by Year",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("unique_actors_plot", height = "400px")
                ),
                box(width = 12,
                    title = "Top Protest Groups by Year",
                    status = "primary",
                    solidHeader = TRUE,
                    column(width = 12,
                           sliderInput("protest_year",
                                       "Select Year:",
                                       min = min(data$year, na.rm = TRUE),
                                       max = max(data$year, na.rm = TRUE),
                                       value = min(data$year, na.rm = TRUE),
                                       step = 1,
                                       sep = "")
                    ),
                    column(width = 12,
                           plotlyOutput("protestor_groups_plot", height = "600px")
                    )
                ),
                box(width = 12,
                    title = "Protest Group Activity Timeline",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("protest_groups_timeline", height = "400px")
                )
              )
      ),
      
      tabItem(tabName = "protest_crowd",
              fluidRow(
                valueBoxOutput("total_protesters_box", width = 4),
                valueBoxOutput("avg_crowd_size_box", width = 4),
                valueBoxOutput("max_crowd_size_box", width = 4)
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Protest Crowd Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  column(width = 12,
                         sliderInput("crowd_year",
                                     "Select Year:",
                                     min = min(data$year, na.rm = TRUE),
                                     max = max(data$year, na.rm = TRUE),
                                     value = min(data$year, na.rm = TRUE),
                                     step = 1,
                                     sep = "")
                  ),
                  column(width = 12,
                         leafletOutput("crowd_size_map", height = "600px")
                  )
                ),
                box(
                  width = 12,
                  title = "Crowd Size Details",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("crowd_size_trend", height = "600px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Notes",
                  status = "primary",
                  solidHeader = TRUE,
                  HTML("
                 <p><strong>About the Crowd Size Calculations:</strong></p>
                 <ul>
                   <li>Several thousand ≈ 2,000 people</li>
                   <li>Few thousand ≈ 1,500 people</li>
                   <li>Thousand ≈ 1,000 people</li>
                   <li>Hundreds ≈ 300 people</li>
                   <li>Several ≈ 5 people</li>
                   <li>Few ≈ 3 people</li>
                   <li>Tens ≈ 10 people</li>
                 </ul>
                 <p>For ranges (e.g., 'between 100-200'), the average value is used.</p>
                 ")
                )
              )
      ),
      
      
      tabItem(tabName = "data_summary",
              fluidRow(
                # Data Overview Boxes
                infoBoxOutput("total_events_box"),
                infoBoxOutput("unique_actors_box"),
                infoBoxOutput("fatalities_box"),
                
                # Event Distribution Visuals
                box(width = 6,
                    title = "Event Distribution by Event Type",
                    plotlyOutput("data_summary_event_type_pie")
                ),
                box(width = 6,
                    title = "Fatalities by Event Type",
                    plotlyOutput("fatalities_by_event_type")
                ),

                box(width = 6,
                    title = "Top Sources",
                    plotlyOutput("sources_bar_chart")
                ),
                box(width = 6,
                    title = "Source Categories",
                    plotlyOutput("sources_category_pie")
                ),
                box(width = 12,
                    title = "Top Actors Involved",
                    plotlyOutput("top_actors_plot")
                ),

              )
      ),
      tabItem(tabName = "regional_analysis",
              fluidRow(
                box(width = 12,
                    title = "Events by Region",
                    plotlyOutput("region_event_count")
                ),
                box(width = 12,
                    title = "Event Type Frequency by Region",
                    plotlyOutput("region_event_type_heatmap")
                ),
                box(width = 12,
                    title = "Geospatial Correlation of Political Events",
                    leafletOutput("heatmap", height = "600px"),
                    sliderInput("year", "Select Year:",
                                min = min(data$year, na.rm = TRUE),
                                max = max(data$year, na.rm = TRUE),
                                value = min(data$year, na.rm = TRUE),
                                step = 1, sep = "")
                )
              )
      ),

      
      
      tabItem(tabName = "regional_heatmap",
              fluidRow(
                box(width = 4, 
                    title = "Event Type Selection",
                    selectInput("event_types", 
                                "Select Event Type:", 
                                choices = unique(data$event_type),
                                selected = unique(data$event_type)[1])
                ),
                box(width = 8,
                    title = "Event Density by Armenian Regions",
                    leafletOutput("armenia_heatmap", height = "600px"),
                    sliderInput("selected_year", "Select Year:",
                                min = min(data$year, na.rm = TRUE),
                                max = max(data$year, na.rm = TRUE),
                                value = min(data$year, na.rm = TRUE),
                                step = 1)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  output$event_type_pie <- renderPlotly({
    event_type_counts <- data %>%
      group_by(event_type) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    plot_ly(event_type_counts, labels = ~event_type, values = ~count, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = rainbow(nrow(event_type_counts)))) %>%
      layout(title = "")
  })
  
  output$event_type_trend <- renderPlotly({
    event_type_trend <- data %>%
      group_by(year, event_type) %>%
      summarise(count = n(), .groups = 'drop')
    
    plot_ly(event_type_trend, x = ~year, y = ~count, color = ~event_type, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Events"))
  })
  
  output$region_event_count <- renderPlotly({
    
    armenian_regions <- c("Shirak", "Lori", "Tavush", "Aragatsotn", "Kotayk", 
                          "Gegharkunik", "Yerevan", "Armavir", "Vayots Dzor", 
                          "Syunik", "Ararat")
    
    region_counts <- data %>%
      filter(region %in% armenian_regions) %>%
      group_by(region) %>%
      summarise(event_count = n(), .groups = 'drop') %>%
      arrange(desc(event_count))
    
    region_counts$region <- factor(region_counts$region, levels = region_counts$region)
    
    plot_ly(region_counts, 
            x = ~region, 
            y = ~event_count, 
            type = 'bar',
            marker = list(color = ~event_count, colorscale = 'Viridis')) %>%
      layout(
        title = "",
        xaxis = list(title = "Region"),
        yaxis = list(title = "Number of Events")
      )
  })
  
  
  
output$region_event_type_heatmap <- renderPlotly({
  
  armenian_regions <- c("Shirak", "Lori", "Tavush", "Aragatsotn", "Kotayk", 
                        "Gegharkunik", "Yerevan", "Armavir", "Vayots Dzor", 
                        "Syunik", "Ararat")
  
  region_event_type <- data %>%
    filter(region %in% armenian_regions) %>%
    group_by(region, event_type) %>%
    summarise(event_count = n(), .groups = 'drop') %>%
    tidyr::complete(region = armenian_regions, event_type, fill = list(event_count = 0))
  
  custom_colorscale <- list(
    list(0.000, "#FFFF00"),
    list(0.007, "#FFE600"), 
    list(0.013, "#FFCC00"), 
    list(0.020, "#FFB300"), 
    list(0.027, "#FF9900"), 
    list(0.033, "#FF8000"), 
    list(0.040, "#FF6600"), 
    list(0.047, "#FF4D00"),
    list(0.053, "#FF3300"), 
    list(0.060, "#FF1A00"), 
    list(0.067, "#FF0000"), 
    list(0.100, "#E60000"), 
    list(0.133, "#CC0000"),
    list(0.167, "#B30000"), 
    list(0.200, "#990000"),
    list(0.267, "#800000"), 
    list(0.333, "#660000"),
    list(0.500, "#4D0000"), 
    list(0.667, "#330000"), 
    list(1.000, "#1A0000")  
  )
  
  
  plot_ly(region_event_type, 
          x = ~region, 
          y = ~event_type, 
          z = ~event_count, 
          type = "heatmap",
          colorscale = custom_colorscale,
          zmin = 0,
          zmax = 1500) %>%
    layout(
      title = "",
      xaxis = list(title = "Region"),
      yaxis = list(title = "Event Type")
    )
})

  
  output$heatmap <- renderLeaflet({
    filteredData <- data %>%
      filter(year == input$year)
    
    leaflet(filteredData) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(43, 38, 47, 41) %>% 
      addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~1, 
        blur = 10,   
        max = 0.1,  
        radius = 15  
      ) %>%
      addLegend(
        "bottomright",
        colors = c("blue", "cyan", "lime", "yellow", "red"),
        labels = c("Low", "", "", "", "High"),
        title = ""
      )
  })
  
  output$events_over_years <- renderPlotly({
    event_counts_year <- data %>%
      group_by(year) %>%
      summarise(events = n())
    
    plot_ly(event_counts_year, x = ~year, y = ~events, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'darkred')) %>%
      layout(title = "",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Events"))
  })

  
  filtered_data <- reactive({
    req(input$selected_year)
    req(input$event_types)
    
    filtered <- data %>%
      filter(
        year == input$selected_year,
        event_type == input$event_types
      ) %>%
      group_by(region) %>%
      summarise(event_count = n(), .groups = 'drop')
    
    return(filtered)
  })
  
  output$armenia_heatmap <- renderLeaflet({
    region_counts <- filtered_data()
    
    if(nrow(region_counts) == 0) {
      return(leaflet() %>% addTiles() %>% addMarkers(lng = 44, lat = 40, popup = "No data available"))
    }
    regions_data_filtered <- left_join(regions_sf, region_counts, by = c("name" = "region"))
    regions_data_filtered <- st_transform(regions_data_filtered, crs = 4326)
    
    regions_data_filtered$event_count[is.na(regions_data_filtered$event_count)] <- 0
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = regions_data_filtered$event_count[regions_data_filtered$event_count > 0]
    )
    
    leaflet(regions_data_filtered) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ifelse(event_count == 0, "gray", pal(event_count)),
        color = "white",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "orange",
          fillOpacity = 0.9
        ),
        popup = ~paste(
          "Region:", name, 
          "<br>Event Count:", 
          ifelse(is.na(event_count), 0, event_count)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~event_count,
        title = "",
        position = "bottomright",
        labels = function(x) ifelse(is.na(x), 0, x)
      )
  })
  
  output$total_events_box <- renderInfoBox({
    infoBox(
      "Total Events", 
      value = nrow(data),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$unique_actors_box <- renderInfoBox({
    unique_actors <- length(unique(c(
      na.omit(data$actor1), 
      na.omit(data$actor2)
    )))
    
    infoBox(
      "Unique Actors", 
      value = unique_actors,
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$fatalities_box <- renderInfoBox({
    infoBox(
      "Total Fatalities", 
      value = sum(data$fatalities, na.rm = TRUE),
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$data_summary_event_type_pie <- renderPlotly({
    event_type_counts <- data %>%
      group_by(event_type) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    plot_ly(event_type_counts, 
            labels = ~event_type, 
            values = ~count, 
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = viridis(nrow(event_type_counts)))) %>%
      layout(title = "")
  })
  
  output$fatalities_by_event_type <- renderPlotly({
    fatalities_by_type <- data %>%
      group_by(event_type) %>%
      summarise(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
      arrange(desc(total_fatalities))  
    
    plot_ly(fatalities_by_type, 
            x = ~reorder(event_type, -total_fatalities), 
            y = ~total_fatalities, 
            type = 'bar',
            marker = list(color = ~total_fatalities, colorscale = 'Viridis')) %>%
      layout(title = "",
             xaxis = list(title = "Event Type", tickangle = 45),
             yaxis = list(title = "Total Fatalities"))
  })
  
  output$top_actors_plot <- renderPlotly({
    top_actors <- bind_rows(
      data %>% 
        group_by(actor1) %>% 
        summarise(event_count = n()) %>% 
        arrange(desc(event_count)) %>% 
        head(10) %>%
        rename(actor = actor1),
      data %>% 
        group_by(actor2) %>% 
        summarise(event_count = n()) %>% 
        arrange(desc(event_count)) %>% 
        head(10) %>%
        rename(actor = actor2)
    ) %>%
      group_by(actor) %>%
      summarise(total_events = sum(event_count)) %>%
      arrange(desc(total_events)) %>%  
      head(10) %>%
      filter(actor != "None")  
    
    plot_ly(top_actors, 
            x = ~reorder(actor, -total_events), 
            y = ~total_events, 
            type = 'bar',
            marker = list(color = ~total_events, colorscale = 'Plasma')) %>%
      layout(title = "",
             xaxis = list(title = "Actor", tickangle = 45),
             yaxis = list(title = "Total Events"))
  })
  
  output$interaction_types_plot <- renderPlotly({
    interaction_counts <- data %>%
      group_by(inter1) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    plot_ly(interaction_counts, 
            labels = ~inter1, 
            values = ~count, 
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = viridis(nrow(interaction_counts)))) %>%
      layout(title = "")
  })
  
  data$source_category <- ifelse(
    data$source %in% armenian_sources, "Armenian Sources",
    ifelse(
      data$source %in% azerbaijani_sources, "Azerbaijani Sources", 
      "International Sources"
    )
  )
  
  output$sources_bar_chart <- renderPlotly({
    source_counts <- data %>%
      group_by(source) %>%
      summarise(event_count = n()) %>%
      top_n(15, event_count) %>%
      arrange(desc(event_count))  
    
    plot_ly(source_counts, 
            x = ~reorder(source, -event_count),  
            y = ~event_count, 
            type = 'bar',
            marker = list(
              color = ~event_count, 
              colorscale = 'Viridis'
            )) %>%
      layout(
        title = "",
        xaxis = list(title = "Source", tickangle = 45),
        yaxis = list(title = "Number of Events")
      )
  })
  
  output$sources_category_pie <- renderPlotly({
    source_category_counts <- data %>%
      group_by(source_category) %>%
      summarise(event_count = n()) %>%
      arrange(desc(event_count))
    
    plot_ly(source_category_counts, 
            labels = ~reorder(source_category, event_count), 
            values = ~event_count, 
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("#2c8e3d", "#ff7f0e", "#1f77b4"))) %>%
      layout(title = "")
  })
  
  data <- data %>%
    filter(!is.na(region) & !is.na(event_type)) %>%
    mutate(
      region = as.factor(region),
      event_type = as.factor(event_type)
    )
  
  event_counts <- data %>%
    group_by(event_type) %>%
    summarise(event_count = n(), .groups = "drop")
  
  output$protest_info <- renderInfoBox({
    protest_count <- event_counts %>% filter(event_type == "Protests") %>% pull(event_count)
    infoBox("Protests", protest_count, icon = icon("fist-raised"), color = "blue")
  })
  
  output$battle_info <- renderInfoBox({
    battle_count <- event_counts %>% filter(event_type == "Battles") %>% pull(event_count)
    infoBox("Battles", battle_count, icon = icon("crosshairs"), color = "red")
  })
  
  output$explosions_info <- renderInfoBox({
    explosions_count <- event_counts %>% filter(event_type == "Explosions/Remote violence") %>% pull(event_count)
    infoBox("Explosions/Remote Violence", explosions_count, icon = icon("bomb"), color = "green")
  })
  
  output$riots_info <- renderInfoBox({
    riots_count <- event_counts %>% filter(event_type == "Riots") %>% pull(event_count)
    infoBox("Riots", riots_count, icon = icon("fire"), color = "orange")
  })
  
  output$strategic_info <- renderInfoBox({
    strategic_count <- event_counts %>% filter(event_type == "Strategic developments") %>% pull(event_count)
    infoBox("Strategic developments", strategic_count, icon = icon("cogs"), color = "purple")
  })
  
  output$violence_info <- renderInfoBox({
    violence_count <- event_counts %>% filter(event_type == "Violence against civilians") %>% pull(event_count)
    infoBox("Violence against civilians", violence_count, icon = icon("users-slash"), color = "yellow")
  })
  
  
  protest_events <- data %>%
    filter(event_type %in% c("Protests", "Riots"))
  
  protest_events_categorized <- protest_events %>%
    left_join(protest_categories, by = "sub_event_type")
  
  category_counts <- protest_events_categorized %>%
    group_by(year, category) %>%
    tally()
  
  total_events_per_year <- category_counts %>%
    group_by(year) %>%
    summarise(total = sum(n))
  
  category_counts_normalized <- category_counts %>%
    left_join(total_events_per_year, by = "year") %>%
    mutate(proportion = n / total)
  
  output$distribution_plot <- renderPlotly({
    plot_ly(category_counts, 
            x = ~as.factor(year), 
            y = ~n, 
            color = ~category, 
            type = "bar", 
            colors = c("skyblue", "orange"),
            text = ~paste("Year:", year, "<br>Count:", n),
            hoverinfo = "text") %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Count of Events"),
        barmode = "stack"
      )
  })
  
  output$proportional_plot <- renderPlotly({
    plot_ly(category_counts_normalized, 
            x = ~as.factor(year), 
            y = ~proportion, 
            color = ~category, 
            type = "bar", 
            colors = c("skyblue", "orange"),
            text = ~paste("Year:", year, "<br>Proportion:", scales::percent(proportion)),
            hoverinfo = "text") %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Proportion of Events"),
        barmode = "stack"
      )
  })
  
  output$peaceful_protest_box <- renderInfoBox({
    count <- data %>%
      filter(sub_event_type == "Peaceful protest") %>%
      nrow()
    
    infoBox(
      "Peaceful Protests",
      value = formatC(count, format="f", big.mark=",", digits=0),
      icon = icon("dove"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$violent_demo_box <- renderInfoBox({
    count <- data %>%
      filter(sub_event_type == "Violent demonstration") %>%
      nrow()
    
    infoBox(
      "Violent Demonstrations",
      value = formatC(count, format="f", big.mark=",", digits=0),
      icon = icon("fire"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$protest_intervention_box <- renderInfoBox({
    count <- data %>%
      filter(sub_event_type == "Protest with intervention") %>%
      nrow()
    
    infoBox(
      "Protests with Intervention",
      value = formatC(count, format="f", big.mark=",", digits=0),
      icon = icon("hand"),
      color = "orange",
      fill = TRUE
    )
  })
  
  output$excessive_force_box <- renderInfoBox({
    count <- data %>%
      filter(sub_event_type == "Excessive force against protesters") %>%
      nrow()
    
    infoBox(
      "Excessive Force Cases",
      value = formatC(count, format="f", big.mark=",", digits=0),
      icon = icon("shield"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$mob_violence_box <- renderInfoBox({
    count <- data %>%
      filter(sub_event_type == "Mob violence") %>%
      nrow()
    
    infoBox(
      "Mob Violence Incidents",
      value = formatC(count, format="f", big.mark=",", digits=0),
      icon = icon("users-slash"),
      color = "blue",
      fill = TRUE
    )
  })
  
  data_transformed <- reactive({
    data %>%
      filter(event_type %in% c("Protests", "Riots"),
             !is.na(assoc_actor_1) & assoc_actor_1 != "" & assoc_actor_1 != "None") %>% 
      mutate(assoc_actor_1 = trimws(assoc_actor_1),
             assoc_actor_1 = recode(assoc_actor_1, !!!actor_name_mapping)) %>%
      count(year, assoc_actor_1)
  })
  
  output$protestor_groups_plot <- renderPlotly({
    filtered_data <- data_transformed() %>%
      filter(year == input$protest_year) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    plot_ly(filtered_data, 
            x = ~reorder(assoc_actor_1, -n),
            y = ~n,
            type = "bar",
            marker = list(color = ~n, colorscale = "Viridis")) %>%
      layout(title = paste(""),
             xaxis = list(title = "Group Name", tickangle = 45),
             yaxis = list(title = "Number of Events"),
             showlegend = FALSE)
    
  })
  
  output$unique_actors_plot <- renderPlotly({
    unique_actors_per_year <- data %>%
      filter(event_type %in% c("Protests", "Riots"),
             !is.na(assoc_actor_1) & assoc_actor_1 != "" & assoc_actor_1 != "None") %>%
      group_by(year) %>%
      summarise(unique_actors = n_distinct(assoc_actor_1))
    
    plot_ly(unique_actors_per_year,
            x = ~year,
            y = ~unique_actors,
            type = "bar",
            marker = list(
              color = ~unique_actors,
              colorscale = "Viridis"
            ),
            hoverinfo = "text",
            hovertext = ~paste(
              "Year:", year,
              "<br>Unique Actors:", unique_actors
            )) %>%
      layout(
        title = "",
        xaxis = list(title = "Year",
                     tickmode = "array",
                     ticktext = ~year,
                     tickvals = ~year),
        yaxis = list(title = "Number of Unique Actors"),
        showlegend = FALSE
      )
  })
  
  output$protest_groups_timeline <- renderPlotly({
    timeline_data <- data_transformed() %>%
      group_by(year, assoc_actor_1) %>%
      summarise(n = sum(n), .groups = 'drop') %>%
      arrange(desc(n)) %>%
      mutate(
        short_name = substr(assoc_actor_1, 1, 10),  
        hover_text = assoc_actor_1 
      )
    
    plot_ly(timeline_data,
            x = ~year,
            y = ~n,
            color = ~short_name,  
            text = ~hover_text,  
            type = "scatter",
            mode = "lines+markers",
            hoverinfo = "text+x+y",
            hovertext = ~paste(hover_text, "\nYear:", year, "\nEvents:", n)) %>%
      layout(title = "",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Events"),
             showlegend = TRUE,
             legend = list(
               itemwidth = 30,
               xanchor = "right",
               x = 0.99
             ))
  })
  


  clean_crowd_size <- function(value) {
    if (is.na(value) || value == "no report" || value == "") return(NA_real_)
    value <- tolower(as.character(value))
    
    tryCatch({
      if (grepl("no report|no reports|local administrators", value)) {
        return(NA_real_)
      } else if (grepl("several thousand", value)) {
        return(2000)
      } else if (grepl("few thousand", value)) {
        return(1500)
      } else if (grepl("thousand", value)) {
        return(1000)
      } else if (grepl("hundred", value)) {
        return(300)
      } else if (grepl("several", value)) {
        return(5)
      } else if (grepl("few", value)) {
        return(3)
      } else if (grepl("tens", value)) {
        return(10)
      } else if (grepl("at least", value)) {
        nums <- as.numeric(unlist(regmatches(value, gregexpr("\\d+", value))))
        return(ifelse(length(nums) > 0, nums[1], NA_real_))
      } else if (grepl("between|around|-|;", value)) {
        nums <- as.numeric(unlist(regmatches(value, gregexpr("\\d+", value))))
        return(ifelse(length(nums) > 1, mean(nums, na.rm = TRUE), NA_real_))
      } else if (grepl("\\d+", value)) {
        return(as.numeric(gsub("[^0-9.]", "", value)))
      } else {
        return(NA_real_)
      }
    }, error = function(e) {
      return(NA_real_)
    })
  }
  
  processed_data <- reactive({
    req(data)
    
    
    result <- data %>%
      mutate(
        crowd_size_clean = sapply(crowd_size, clean_crowd_size)
      ) %>%
      filter(
        !is.na(crowd_size_clean),
        !is.na(latitude),
        !is.na(longitude),
        !is.na(region),
        crowd_size_clean > 0,
        latitude >= 38.8,
        latitude <= 41.3,
        longitude >= 43.5,
        longitude <= 46.5
      ) %>%
      group_by(region, year) %>%
      summarise(
        total_crowd_size = sum(crowd_size_clean, na.rm = TRUE),
        latitude = mean(latitude, na.rm = TRUE),
        longitude = mean(longitude, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(total_crowd_size > 0)
  })

  output$crowd_size_map <- renderLeaflet({
    req(input$crowd_year)
    
    filtered_data <- processed_data() %>%
      filter(year == input$crowd_year)
    
    if (nrow(filtered_data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 44.4991, lat = 40.1792, zoom = 7) %>%
          addControl(
            html = '<div style="padding: 8px; background: white;">No data available for the selected year</div>',
            position = "topright"
          )
      )
    }
    
    pal <- colorNumeric(
      palette = "Spectral",
      domain = filtered_data$total_crowd_size
    )
    
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 44.4991, lat = 40.1792, zoom = 7) %>%
      addCircleMarkers(
        data = filtered_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~log(total_crowd_size + 1) * 1.5,
        fillColor = ~pal(total_crowd_size),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Region:</b> ", region,
          "<br><b>Total Crowd Size:</b> ", formatC(total_crowd_size, format="f", big.mark=",", digits=0),
          "<br><b>Year:</b> ", year
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = filtered_data$total_crowd_size,
        title = "Crowd Size",
        opacity = 1
      )
  })
  
  output$crowd_size_trend <- renderPlotly({
    trend_data <- processed_data() %>%
      group_by(year) %>%
      summarise(
        total_crowd = sum(total_crowd_size, na.rm = TRUE),
        avg_crowd = mean(total_crowd_size, na.rm = TRUE),
        .groups = 'drop'
      )
    
 
    if (nrow(trend_data) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "No Data")
               ))
    }
    
    plot_ly() %>%
      add_trace(
        data = trend_data,
        x = ~year,
        y = ~total_crowd,
        name = "Total Crowd",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2c8e3d")
      ) %>%
      add_trace(
        data = trend_data,
        x = ~year,
        y = ~avg_crowd,
        name = "Average Crowd",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#1f77b4"),
        yaxis = "y2"
      ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Total Crowd Size",
          titlefont = list(color = "#2c8e3d"),
          tickfont = list(color = "#2c8e3d")
        ),
        yaxis2 = list(
          title = "Average Crowd Size",
          titlefont = list(color = "#1f77b4"),
          tickfont = list(color = "#1f77b4"),
          overlaying = "y",
          side = "right"
        ),
        showlegend = TRUE,
        legend = list(x = 0.1, y = 1)
      )
  })
  
  output$total_protesters_box <- renderValueBox({
    req(input$crowd_year)
    
    year_data <- processed_data() %>%
      filter(year == input$crowd_year)
    
    if(nrow(year_data) == 0) {
      total <- 0
    } else {
      total <- sum(year_data$total_crowd_size, na.rm = TRUE)
    }
    
    valueBox(
      value = formatC(total, format="f", big.mark=",", digits=0),
      subtitle = "Total Protesters",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$avg_crowd_size_box <- renderValueBox({
    req(input$crowd_year)
    
    year_data <- processed_data() %>%
      filter(year == input$crowd_year)
    
    if(nrow(year_data) == 0) {
      avg <- 0
    } else {
      avg <- mean(year_data$total_crowd_size, na.rm = TRUE)
    }
    
    valueBox(
      value = formatC(round(avg), format="f", big.mark=",", digits=0),
      subtitle = "Average Crowd Size",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$max_crowd_size_box <- renderValueBox({
    req(input$crowd_year)
    
    year_data <- processed_data() %>%
      filter(year == input$crowd_year)
    
    if(nrow(year_data) == 0) {
      max_size <- 0
    } else {
      max_size <- max(year_data$total_crowd_size, na.rm = TRUE)
    }
    
    valueBox(
      value = formatC(max_size, format="f", big.mark=",", digits=0),
      subtitle = "Largest Gathering",
      icon = icon("chart-bar"),
      color = "red"
    )
  })
  




}

shinyApp(ui = ui, server = server)
