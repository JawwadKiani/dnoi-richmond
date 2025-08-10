library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)
library(htmltools)
library(shinyWidgets)

# Safe string fallback helper
`%||%` <- function(a, b) {
  if (length(a) == 0) return(b)
  ifelse(!is.na(a) & a != "", a, b)
}

# --------------------------
# Load spatial polygons (Greater Richmond)
dnoi_sf <- st_read(here("data", "processed", "greater_richmond_tracts.geojson"), quiet = TRUE) %>%
  st_transform(4326)

# Load businesses
businesses_all <- st_read(here("data", "raw", "export.geojson"), quiet = TRUE) %>%
  st_transform(4326)

# Filter businesses inside Richmond polygons
businesses_richmond <- st_join(businesses_all, dnoi_sf, join = st_within) %>%
  filter(!is.na(GEOID))

# Filter for halal/desi businesses
businesses_filtered <- businesses_richmond %>%
  filter(
    grepl("indian|pakistani|halal|desi", tolower(cuisine %||% ""), ignore.case = TRUE) |
      grepl("indian|pakistani|halal|desi", tolower(name %||% ""), ignore.case = TRUE) |
      grepl("butcher|halal", tolower(shop %||% ""), ignore.case = TRUE)
  ) %>%
  distinct(id, .keep_all = TRUE)

# -- Additional Data Placeholders --
# Load transit data (bus stops, metro stations), affordability data, and community resources here.
# For example:
# transit_sf <- st_read(here("data","processed","transit_stops.geojson")) %>% st_transform(4326)
# affordability_df <- read.csv(here("data","processed","affordability.csv"))

# --- Calculate metrics per neighborhood polygon ---

# 1. Count of halal/desi businesses per polygon
business_count_per_tract <- businesses_filtered %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(business_count = n())

# Join counts back to polygons
dnoi_sf <- dnoi_sf %>%
  left_join(business_count_per_tract, by = "GEOID") %>%
  mutate(business_count = ifelse(is.na(business_count), 0, business_count))

# 2. Placeholder: Transit access score (0-1)
# For now, random scores; replace with real transit data and accessibility analysis
set.seed(42)
dnoi_sf$transit_score <- runif(nrow(dnoi_sf), 0, 1)

# 3. Placeholder: Affordability score (0-1)
# Replace with normalized affordability index, e.g. inverse of median rent
dnoi_sf$affordability_score <- runif(nrow(dnoi_sf), 0, 1)

# 4. Competition score (inverse of business_count to favor less crowded areas)
dnoi_sf$competition_score <- 1 / (1 + dnoi_sf$business_count)

# 5. Composite opportunity score: weighted sum (weights can be tuned)
dnoi_sf$opportunity_score <- with(dnoi_sf,
                                  0.4 * transit_score +
                                    0.3 * affordability_score +
                                    0.3 * competition_score)

# Normalize opportunity score to 0-100 scale
dnoi_sf$opportunity_score <- scales::rescale(dnoi_sf$opportunity_score, to = c(0,100))

# UI choices
unique_cuisines <- sort(unique(tolower(businesses_filtered$cuisine[!is.na(businesses_filtered$cuisine)])))
unique_types <- sort(unique(tolower(coalesce(businesses_filtered$amenity, businesses_filtered$shop, "Unknown"))))

ui <- fluidPage(
  titlePanel("Desi Neighborhood Opportunity Index (DNOI) — Greater Richmond Area"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("cuisine_filter", "Filter by Cuisine:", choices = unique_cuisines,
                  selected = unique_cuisines, multiple = TRUE, options = list(`actions-box`=TRUE)),
      pickerInput("type_filter", "Filter by Business Type:", choices = unique_types,
                  selected = unique_types, multiple = TRUE, options = list(`actions-box`=TRUE)),
      checkboxInput("show_borders", "Show Richmond Area Borders", TRUE),
      sliderInput("opportunity_filter", "Minimum Opportunity Score:", min = 0, max = 100, value = 0),
      br(),
      helpText("Click markers for business details. Filter neighborhoods by opportunity score and business types."),
      br(),
      h5(textOutput("business_count")),
      h5(textOutput("neighborhood_count"))
    ),
    
    mainPanel(
      leafletOutput("map", height = "750px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered businesses by cuisine/type
  filtered_businesses <- reactive({
    req(input$cuisine_filter, input$type_filter)
    businesses_filtered %>%
      filter(tolower(cuisine) %in% input$cuisine_filter,
             tolower(coalesce(amenity, shop, "Unknown")) %in% input$type_filter)
  })
  
  # Reactive filtered neighborhoods by opportunity score
  filtered_tracts <- reactive({
    dnoi_sf %>% filter(opportunity_score >= input$opportunity_filter)
  })
  
  # Counts output
  output$business_count <- renderText({
    paste("Total Halal/Desi Businesses Found:", nrow(filtered_businesses()))
  })
  
  output$neighborhood_count <- renderText({
    paste("Neighborhoods with opportunity score >=", input$opportunity_filter, ":",
          nrow(filtered_tracts()))
  })
  
  # Render main leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      {
        if (input$show_borders) {
          addPolygons(., data = filtered_tracts(),
                      fillColor = ~colorNumeric("YlGnBu", opportunity_score)(opportunity_score),
                      color = "#0073b7",
                      weight = 2,
                      opacity = 0.7,
                      dashArray = "6 4",
                      group = "Richmond Area",
                      label = ~paste0(NAME, "<br>Opportunity Score: ", round(opportunity_score,1))
          )
        } else .
      } %>%
      addMarkers(data = filtered_businesses(),
                 lng = ~st_coordinates(geometry)[,1],
                 lat = ~st_coordinates(geometry)[,2],
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(name %||% "Unnamed"),
                 popup = ~paste0(
                   "<b>", htmlEscape(name %||% "Unnamed"), "</b><br/>",
                   "Cuisine: ", htmlEscape(cuisine %||% "N/A"), "<br/>",
                   "Type: ", htmlEscape(amenity %||% shop %||% "Unknown")
                 )) %>%
      addLegend("bottomright",
                pal = colorNumeric("YlGnBu", domain = c(0,100)),
                values = filtered_tracts()$opportunity_score,
                title = "Neighborhood Opportunity Score",
                opacity = 0.7)
  })
  
  # Update markers and polygons dynamically
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    # Add polygons if toggle is on
    if(input$show_borders){
      proxy %>% addPolygons(data = filtered_tracts(),
                            fillColor = ~colorNumeric("YlGnBu", opportunity_score)(opportunity_score),
                            color = "#0073b7",
                            weight = 2,
                            opacity = 0.7,
                            dashArray = "6 4",
                            group = "Richmond Area",
                            label = ~paste0(NAME, "<br>Opportunity Score: ", round(opportunity_score,1))
      )
    }
    
    # Add filtered businesses markers
    proxy %>% addMarkers(data = filtered_businesses(),
                         lng = ~st_coordinates(geometry)[,1],
                         lat = ~st_coordinates(geometry)[,2],
                         clusterOptions = markerClusterOptions(),
                         label = ~htmlEscape(name %||% "Unnamed"),
                         popup = ~paste0(
                           "<b>", htmlEscape(name %||% "Unnamed"), "</b><br/>",
                           "Cuisine: ", htmlEscape(cuisine %||% "N/A"), "<br/>",
                           "Type: ", htmlEscape(amenity %||% shop %||% "Unknown")
                         ))
    
    # Add legend again
    proxy %>% addLegend("bottomright",
                        pal = colorNumeric("YlGnBu", domain = c(0,100)),
                        values = filtered_tracts()$opportunity_score,
                        title = "Neighborhood Opportunity Score",
                        opacity = 0.7)
  })
}

shinyApp(ui, server)
