library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)
library(htmltools)
library(shinyWidgets) # for enhanced search input

# Helper: safe string fallback
`%||%` <- function(a, b) {
  if (length(a) == 0) return(b)
  ifelse(!is.na(a) & a != "", a, b)
}

# Load Richmond polygons (greater Richmond area)
dnoi_sf <- st_read(here("data", "processed", "greater_richmond_tracts.geojson"), quiet = TRUE) %>%
  st_transform(4326)

# Load business data
businesses_all <- st_read(here("data", "raw", "export.geojson"), quiet = TRUE) %>%
  st_transform(4326)

# Keep only businesses inside Richmond polygons
businesses_richmond <- st_join(businesses_all, dnoi_sf, join = st_within) %>%
  filter(!is.na(GEOID))

# Filter businesses for halal/desi keywords
businesses_filtered <- businesses_richmond %>%
  filter(
    grepl("indian|pakistani|halal|desi", tolower(cuisine %||% ""), ignore.case = TRUE) |
      grepl("indian|pakistani|halal|desi", tolower(name %||% ""), ignore.case = TRUE) |
      grepl("butcher|halal", tolower(shop %||% ""), ignore.case = TRUE)
  ) %>%
  distinct(id, .keep_all = TRUE)  # Remove duplicates by unique id

# Extract cuisines and types for filters
unique_cuisines <- sort(unique(tolower(businesses_filtered$cuisine[!is.na(businesses_filtered$cuisine)])))
unique_types <- sort(unique(tolower(coalesce(businesses_filtered$amenity, businesses_filtered$shop, "Unknown"))))

# UI
ui <- fluidPage(
  titlePanel("Halal/Desi Businesses in Greater Richmond Area"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "cuisine_filter",
        label = "Filter by Cuisine:",
        choices = unique_cuisines,
        selected = unique_cuisines,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId = "type_filter",
        label = "Filter by Business Type:",
        choices = unique_types,
        selected = unique_types,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      checkboxInput("show_borders", "Show Richmond Area Borders", value = TRUE),
      br(),
      helpText("Click markers for business details. Use filters to refine visible businesses."),
      br(),
      h5(textOutput("business_count"))
    ),
    
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered businesses based on cuisine and type selections
  filtered_businesses <- reactive({
    req(input$cuisine_filter, input$type_filter)
    
    businesses_filtered %>%
      filter(
        tolower(cuisine) %in% input$cuisine_filter,
        tolower(coalesce(amenity, shop, "Unknown")) %in% input$type_filter
      )
  })
  
  # Display count of filtered businesses
  output$business_count <- renderText({
    paste("Total Halal/Desi Businesses Found:", nrow(filtered_businesses()))
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      {
        if (input$show_borders) {
          addPolygons(., data = dnoi_sf,
                      fillColor = "transparent",
                      color = "#0073b7",
                      weight = 2,
                      opacity = 0.6,
                      dashArray = "6 4",
                      group = "Richmond Area")
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
                 ))
  })
  
  # Update markers when filters or toggle change
  observe({
    leafletProxy("map", data = filtered_businesses()) %>%
      clearMarkers() %>%
      addMarkers(lng = ~st_coordinates(geometry)[,1],
                 lat = ~st_coordinates(geometry)[,2],
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(name %||% "Unnamed"),
                 popup = ~paste0(
                   "<b>", htmlEscape(name %||% "Unnamed"), "</b><br/>",
                   "Cuisine: ", htmlEscape(cuisine %||% "N/A"), "<br/>",
                   "Type: ", htmlEscape(amenity %||% shop %||% "Unknown")
                 ))
  })
  
  # Show/hide Richmond polygon borders on toggle
  observe({
    proxy <- leafletProxy("map")
    if (input$show_borders) {
      proxy %>%
        clearGroup("Richmond Area") %>%
        addPolygons(data = dnoi_sf,
                    fillColor = "transparent",
                    color = "#0073b7",
                    weight = 2,
                    opacity = 0.6,
                    dashArray = "6 4",
                    group = "Richmond Area")
    } else {
      proxy %>% clearGroup("Richmond Area")
    }
  })
  
}

# Run app
shinyApp(ui, server)
