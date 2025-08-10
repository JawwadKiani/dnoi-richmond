library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(here)

# Helper for NA/empty string fallback
`%||%` <- function(a, b) {
  if (length(a) == 0) return(b)
  ifelse(!is.na(a) & a != "", a, b)
}

# Load DNOI data
dnoi_sf <- st_read(here("data", "processed", "greater_richmond_tracts.geojson"), quiet = TRUE) %>%
  st_transform(4326)

# Load businesses
businesses_all <- st_read(here("data", "raw", "export.geojson"), quiet = TRUE)

# Filter only point geometries
businesses_points <- businesses_all[st_geometry_type(businesses_all) %in% c("POINT", "MULTIPOINT"), ]

# Valid and transform CRS
businesses_sf <- st_transform(st_make_valid(businesses_points), 4326)

# Filter businesses on keywords
businesses_filtered <- businesses_sf %>%
  filter(
    grepl("indian|pakistani|halal|desi", tolower(cuisine %||% ""), ignore.case = TRUE) |
      grepl("indian|pakistani|halal|desi", tolower(name %||% ""), ignore.case = TRUE) |
      grepl("butcher|halal", tolower(shop %||% ""), ignore.case = TRUE)
  )

# Count businesses per tract via spatial join
businesses_in_tracts <- st_join(businesses_filtered, dnoi_sf, join = st_within)

business_counts <- businesses_in_tracts %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(business_count = n(), .groups = "drop")

# Add business count to DNOI data, fill NAs with 0
dnoi_sf <- dnoi_sf %>%
  left_join(business_counts, by = "GEOID") %>%
  mutate(business_count = ifelse(is.na(business_count), 0, business_count))

# Numeric column for filtering and color
dnoi_numeric_column <- "ALAND"

# Color palette based on numeric column
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = dnoi_sf[[dnoi_numeric_column]],
  na.color = "transparent"
)

ui <- fluidPage(
  titlePanel("Desi Neighborhood Opportunity Index (DNOI) — Richmond, VA"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "dnoiFilter",
        paste("Minimum", dnoi_numeric_column, ":"),
        min = min(dnoi_sf[[dnoi_numeric_column]], na.rm = TRUE),
        max = max(dnoi_sf[[dnoi_numeric_column]], na.rm = TRUE),
        value = min(dnoi_sf[[dnoi_numeric_column]], na.rm = TRUE),
        step = (max(dnoi_sf[[dnoi_numeric_column]], na.rm = TRUE) - min(dnoi_sf[[dnoi_numeric_column]], na.rm = TRUE)) / 1000
      ),
      br(),
      helpText("Use the slider to filter census tracts by minimum value."),
      hr(),
      h4("Selected Tract Details"),
      uiOutput("selectedTractName"),
      plotOutput("tractStatsPlot")
    ),
    mainPanel(
      leafletOutput("dnoiMap", height = 600)
    )
  )
)

server <- function(input, output, session) {
  selected_tract <- reactiveVal(NULL)
  
  filtered_data <- reactive({
    req(input$dnoiFilter)
    dnoi_sf %>% filter(!is.na(.data[[dnoi_numeric_column]]) & .data[[dnoi_numeric_column]] >= input$dnoiFilter)
  })
  
  output$dnoiMap <- renderLeaflet({
    fd <- filtered_data()
    
    # Extract numeric vector ahead of time
    numeric_vals <- fd[[dnoi_numeric_column]]
    
    leaflet(fd) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~GEOID,
        fillColor = pal(numeric_vals), # Use palette with numeric vector, NOT inside formula
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(
          "<strong>", NAME, "</strong><br/>",
          "Business Count: ", business_count, "<br/>",
          dnoi_numeric_column, ": ", formatC(fd[[dnoi_numeric_column]], format = "f", digits = 0, big.mark = ",")
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addCircleMarkers(
        data = businesses_filtered,
        lng = ~st_coordinates(businesses_filtered)[,1],
        lat = ~st_coordinates(businesses_filtered)[,2],
        radius = 5,
        color = "green",
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~paste0(
          "<strong>", name %||% "Unnamed", "</strong><br/>",
          "Type: ", amenity %||% shop %||% "Unknown", "<br/>",
          "Cuisine: ", cuisine %||% "N/A"
        ) %>% lapply(htmltools::HTML)
      ) %>%
      addLegend(
        pal = pal,
        values = numeric_vals,
        opacity = 0.7,
        title = dnoi_numeric_column,
        position = "bottomright"
      )
  })
  
  observeEvent(input$dnoiMap_shape_click, {
    click <- input$dnoiMap_shape_click
    if (is.null(click$id)) return()
    
    selected_tract(click$id)
    
    tract_poly <- dnoi_sf %>% filter(GEOID == click$id)
    tract_poly <- st_make_valid(tract_poly)
    
    if (!any(st_geometry_type(tract_poly) %in% c("POLYGON", "MULTIPOLYGON"))) {
      showNotification("Selected tract does not have valid polygon geometry.", type = "error")
      return()
    }
    
    if (!any(st_geometry_type(businesses_filtered) %in% c("POINT", "MULTIPOINT"))) {
      showNotification("Business data does not contain valid point geometry.", type = "error")
      return()
    }
    
    businesses_in_tract <- st_join(businesses_filtered, tract_poly, join = st_within) %>%
      filter(!is.na(GEOID))
    
    if (nrow(businesses_in_tract) == 0) {
      popup_content <- "<b>No halal/desi businesses found in this tract.</b>"
    } else {
      biz_list <- paste0(
        "<ul>",
        paste0(
          "<li><b>", 
          businesses_in_tract$name %||% "Unnamed", 
          "</b> (", 
          businesses_in_tract$amenity %||% businesses_in_tract$shop %||% "Unknown", 
          ")</li>",
          collapse = ""
        ),
        "</ul>"
      )
      popup_content <- paste0("<b>Halal/Desi Businesses:</b>", biz_list)
    }
    
    leafletProxy("dnoiMap") %>%
      clearPopups() %>%
      addPopups(
        lng = click$lng, lat = click$lat,
        popup = popup_content,
        layerId = "business_popup"
      )
  })
  
  output$selectedTractName <- renderUI({
    req(selected_tract())
    tract <- dnoi_sf %>% filter(GEOID == selected_tract())
    if (nrow(tract) == 0) return(NULL)
    HTML(paste0("<strong>", tract$NAME[1], "</strong>"))
  })
  
  output$tractStatsPlot <- renderPlot({
    req(selected_tract())
    tract <- dnoi_sf %>% filter(GEOID == selected_tract())
    if (nrow(tract) == 0) return(NULL)
    
    values <- c(
      `Businesses` = tract$business_count,
      `Population` = NA,
      `Median Income` = NA
    )
    
    barplot(
      values,
      col = c("#F8766D", "#00BFC4", "#7CAE00"),
      main = paste("Statistics for", tract$NAME),
      ylab = "Count / Value",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, max(values, na.rm = TRUE) * 1.2)
    )
  })
}

shinyApp(ui, server)
