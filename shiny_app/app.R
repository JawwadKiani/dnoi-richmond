library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)
library(htmltools)
library(shinyWidgets)
library(osmdata)
library(tidycensus)

# Set your Census API key here (get from https://api.census.gov/data/key_signup.html)
census_api_key("your_api_key", install = FALSE, overwrite = TRUE)

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

# Filter businesses inside Richmond polygons
businesses_richmond <- st_join(businesses_all, dnoi_sf, join = st_within) %>%
  filter(!is.na(GEOID)) %>%
  distinct(id, .keep_all = TRUE)

# Filter halal/desi businesses
businesses_filtered <- businesses_richmond %>%
  filter(
    grepl("indian|pakistani|halal|desi", tolower(cuisine %||% ""), ignore.case = TRUE) |
      grepl("indian|pakistani|halal|desi", tolower(name %||% ""), ignore.case = TRUE) |
      grepl("butcher|halal", tolower(shop %||% ""), ignore.case = TRUE)
  )

# --- Fetch Transit Stops from OSM dynamically ---
get_transit_stops <- function(bbox) {
  transit_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway", value = "bus_stop") %>%
    osmdata_sf()
  return(transit_query$osm_points)
}

# Bounding box of Richmond polygons
richmond_bbox <- st_bbox(dnoi_sf)

# Fetch transit stops (on app start)
transit_stops <- get_transit_stops(richmond_bbox)

# Assign transit stops to Richmond tracts
transit_with_tracts <- st_join(transit_stops, dnoi_sf, join = st_within)

# Calculate transit stop counts per tract
transit_counts <- transit_with_tracts %>%
  st_drop_geometry() %>%
  count(GEOID, name = "transit_stop_count")

# Join transit counts to tracts
dnoi_sf <- dnoi_sf %>%
  left_join(transit_counts, by = "GEOID") %>%
  mutate(transit_stop_count = ifelse(is.na(transit_stop_count), 0, transit_stop_count))

# --- Fetch Affordability Data from ACS dynamically ---

# Census variables for median rent and median income
afford_vars <- c(median_rent = "B25064_001", median_income = "B19013_001")

# Fetch ACS data for Richmond area tracts
afford_data <- get_acs(
  geography = "tract",
  variables = afford_vars,
  state = "51", # Virginia FIPS
  county = c("760", "087", "095"), # Richmond City, Henrico, Chesterfield County FIPS codes
  year = 2021,
  geometry = FALSE,
  cache_table = TRUE
)

# Reshape affordability data to wide
afford_wide <- afford_data %>%
  select(GEOID, variable, estimate) %>%
  tidyr::pivot_wider(names_from = variable, values_from = estimate)

# Join affordability to tracts
dnoi_sf <- dnoi_sf %>%
  left_join(afford_wide, by = "GEOID")

# --- Calculate Competition Score (business density per tract) ---

business_counts <- businesses_filtered %>%
  st_drop_geometry() %>%
  count(GEOID, name = "halal_business_count")

dnoi_sf <- dnoi_sf %>%
  left_join(business_counts, by = "GEOID") %>%
  mutate(halal_business_count = ifelse(is.na(halal_business_count), 0, halal_business_count))

# --- Normalize scores 0-1 (min-max scaling) ---

normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

dnoi_sf <- dnoi_sf %>%
  mutate(
    transit_score = normalize(transit_stop_count),
    affordability_score = 1 - normalize(median_rent), # lower rent = higher score
    competition_score = 1 - normalize(halal_business_count), # less competition = higher score
    dnoi_score = 0.4 * transit_score +
      0.3 * affordability_score +
      0.3 * competition_score
  )

# Filter cuisines and types for UI
unique_cuisines <- sort(unique(tolower(businesses_filtered$cuisine[!is.na(businesses_filtered$cuisine)])))
unique_types <- sort(unique(tolower(coalesce(businesses_filtered$amenity, businesses_filtered$shop, "Unknown"))))

# --- UI ---

ui <- fluidPage(
  titlePanel("Desi Neighborhood Opportunity Index (DNOI) — Greater Richmond Area"),
  
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
      sliderInput("dnoi_threshold", "Minimum DNOI Score:", min = 0, max = 1, value = 0, step = 0.05),
      br(),
      helpText("Click markers for business details. Use filters and DNOI slider to refine."),
      br(),
      h5(textOutput("business_count"))
    ),
    
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

# --- Server ---

server <- function(input, output, session) {
  
  filtered_businesses <- reactive({
    req(input$cuisine_filter, input$type_filter)
    businesses_filtered %>%
      filter(
        tolower(cuisine) %in% input$cuisine_filter,
        tolower(coalesce(amenity, shop, "Unknown")) %in% input$type_filter
      )
  })
  
  # Reactive: tracts filtered by DNOI threshold
  filtered_tracts <- reactive({
    dnoi_sf %>% filter(dnoi_score >= input$dnoi_threshold)
  })
  
  # Filter businesses to only those within filtered tracts
  filtered_businesses_final <- reactive({
    req(filtered_businesses())
    req(filtered_tracts())
    filtered_businesses() %>% filter(GEOID %in% filtered_tracts()$GEOID)
  })
  
  output$business_count <- renderText({
    paste("Total Halal/Desi Businesses Found:", nrow(filtered_businesses_final()))
  })
  
  output$map <- renderLeaflet({
    # Join dnoi_score onto businesses for popup display
    businesses_with_score <- filtered_businesses_final() %>%
      left_join(dnoi_sf %>% st_drop_geometry() %>% select(GEOID, dnoi_score), by = "GEOID")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      {
        if (input$show_borders) {
          addPolygons(., data = filtered_tracts(),
                      fillColor = ~colorNumeric("YlGnBu", domain = dnoi_sf$dnoi_score)(dnoi_score),
                      fillOpacity = 0.4,
                      color = "#0073b7",
                      weight = 2,
                      opacity = 0.6,
                      dashArray = "6 4",
                      group = "Richmond Area")
        } else .
      } %>%
      addMarkers(data = businesses_with_score,
                 lng = ~st_coordinates(geometry)[,1],
                 lat = ~st_coordinates(geometry)[,2],
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(name %||% "Unnamed"),
                 popup = ~paste0(
                   "<b>", htmlEscape(name %||% "Unnamed"), "</b><br/>",
                   "Cuisine: ", htmlEscape(cuisine %||% "N/A"), "<br/>",
                   "Type: ", htmlEscape(amenity %||% shop %||% "Unknown"), "<br/>",
                   "DNOI Score: ", round(dnoi_score, 3)
                 ))
  })
  
  # Update markers and polygons on filter change
  observe({
    businesses_with_score <- filtered_businesses_final() %>%
      left_join(dnoi_sf %>% st_drop_geometry() %>% select(GEOID, dnoi_score), by = "GEOID")
    
    leafletProxy("map", data = businesses_with_score) %>%
      clearMarkers() %>%
      addMarkers(lng = ~st_coordinates(geometry)[,1],
                 lat = ~st_coordinates(geometry)[,2],
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(name %||% "Unnamed"),
                 popup = ~paste0(
                   "<b>", htmlEscape(name %||% "Unnamed"), "</b><br/>",
                   "Cuisine: ", htmlEscape(cuisine %||% "N/A"), "<br/>",
                   "Type: ", htmlEscape(amenity %||% shop %||% "Unknown"), "<br/>",
                   "DNOI Score: ", round(dnoi_score, 3)
                 ))
  })
  
  observe({
    proxy <- leafletProxy("map")
    if (input$show_borders) {
      proxy %>%
        clearGroup("Richmond Area") %>%
        addPolygons(data = filtered_tracts(),
                    fillColor = ~colorNumeric("YlGnBu", domain = dnoi_sf$dnoi_score)(dnoi_score),
                    fillOpacity = 0.4,
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

# --- Run the app ---
shinyApp(ui, server)
