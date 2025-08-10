# Install if not present
# install.packages(c("tidycensus", "tigris", "sf", "dplyr"))

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)

# 🔑 Get your Census API key here: https://api.census.gov/data/key_signup.html
# Then run this once (replace with your key):
# census_api_key("YOUR_KEY", install = TRUE)

# Variables:
# B01003_001 = Total Population
# B19013_001 = Median Household Income
vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001"
)

# Step 1: Get ACS data for Richmond, VA
census_data <- get_acs(
  geography = "tract",
  variables = vars,
  state = "VA",
  county = "Richmond City",
  year = 2021,
  survey = "acs5",
  geometry = TRUE
)

# Step 2: Reshape into wide format
census_wide <- census_data %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  st_drop_geometry() %>%
  tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(
    total_pop = total_pop,
    median_income = median_income
  )

# Step 3: Merge with geometry
census_geom <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "VA",
  county = "Richmond City",
  year = 2021,
  survey = "acs5",
  geometry = TRUE
) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  select(GEOID, geometry)

richmond_census <- left_join(census_geom, census_wide, by = "GEOID")

# Step 4: Save GeoJSON
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
st_write(richmond_census, "data/raw/richmond_census.geojson", delete_dsn = TRUE)

cat("✅ Saved Richmond census tract boundaries + demographics to data/raw/richmond_census.geojson\n")
