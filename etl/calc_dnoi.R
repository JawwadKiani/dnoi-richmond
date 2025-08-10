library(sf)
library(dplyr)

# Step 1: Load datasets
businesses <- st_read("data/raw/osm_businesses.geojson", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

census <- st_read("data/raw/richmond_census.geojson", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

# Step 2: Spatial join — assign each business to its census tract
businesses_in_tracts <- st_join(businesses, census, join = st_within)

# Step 3: Count businesses per tract
business_counts <- businesses_in_tracts %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(business_count = n())

# Step 4: Merge counts with census data
dnoi_data <- census %>%
  left_join(business_counts, by = "GEOID") %>%
  mutate(
    business_count = ifelse(is.na(business_count), 0, business_count),
    businesses_per_capita = business_count / total_pop,
    city_median_income = median(median_income, na.rm = TRUE),
    income_weight = median_income / city_median_income,
    DNOI_score = businesses_per_capita * income_weight
  )

# Step 5: Save output
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
st_write(dnoi_data, "data/processed/richmond_dnoi.geojson", delete_dsn = TRUE)

cat("✅ Saved DNOI results to data/processed/richmond_dnoi.geojson\n")
cat("📊 DNOI score range:", range(dnoi_data$DNOI_score, na.rm = TRUE), "\n")

