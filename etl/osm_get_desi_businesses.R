library(osmdata)
library(dplyr)
library(sf)

city_name <- "Richmond, Virginia, USA"

# Search terms
name_terms <- c("halal", "pakistani", "indian", "bangladeshi", "desi")
cuisine_terms <- c("indian", "pakistani", "bangladeshi", "halal")

# Helper to enforce consistent columns
standardize_cols <- function(sf_obj) {
  needed_cols <- c("name", "amenity", "shop", "cuisine", "geometry")
  missing_cols <- setdiff(needed_cols, colnames(sf_obj))
  
  for (col in missing_cols) {
    if (col != "geometry") sf_obj[[col]] <- NA
  }
  
  sf_obj <- sf_obj[, needed_cols]
  return(sf_obj)
}

# Query functions
get_by_name <- function(term) {
  opq(bbox = city_name) %>%
    add_osm_feature(key = "amenity", value = c("restaurant", "fast_food", "cafe")) %>%
    add_osm_feature(key = "name", value = term, match_case = FALSE) %>%
    osmdata_sf()
}

get_by_cuisine <- function(cuisine) {
  opq(bbox = city_name) %>%
    add_osm_feature(key = "amenity", value = c("restaurant", "fast_food", "cafe")) %>%
    add_osm_feature(key = "cuisine", value = cuisine, match_case = FALSE) %>%
    osmdata_sf()
}

get_shops <- function(term) {
  opq(bbox = city_name) %>%
    add_osm_feature(key = "shop", value = c("supermarket", "convenience", "butcher", "greengrocer")) %>%
    add_osm_feature(key = "name", value = term, match_case = FALSE) %>%
    osmdata_sf()
}

# Extract sf layers
extract_sf <- function(res) {
  sf_objs <- list(res$osm_points, res$osm_polygons)
  sf_objs <- sf_objs[!sapply(sf_objs, is.null)]
  sf_objs <- sf_objs[sapply(sf_objs, nrow) > 0]
  
  if (length(sf_objs) == 0) return(NULL)
  
  sf_objs <- lapply(sf_objs, standardize_cols)
  do.call(rbind, sf_objs)
}

# Run queries
results <- c(
  lapply(name_terms, get_by_name),
  lapply(cuisine_terms, get_by_cuisine),
  lapply(name_terms, get_shops)
)

# Extract and merge
sf_list <- lapply(results, extract_sf)
sf_list <- sf_list[!sapply(sf_list, is.null)]

if (length(sf_list) > 0) {
  sf_list <- lapply(sf_list, standardize_cols) # Ensure consistency
  all_businesses <- do.call(rbind, sf_list) %>%
    st_transform(4326) %>%
    distinct()
  
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  st_write(all_businesses, "data/raw/osm_businesses.geojson", delete_dsn = TRUE)
  
  cat("✅ Saved", nrow(all_businesses), "businesses to data/raw/osm_businesses.geojson\n")
} else {
  cat("⚠️ Still no businesses found. Might need manual mapping.\n")
}
