#Step 4: Construct an interactive map of Chicago Community Areas colored by cluster.
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sf)
  library(leaflet)
})

if (!dir.exists("data")) dir.create("data")

message("Step 4: Read the clustering results ca_clustered.csv and the original 311 data raw_311.csv...")

clustered <- read_csv("data/ca_clustered.csv", show_col_types = FALSE)
raw_311   <- read_csv("data/raw_311.csv",   show_col_types = FALSE)


dat_311 <- raw_311 %>%
  clean_names() %>%
  mutate(
    sr_txt        = coalesce(sr_type, sr_short_code),
    creation_date = lubridate::ymd_hms(created_date, quiet = TRUE),
    community_area = as.integer(community_area),
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    !is.na(sr_txt),
    !is.na(community_area),
    !is.na(creation_date),
    !is.na(latitude),
    !is.na(longitude)
  )


dat_map <- dat_311 %>%
  inner_join(
    clustered %>% select(community_area, cluster),
    by = "community_area"
  )


set.seed(446)
max_points <- 5000
if (nrow(dat_map) > max_points) {
  dat_map_plot <- dat_map %>% sample_n(max_points)
  message("With a large number of records, sampling", max_points, " The bar is used for map display.")
} else {
  dat_map_plot <- dat_map
}

pal <- colorFactor("Set2", domain = dat_map_plot$cluster)

m <- leaflet(dat_map_plot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.7,
    color = ~pal(cluster),
    popup = ~paste0(
      "<b>SR Number: </b>", sr_number, "<br>",
      "<b>Type: </b>", sr_txt, "<br>",
      "<b>Community Area: </b>", community_area, "<br>",
      "<b>Cluster: </b>", cluster
    )
  ) %>%
  addLegend("bottomright", pal = pal, values = ~cluster,
            title = "311 Complaint Clusters")

print(m)

message("An interactive 311-dot plot has been generated (colored by cluster)")