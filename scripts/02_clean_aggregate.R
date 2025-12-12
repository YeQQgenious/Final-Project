#Clean 311 data and build a community × complaint type matrix.
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(naniar)
})

year_window  <- 1       #Recent year
top_n_types  <- 10      # Top N Complaint types

if (!dir.exists("data")) dir.create("data")

message("Step 2: Read raw_311.csv and clean the data...")

raw_311 <- read_csv("data/raw_311.csv", show_col_types = FALSE)

dat_311 <- raw_311 %>%
  clean_names() %>%   
  mutate(
    sr_txt        = coalesce(sr_type, sr_short_code),
    creation_date = lubridate::ymd_hms(created_date, quiet = TRUE),
    community_area = as.integer(community_area)
  ) %>%
  filter(
    !is.na(sr_txt),
    !is.na(community_area),
    !is.na(creation_date),
    creation_date >= lubridate::today() - lubridate::years(year_window)
  )

# Missing Case Diagram
if (!file.exists("data/missing_pattern.png")) {
  png("data/missing_pattern.png", width = 800, height = 600)
  vis_miss(dat_311 %>% select(community_area, sr_txt, latitude, longitude)) +
    ggtitle("Missingness Pattern in Key Fields")
  dev.off()
  message("The missing pattern diagram has been saved to data/missing_pattern.png")
}

top_types <- dat_311 %>%
  count(sr_txt, sort = TRUE) %>%
  slice_head(n = top_n_types) %>%
  pull(sr_txt)

dat_top <- dat_311 %>%
  filter(sr_txt %in% top_types)


ca_type_mat <- dat_top %>%
  count(community_area, sr_txt) %>%
  pivot_wider(
    names_from  = sr_txt,
    values_from = n,
    values_fill = 0
  )

write_csv(ca_type_mat, "data/ca_type_matrix.csv")
message("The community × type matrix has been saved to data/ca_type_matrix.csv")
print(ca_type_mat)

# TABLE1:Top N Type of bar chart
p_types <- dat_top %>%
  count(sr_txt, sort = TRUE) %>%
  ggplot(aes(x = reorder(sr_txt, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Top ", top_n_types, " 311 Complaint Types"),
    x = NULL,
    y = "Count"
  ) +
  theme_minimal()
print(p_types)

ggsave("data/plot_top_types.png", p_types, width = 8, height = 5, dpi = 300)

#TABLE2: Community Total Bar Chart
p_ca_total <- dat_top %>%
  count(community_area, name = "n_requests") %>%
  ggplot(aes(x = community_area, y = n_requests)) +
  geom_col() +
  labs(
    title = "Total 311 Requests by Community Area (Top Types Only)",
    x = "Community Area",
    y = "Requests"
  ) +
  theme_minimal()

print(p_ca_total)

ggsave("data/plot_ca_totals.png", p_ca_total, width = 8, height = 5, dpi = 300)