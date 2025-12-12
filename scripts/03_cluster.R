#Step 3: Use k-means to cluster the community 311 complaint structure.
library(tidyverse)
library(tidymodels)

k_clusters <- 4 #Clustered into several

if (!dir.exists("data")) dir.create("data")

message("Step 3: Read ca_type_matrix.csv and perform clustering...")

ca_type_mat <- read_csv("data/ca_type_matrix.csv", show_col_types = FALSE)

x <- ca_type_mat %>%
  select(-community_area)

#standardization
rec <- recipe(~ ., data = x) %>%
  step_normalize(all_predictors())

x_scaled <- prep(rec) %>% bake(new_data = NULL)

set.seed(446)

# Elbow plot: select k
k_grid <- tibble(k = 2:8) %>%
  mutate(
    model        = map(k, ~ kmeans(x_scaled, centers = .x, nstart = 25)),
    tot_withinss = map_dbl(model, "tot.withinss")
  )

p_elbow <- ggplot(k_grid, aes(k, tot_withinss)) +
  geom_line() + geom_point() +
  labs(
    title = "Elbow Plot for K-means",
    x = "Number of clusters (k)",
    y = "Total within-cluster SS"
  ) +
  theme_minimal()

ggsave("data/plot_elbow.png", p_elbow, width = 6, height = 4, dpi = 300)
message("The Elbow Plot has been saved to data/plot_elbow.png")

# Actual k-means clustering
km_fit <- kmeans(x_scaled, centers = k_clusters, nstart = 50)

clustered <- ca_type_mat %>%
  mutate(cluster = factor(km_fit$cluster))

write_csv(clustered, "data/ca_clustered.csv")
message("The clustering results have been saved to data/ca_clustered.csv")
print(clustered)

# Cluster average structure heatmap
cluster_profile <- clustered %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "type", values_to = "avg_count")

p_heat <- cluster_profile %>%
  ggplot(aes(x = type, y = cluster, fill = avg_count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Average Complaint Profile by Cluster",
    x = "Complaint Type",
    y = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("data/plot_cluster_heatmap.png", p_heat, width = 9, height = 4.5, dpi = 300)
message("Figure 3 (cluster heatmap) has been saved to data/plot_cluster_heatmap.png")