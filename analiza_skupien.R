# =============================================================================
# CLUSTER ANALYSIS – equivalent of the procedure from Statistica
# Data: baza_danych.xlsx (100 observations)
# Ward's method, Euclidean distance
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
required <- c("readxl", "ggplot2", "ggdendro", "factoextra", "cluster", "dplyr", "tidyr")
to_install <- required[!required %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(readxl)
library(ggplot2)
library(ggdendro)
library(factoextra)
library(cluster)
library(dplyr)
library(tidyr)

# =============================================================================
# STEP 1 – LOAD DATA
# =============================================================================
dane <- read_excel("baza_danych.xlsx")
names(dane) <- trimws(names(dane))   # remove potential whitespace from column names

# Rename Polish column names to English
names(dane)[names(dane) == "Bateria"]      <- "Battery"
names(dane)[names(dane) == "Aparat"]       <- "Camera"
names(dane)[names(dane) == "Wydajność"]    <- "Performance"
names(dane)[names(dane) == "Budżet"]       <- "Budget"
names(dane)[names(dane) == "Marka"]        <- "Brand"
names(dane)[names(dane) == "Funkcje AI"]   <- "AI Features"
names(dane)[names(dane) == "Id Klienta"]   <- "Customer ID"

# Columns for segmentation (new file has 2 additional features: Brand and AI Features)
cechy <- c("Battery", "Camera", "Performance", "Budget", "Brand", "AI Features")

cat("Loaded", nrow(dane), "observations,", ncol(dane), "columns.\n")
cat("Columns:", paste(names(dane), collapse = ", "), "\n\n")
print(summary(dane[, cechy]))

# =============================================================================
# STEP 2 – HIERARCHICAL CLUSTER ANALYSIS
# Standardisation (z-score) → Euclidean distance → Ward's method
# Equivalent: Statistica ➔ Cluster Analysis ➔ Ward's Method
# =============================================================================
X <- scale(dane[, cechy])          # standardisation – each feature has mean=0, sd=1
rownames(X) <- dane$`Customer ID`

dist_matrix <- dist(X, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")   # Ward D2 = classic Ward's Method

# =============================================================================
# STEP 3 – DIAGNOSTIC CHARTS
# =============================================================================

# ── 3a. ELBOW METHOD (Scree plot) ─────────────────────────────────────────────
# Equivalent: "Scree plot" in Statistica
# Look for the "elbow" – the point where the curve rises sharply

colours    <- c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")
k_selected <- 3   # <── CHANGE after inspecting the elbow / silhouette chart
heights    <- rev(hc$height)
k_max <- 15
df_elbow <- data.frame(
  k        = 2:(k_max + 1),
  distance = heights[1:k_max]
)

p_elbow <- ggplot(df_elbow, aes(x = k, y = distance)) +
  geom_line(colour = "#2980b9", linewidth = 1.2) +
  geom_point(colour = "#2980b9", size = 3.5) +
  scale_x_continuous(breaks = 2:(k_max + 1)) +
  labs(
    title = "Elbow Method – Selecting the Number of Clusters",
    x = "Number of clusters (k)", y = "Linkage distance (sum of squares)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

print(p_elbow)
ggsave("metoda_lokcia.png", p_elbow, width = 10, height = 5.5, dpi = 150)
cat("Saved: metoda_lokcia.png\n")

# ── 3b. SILHOUETTE METHOD ─────────────────────────────────────────────────────
# For each number of clusters k, compute average silhouette width (closer to 1 = better)
# Highest bar = optimal number of clusters

sil_scores <- sapply(2:k_max, function(k) {
  clusters <- cutree(hc, k = k)
  mean(silhouette(clusters, dist_matrix)[, "sil_width"])
})

df_sil <- data.frame(k = 2:k_max, sil = sil_scores)
k_sil_opt <- df_sil$k[which.max(df_sil$sil)]

p_silhouette_avg <- ggplot(df_sil, aes(x = k, y = sil)) +
  geom_col(aes(fill = k == k_sil_opt), width = 0.7, show.legend = FALSE) +
  geom_line(colour = "#2c3e50", linewidth = 0.8, linetype = "dashed") +
  geom_point(colour = "#2c3e50", size = 3) +
  scale_fill_manual(values = c("FALSE" = "#85c1e9", "TRUE" = "#e74c3c")) +
  scale_x_continuous(breaks = 2:k_max) +
  scale_y_continuous(limits = c(0, max(df_sil$sil) * 1.15),
                     labels = scales::number_format(accuracy = 0.01)) +
  annotate("text", x = k_sil_opt, y = max(df_sil$sil) * 1.07,
           label = paste("Optimum: k =", k_sil_opt),
           colour = "#e74c3c", fontface = "bold", size = 4) +
  labs(
    title = "Silhouette Method – Selecting the Number of Clusters",
    x = "Number of clusters (k)", y = "Average silhouette width"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

print(p_silhouette_avg)
ggsave("silhouette_method.png", p_silhouette_avg, width = 10, height = 5.5, dpi = 150)
cat("Saved: silhouette_method.png\n")

# Detailed silhouette plot for selected k (each observation separately)
sil_detail <- silhouette(cutree(hc, k = k_selected), dist_matrix)

p_sil_detail <- fviz_silhouette(sil_detail, palette = colours[1:k_selected],
                                 ggtheme = theme_minimal(base_size = 12)) +
  labs(
    title = paste("Silhouette plot for k =", k_selected, "clusters"),
    x = "Respondents (sorted by cluster)", y = "Silhouette width"
  ) +
  theme(plot.title = element_text(face = "bold", size = 15))

print(p_sil_detail)
ggsave("silhouette_detail.png", p_sil_detail, width = 12, height = 6, dpi = 150)
cat("Saved: silhouette_detail.png\n")

# ── 3c. DENDROGRAM ────────────────────────────────────────────────────────────
# Equivalent: "Tree diagram (dendrogram)" in Statistica
#
cut_height <- mean(
  sort(hc$height, decreasing = TRUE)[c(k_selected - 1, k_selected)]
)

# Dendrogram from factoextra – automatically colours branches
p_dendrogram <- fviz_dend(
  hc,
  k             = k_selected,
  cex           = 0.45,
  lwd           = 0.6,
  k_colors      = c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")[1:k_selected],
  color_labels_by_k = TRUE,
  rect          = TRUE,
  rect_border   = c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")[1:k_selected],
  rect_fill     = TRUE,
  main          = paste("Dendrogram – Ward's Method |", k_selected, "clusters"),
  sub           = "",
  xlab          = "Respondents (ID)",
  ylab          = "Linkage distance"
) +
  theme(plot.title = element_text(face = "bold", size = 15))

print(p_dendrogram)
ggsave("dendrogram.png", p_dendrogram, width = 16, height = 7, dpi = 150)
cat("Saved: dendrogram.png\n")

# =============================================================================
# STEP 4 – ASSIGN OBSERVATIONS TO CLUSTERS
# Equivalent: "Save classification and distances" in Statistica
# =============================================================================
dane$Cluster <- factor(cutree(hc, k = k_selected))

cat("\nDistribution of respondents across clusters:\n")
print(table(dane$Cluster))

write.csv(dane, "dane_z_klastrami.csv", row.names = FALSE)
cat("Saved: dane_z_klastrami.csv\n\n")

# =============================================================================
# STEP 5 – SEGMENT PROFILING
# Equivalent: Statistics ➔ Basic ➔ Breakdowns (means per cluster)
# =============================================================================

# ── 5a. Means table ───────────────────────────────────────────────────────────
means <- dane %>%
  group_by(Cluster) %>%
  summarise(across(all_of(cechy), mean), .groups = "drop") %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

cat("Average feature values in each cluster:\n")
print(as.data.frame(means))

# ── 5b. SEGMENT PROFILE CHART ─────────────────────────────────────────────────
# Equivalent: "Mean plot" / profiles in Statistica
# Budget is scaled to 1-5 for the chart (to make it comparable with the rest)

features_scale <- c("Battery", "Camera", "Performance", "Brand", "AI Features")   # features on 1-5 scale

df_profile <- means %>%
  select(Cluster, all_of(features_scale)) %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean")

p_profile <- ggplot(df_profile, aes(x = Feature, y = Mean,
                                    colour = Cluster, group = Cluster)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 5) +
  geom_label(aes(label = round(Mean, 2)), size = 3.5,
             show.legend = FALSE, nudge_y = 0.08, fontface = "bold") +
  scale_colour_manual(values = colours) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Segment profiles – average feature ratings (scale 1–5)",
    x = NULL, y = "Average rating", colour = "Segment (Cluster)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "top"
  )

print(p_profile)
ggsave("profile_segmentow.png", p_profile, width = 9, height = 6, dpi = 150)
cat("Saved: profile_segmentow.png\n")

# ── 5c. BUDGET PER CLUSTER CHART (bar chart) ─────────────────────────────────
df_budget <- means %>% select(Cluster, Budget)

p_budget <- ggplot(df_budget, aes(x = Cluster, y = Budget, fill = Cluster)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(round(Budget, 0), " PLN")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = colours) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ",", suffix = " PLN"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Average smartphone budget per segment",
    x = "Segment (Cluster)", y = "Average budget [PLN]", fill = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "none"
  )

print(p_budget)
ggsave("budzet_per_segment.png", p_budget, width = 7, height = 5, dpi = 150)
cat("Saved: budzet_per_segment.png\n")

# ── 5d. BOX-WHISKER PLOTS ─────────────────────────────────────────────────────
# Equivalent: "Box-whisker plots" in Statistica

df_box <- dane %>%
  pivot_longer(cols = all_of(cechy), names_to = "Feature", values_to = "Value")

p_boxplot <- ggplot(df_box, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.8,
               outlier.fill = "white") +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colours) +
  labs(
    title = "Box-Whisker Plots – Feature Distribution by Segment",
    x = "Segment (Cluster)", y = "Value", fill = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 12)
  )

print(p_boxplot)
ggsave("boxploty_cechy.png", p_boxplot, width = 11, height = 8, dpi = 150)
cat("Saved: boxploty_cechy.png\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("OUTPUT FILES:\n")
cat("  metoda_lokcia.png      – cluster count selection (elbow)\n")
cat("  dendrogram.png         – hierarchical cluster structure\n")
cat("  profile_segmentow.png  – mean feature ratings per cluster\n")
cat("  budzet_per_segment.png – average budget per segment\n")
cat("  boxploty_cechy.png     – feature value distributions per cluster\n")
cat("  dane_z_klastrami.csv   – data with 'Cluster' column\n")
cat("\nMeans per cluster (segment interpretation):\n")
print(as.data.frame(means))
cat("══════════════════════════════════════════════════════════\n")
cat("TIP: Change k_selected (line ~53) if the elbow suggests a different number.\n")
