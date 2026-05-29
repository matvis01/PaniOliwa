# =============================================================================
# ANALIZA SKUPIEŃ – odpowiednik procedury z programu Statistica
# Dane: baza_danych.xlsx (100 obserwacji)
# Metoda Warda, odległość euklidesowa
# =============================================================================

# ── 0. Pakiety ────────────────────────────────────────────────────────────────
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
# KROK 1 – WCZYTANIE DANYCH
# =============================================================================
dane <- read_excel("baza_danych.xlsx")
names(dane) <- trimws(names(dane))   # usuń ewentualne spacje z nazw kolumn

# Kolumny do segmentacji (nowy plik ma 2 dodatkowe cechy: Marka i Funkcje AI)
cechy <- c("Bateria", "Aparat", "Wydajność", "Budżet", "Marka", "Funkcje AI")

cat("Wczytano", nrow(dane), "obserwacji,", ncol(dane), "kolumn.\n")
cat("Kolumny:", paste(names(dane), collapse = ", "), "\n\n")
print(summary(dane[, cechy]))

# =============================================================================
# KROK 2 – HIERARCHICZNA ANALIZA SKUPIEŃ
# Standaryzacja (z-score) → odległość euklidesowa → metoda Warda
# Odpowiednik: Statistica ➔ Analiza skupień ➔ Metoda Warda
# =============================================================================
X <- scale(dane[, cechy])          # standaryzacja – każda cecha ma mean=0, sd=1
rownames(X) <- dane$`Id Klienta`

dist_macierz <- dist(X, method = "euclidean")
hc <- hclust(dist_macierz, method = "ward.D2")   # Ward D2 = klasyczna Metoda Warda

# =============================================================================
# KROK 3 – WYKRESY DIAGNOSTYCZNE
# =============================================================================

# ── 3a. METODA ŁOKCIA (Wykres osierocenia) ────────────────────────────────────
# Odpowiednik: "Wykres osierocenia" w Statistica
# Szukamy "załamania" – miejsca, gdzie krzywa gwałtownie rośnie

kolory    <- c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")
k_wybrane <- 3   # <── ZMIEŃ po analizie wykresu łokcia / silhouette
wysokosci <- rev(hc$height)
k_max <- 15
df_lokiec <- data.frame(
  k         = 2:(k_max + 1),
  odleglosc = wysokosci[1:k_max]
)

p_lokiec <- ggplot(df_lokiec, aes(x = k, y = odleglosc)) +
  geom_line(colour = "#2980b9", linewidth = 1.2) +
  geom_point(colour = "#2980b9", size = 3.5) +
  scale_x_continuous(breaks = 2:(k_max + 1)) +
  labs(
    title    = "Metoda Łokcia – wybór liczby skupień",
    subtitle = "Szukaj punktu, po którym krzywa gwałtownie rośnie ('załamania')",
    x = "Liczba skupień (k)", y = "Odległość wiązania (suma kwadratów)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

print(p_lokiec)
ggsave("metoda_lokcia.png", p_lokiec, width = 10, height = 5.5, dpi = 150)
cat("Zapisano: metoda_lokcia.png\n")

# ── 3b. METODA SILHOUETTE ─────────────────────────────────────────────────────
# Dla każdej liczby skupień k liczy średnią szerokość sylwetki (im bliżej 1, tym lepiej)
# Najwyższy słupek = optymalna liczba skupień

sil_scores <- sapply(2:k_max, function(k) {
  klastry <- cutree(hc, k = k)
  mean(silhouette(klastry, dist_macierz)[, "sil_width"])
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
    title    = "Metoda Silhouette – wybór liczby skupień",
    subtitle = "Czerwony słupek = optymalna liczba skupień (najwyższa średnia szerokość sylwetki)",
    x = "Liczba skupień (k)", y = "Średnia szerokość sylwetki"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

print(p_silhouette_avg)
ggsave("silhouette_method.png", p_silhouette_avg, width = 10, height = 5.5, dpi = 150)
cat("Zapisano: silhouette_method.png\n")

# Wykres sylwetki dla wybranego k (szczegółowy – każda obserwacja osobno)
sil_detail <- silhouette(cutree(hc, k = k_wybrane), dist_macierz)

p_sil_detail <- fviz_silhouette(sil_detail, palette = kolory[1:k_wybrane],
                                 ggtheme = theme_minimal(base_size = 12)) +
  labs(
    title    = paste("Wykres sylwetki dla k =", k_wybrane, "skupień"),
    subtitle = "Każdy słupek = jeden respondent. Wartości ujemne = błędna klasyfikacja.",
    x = "Respondenci (posortowani wg klastra)", y = "Szerokość sylwetki"
  ) +
  theme(plot.title = element_text(face = "bold", size = 15))

print(p_sil_detail)
ggsave("silhouette_detail.png", p_sil_detail, width = 12, height = 6, dpi = 150)
cat("Zapisano: silhouette_detail.png\n")

# ── 3c. DENDROGRAM ────────────────────────────────────────────────────────────
# Odpowiednik: "Wykres drzewkowy (dendrogram)" w Statistica
#
wysokosc_ciecia <- mean(
  sort(hc$height, decreasing = TRUE)[c(k_wybrane - 1, k_wybrane)]
)

# Dendrogram z pakietu factoextra – automatycznie koloruje gałęzie
p_dendrogram <- fviz_dend(
  hc,
  k             = k_wybrane,
  cex           = 0.45,
  lwd           = 0.6,
  k_colors      = c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")[1:k_wybrane],
  color_labels_by_k = TRUE,
  rect          = TRUE,
  rect_border   = c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")[1:k_wybrane],
  rect_fill     = TRUE,
  main          = paste("Dendrogram – Metoda Warda |", k_wybrane, "skupień"),
  sub           = "Każdy kolor to osobny segment; wysokość łączenia = miara różnorodności",
  xlab          = "Respondenci (ID)",
  ylab          = "Odległość wiązania"
) +
  theme(plot.title = element_text(face = "bold", size = 15))

print(p_dendrogram)
ggsave("dendrogram.png", p_dendrogram, width = 16, height = 7, dpi = 150)
cat("Zapisano: dendrogram.png\n")

# =============================================================================
# KROK 4 – PRZYPISANIE OBSERWACJI DO KLASTRÓW
# Odpowiednik: "Zapisz klasyfikację i odległości" w Statistica
# =============================================================================
dane$Klaster <- factor(cutree(hc, k = k_wybrane))

cat("\nRozkład respondentów w klastrach:\n")
print(table(dane$Klaster))

write.csv(dane, "dane_z_klastrami.csv", row.names = FALSE)
cat("Zapisano: dane_z_klastrami.csv\n\n")

# =============================================================================
# KROK 5 – PROFILOWANIE SEGMENTÓW
# Odpowiednik: Statystyki ➔ Podstawowe ➔ Przekroje (średnie per klaster)
# =============================================================================

# ── 5a. Tabela średnich ───────────────────────────────────────────────────────
srednie <- dane %>%
  group_by(Klaster) %>%
  summarise(across(all_of(cechy), mean), .groups = "drop") %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

cat("Średnie wartości cech w każdym klastrze:\n")
print(as.data.frame(srednie))

# ── 5b. WYKRES PROFILI SEGMENTÓW ──────────────────────────────────────────────
# Odpowiednik: "Wykres średnich" / profile w Statistica
# Budżet skalujemy do 1-5 na potrzeby wykresu (żeby był porównywalny z resztą)

cechy_skala <- c("Bateria", "Aparat", "Wydajność", "Marka", "Funkcje AI")   # cechy na skali 1-5

df_profil <- srednie %>%
  select(Klaster, all_of(cechy_skala)) %>%
  pivot_longer(-Klaster, names_to = "Cecha", values_to = "Srednia")

p_profil <- ggplot(df_profil, aes(x = Cecha, y = Srednia,
                                   colour = Klaster, group = Klaster)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 5) +
  geom_label(aes(label = round(Srednia, 2)), size = 3.5,
             show.legend = FALSE, nudge_y = 0.08, fontface = "bold") +
  scale_colour_manual(values = kolory) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title    = "Profile segmentów – średnie oceny cech (skala 1–5)",
    subtitle = "Każda linia to jeden segment. Wyżej = ważniejsza/wyżej oceniana cecha.",
    x = NULL, y = "Średnia ocena", colour = "Segment (Klaster)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "top"
  )

print(p_profil)
ggsave("profile_segmentow.png", p_profil, width = 9, height = 6, dpi = 150)
cat("Zapisano: profile_segmentow.png\n")

# ── 5c. WYKRES BUDŻETU PER KLASTER (słupkowy) ────────────────────────────────
df_budzet <- srednie %>% select(Klaster, Budżet)

p_budzet <- ggplot(df_budzet, aes(x = Klaster, y = Budżet, fill = Klaster)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(round(Budżet, 0), " zł")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = kolory) +
  scale_y_continuous(labels = scales::label_comma(big.mark = " ", suffix = " zł"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Średni budżet na smartfon per segment",
    x = "Segment (Klaster)", y = "Średni budżet [zł]", fill = "Klaster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "none"
  )

print(p_budzet)
ggsave("budzet_per_segment.png", p_budzet, width = 7, height = 5, dpi = 150)
cat("Zapisano: budzet_per_segment.png\n")

# ── 5d. WYKRESY RAMKA-WĄSY (Box-plot) ────────────────────────────────────────
# Odpowiednik: "Wykresy ramka-wąsy" w Statistica

df_box <- dane %>%
  pivot_longer(cols = all_of(cechy), names_to = "Cecha", values_to = "Wartosc")

p_boxplot <- ggplot(df_box, aes(x = Klaster, y = Wartosc, fill = Klaster)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.8,
               outlier.fill = "white") +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  facet_wrap(~ Cecha, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = kolory) +
  labs(
    title    = "Wykresy ramka-wąsy – rozkład cech w segmentach",
    subtitle = "Pudełko = Q1–Q3, gruba linia = mediana, wąsy = 1.5×IQR",
    x = "Segment (Klaster)", y = "Wartość", fill = "Klaster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 12)
  )

print(p_boxplot)
ggsave("boxploty_cechy.png", p_boxplot, width = 11, height = 8, dpi = 150)
cat("Zapisano: boxploty_cechy.png\n")

# =============================================================================
# PODSUMOWANIE
# =============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("PLIKI WYNIKOWE:\n")
cat("  metoda_lokcia.png      – dobór liczby skupień (łokieć)\n")
cat("  dendrogram.png         – struktura hierarchiczna skupień\n")
cat("  profile_segmentow.png  – porównanie średnich ocen per klaster\n")
cat("  budzet_per_segment.png – średni budżet w każdym segmencie\n")
cat("  boxploty_cechy.png     – rozkłady wartości per klaster\n")
cat("  dane_z_klastrami.csv   – dane z kolumną 'Klaster'\n")
cat("\nŚrednie per klaster (interpretacja segmentów):\n")
print(as.data.frame(srednie))
cat("══════════════════════════════════════════════════════════\n")
cat("WSKAZÓWKA: Zmień k_wybrane (linia ~70) jeśli łokieć sugeruje inną liczbę.\n")
