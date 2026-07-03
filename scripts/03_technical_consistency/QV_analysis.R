## Quina Valley and Longtan scraper comparison
## Groups:
##   SC_Quina     = Quina scraper from Quina_scraper_surface.xlsx
##   LT_Quina     = Quina scraper from Longtan_lithic_tools.xlsx
##   LT_Ordinary  = Ordinary scraper from Longtan_lithic_tools.xlsx

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan",
                       "rstatix", "ggpubr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(rstatix)

set.seed(123)

variables <- c(
  "Thickness",
  "Retouch_length_index",
  "Ave_GIUR",
  "N_Scar",
  "Ave_RG",
  "Edge_Angle"
)

group_colors <- c(
  SC_Quina = "#E07C90",
  LT_Quina = "#E6C25C",
  LT_Ordinary = "#6BA8CE"
)

group_fills <- c(
  SC_Quina = "#E07C90",
  LT_Quina = "#E6C25C",
  LT_Ordinary = "#6BA8CE"
)

sc_path <- "H:/Quina_valleys/data/Quina_scraper_surface.xlsx"
lt_path <- "H:/Quina_valleys/data/Longtan_lithic_tools.xlsx"
output_dir <- "H:/Quina_valleys/output/03_technical_consistency"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_scraper_group <- function(path, sheet, group_name) {
  read_excel(path, sheet = sheet) |>
    mutate(Group = group_name) |>
    select(Group, all_of(variables))
}

scraper_data <- bind_rows(
  read_scraper_group(sc_path, "Quina scraper", "SC_Quina"),
  read_scraper_group(lt_path, "Quina scraper", "LT_Quina"),
  read_scraper_group(lt_path, "Ordinary scraper", "LT_Ordinary")
) |>
  mutate(
    Group = factor(Group, levels = c("SC_Quina", "LT_Quina", "LT_Ordinary")),
    across(all_of(variables), as.numeric)
  )

complete_data <- scraper_data |>
  filter(if_all(all_of(variables), ~ !is.na(.x)))

cat("Sample size before removing missing values:\n")
print(table(scraper_data$Group))

cat("\nSample size used in multivariate analyses:\n")
print(table(complete_data$Group))

analysis_matrix <- complete_data |>
  select(all_of(variables)) |>
  scale() |>
  as.matrix()

euclidean_distance <- dist(analysis_matrix, method = "euclidean")

## Overall PERMANOVA
permanova_result <- adonis2(
  euclidean_distance ~ Group,
  data = complete_data,
  permutations = 999
)

cat("\nOverall PERMANOVA result:\n")
print(permanova_result)

write.csv(
  as.data.frame(permanova_result),
  file = file.path(output_dir, "permanova_overall.csv")
)

## Pairwise post-hoc PERMANOVA with Benjamini-Hochberg adjusted p-values
pairwise_permanova <- function(data, scaled_matrix, group_col = "Group",
                               permutations = 999,
                               p_adjust_method = "BH") {
  groups <- levels(droplevels(data[[group_col]]))
  group_pairs <- combn(groups, 2, simplify = FALSE)

  results <- lapply(group_pairs, function(pair) {
    pair_rows <- data[[group_col]] %in% pair
    pair_data <- data[pair_rows, , drop = FALSE]
    pair_data[[group_col]] <- droplevels(pair_data[[group_col]])

    pair_distance <- dist(scaled_matrix[pair_rows, , drop = FALSE],
                          method = "euclidean")
    pair_model <- adonis2(
      pair_distance ~ Group,
      data = pair_data,
      permutations = permutations
    )

    data.frame(
      Comparison = paste(pair, collapse = " vs "),
      Df = pair_model$Df[1],
      SumOfSqs = pair_model$SumOfSqs[1],
      R2 = pair_model$R2[1],
      F = pair_model$F[1],
      p_value = pair_model$`Pr(>F)`[1],
      stringsAsFactors = FALSE
    )
  })

  bind_rows(results) |>
    mutate(p_adjusted = p.adjust(p_value, method = p_adjust_method))
}

posthoc_result <- pairwise_permanova(
  complete_data,
  analysis_matrix,
  permutations = 999,
  p_adjust_method = "BH"
)

cat("\nPairwise post-hoc PERMANOVA result:\n")
print(posthoc_result)

write.csv(
  posthoc_result,
  file = file.path(output_dir, "permanova_posthoc_pairwise.csv"),
  row.names = FALSE
)

## ----------------------------------------------------------------------------
## Multivariate dispersion (PERMDISP, betadisper)
## A significant PERMANOVA can stem from differences in group location
## (centroids), within-group dispersion, or both. betadisper isolates the
## dispersion component so the PERMANOVA result is interpreted correctly --
## important here given the unbalanced group sizes.
## ----------------------------------------------------------------------------
dispersion <- betadisper(euclidean_distance, complete_data$Group)
dispersion_anova <- anova(dispersion)
dispersion_permutest <- permutest(dispersion, permutations = 999,
                                  pairwise = TRUE)

cat("\nMultivariate dispersion (betadisper) - ANOVA:\n")
print(dispersion_anova)

cat("\nMultivariate dispersion (betadisper) - permutation test:\n")
print(dispersion_permutest)

dispersion_distances <- data.frame(
  Group = complete_data$Group,
  DistanceToCentroid = dispersion$distances
)

write.csv(
  as.data.frame(dispersion_anova),
  file = file.path(output_dir, "betadisper_anova.csv")
)

write.csv(
  dispersion_distances,
  file = file.path(output_dir, "betadisper_distances.csv"),
  row.names = FALSE
)

## ----------------------------------------------------------------------------
## Shared plotting elements
## ----------------------------------------------------------------------------
ordination_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649",
                                 margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

make_centroids <- function(scores, x_var, y_var) {
  scores |>
    group_by(Group) |>
    summarise(
      x_centroid = mean(.data[[x_var]], na.rm = TRUE),
      y_centroid = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )
}

make_spokes <- function(scores, centroids, x_var, y_var) {
  dplyr::left_join(scores, centroids, by = "Group")
}

make_convex_hulls <- function(scores, x_var, y_var) {
  scores |>
    group_by(Group) |>
    filter(n() >= 3) |>
    group_modify(function(group_data, group_key) {
      hull_rows <- chull(group_data[[x_var]], group_data[[y_var]])
      hull_data <- group_data[hull_rows, , drop = FALSE]
      bind_rows(hull_data, hull_data[1, , drop = FALSE])
    }) |>
    ungroup()
}

## ----------------------------------------------------------------------------
## PCA ordination
## On z-scored variables prcomp() is a PCA of the correlation matrix -- the
## metric equivalent of the previous PCoA. The ordination shows group convex
## hulls and within-group spokes to each centroid; the variable loadings are
## visualised separately below.
## ----------------------------------------------------------------------------
pca_result <- prcomp(analysis_matrix, center = TRUE, scale. = FALSE)
pca_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

pca_scores <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Group = complete_data$Group
)

pca_loadings <- data.frame(
  Variable = rownames(pca_result$rotation),
  PC1 = pca_result$rotation[, 1],
  PC2 = pca_result$rotation[, 2],
  row.names = NULL
)

write.csv(pca_scores, file.path(output_dir, "pca_scores.csv"),
          row.names = FALSE)
write.csv(pca_loadings, file.path(output_dir, "pca_loadings.csv"),
          row.names = FALSE)

pca_centroids <- make_centroids(pca_scores, "PC1", "PC2")
pca_spokes <- make_spokes(pca_scores, pca_centroids, "PC1", "PC2")
pca_hulls <- make_convex_hulls(pca_scores, "PC1", "PC2")

pca_biplot <- ggplot(
  pca_scores,
  aes(x = PC1, y = PC2, color = Group)
) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4,
             linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4,
             linetype = "dashed") +
  geom_polygon(
    data = pca_hulls,
    aes(x = PC1, y = PC2, fill = Group, group = Group),
    alpha = 0.12, color = NA, inherit.aes = FALSE
  ) +
  geom_path(
    data = pca_hulls,
    aes(x = PC1, y = PC2, color = Group, group = Group),
    linewidth = 0.45, alpha = 0.65, inherit.aes = FALSE
  ) +
  geom_segment(
    data = pca_spokes,
    aes(x = PC1, y = PC2, xend = x_centroid, yend = y_centroid, color = Group),
    linewidth = 0.25, alpha = 0.35, inherit.aes = FALSE
  ) +
  geom_point(size = 1.85, alpha = 0.8, shape = 16) +
  geom_point(
    data = pca_centroids,
    aes(x = x_centroid, y = y_centroid, color = Group),
    shape = 21, fill = "white", size = 4, stroke = 1.1, inherit.aes = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_fills) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(
    x = paste0("PC1 (", round(pca_variance[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_variance[2], 1), "%)"),
    color = "Group",
    fill = "Group"
  ) +
  coord_equal() +
  ordination_theme +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(size = 3.2, alpha = 1, shape = 16)
    )
  )

ggsave(
  filename = file.path(output_dir, "pca_ordination_scraper_groups.png"),
  plot = pca_biplot,
  width = 7.6,
  height = 5.8,
  dpi = 300
)

print(pca_biplot)

## ----------------------------------------------------------------------------
## PCA variable loadings (visualised separately from the ordination)
## ----------------------------------------------------------------------------
pca_loadings_long <- pca_loadings |>
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "PC",
    values_to = "Loading"
  ) |>
  mutate(
    PC = factor(PC, levels = c("PC1", "PC2")),
    Variable = factor(Variable, levels = rev(variables)),
    Sign = ifelse(Loading >= 0, "Positive", "Negative")
  )

pca_loadings_plot <- ggplot(
  pca_loadings_long,
  aes(x = Loading, y = Variable, fill = Sign)
) +
  geom_col(width = 0.7, color = "#303238", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  facet_wrap(~ PC) +
  scale_fill_manual(values = c(Positive = "#6BA8CE", Negative = "#E07C90")) +
  scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
  labs(
    subtitle = "PCA variable loadings",
    x = "Loading",
    y = NULL,
    fill = NULL
  ) +
  ordination_theme +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")

ggsave(
  filename = file.path(output_dir, "pca_loadings.png"),
  plot = pca_loadings_plot,
  width = 7.6,
  height = 4.6,
  dpi = 300
)

print(pca_loadings_plot)

## ----------------------------------------------------------------------------
## Per-variable distribution by group (raw, unscaled values)
## Shows which individual metrics drive the multivariate group separation.
## ----------------------------------------------------------------------------
variable_long <- complete_data |>
  select(Group, all_of(variables)) |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "Variable",
    values_to = "Value"
  ) |>
  mutate(Variable = factor(Variable, levels = variables))

## ----------------------------------------------------------------------------
## Per-variable group comparisons
##   Non-parametric (bounded indices / counts): Kruskal-Wallis omnibus
##     + Dunn post-hoc (Bonferroni)         -> Ave_GIUR, N_Scar, Ave_RG
##   Unequal-variance continuous metrics: Welch's ANOVA omnibus
##     + pairwise Welch t-tests (Bonferroni) -> Retouch_length_index,
##                                              Thickness, Edge_Angle
##   (3 groups, so the omnibus for the Welch set is Welch's ANOVA, not a t-test.)
## ----------------------------------------------------------------------------
kw_vars    <- c("Ave_GIUR", "N_Scar", "Ave_RG")
welch_vars <- c("Retouch_length_index", "Thickness", "Edge_Angle")

kw_long <- variable_long |>
  filter(Variable %in% kw_vars) |>
  mutate(Variable = droplevels(Variable))
welch_long <- variable_long |>
  filter(Variable %in% welch_vars) |>
  mutate(Variable = droplevels(Variable))

kw_omnibus <- kw_long |>
  group_by(Variable) |>
  kruskal_test(Value ~ Group) |>
  ungroup()
kw_posthoc <- kw_long |>
  group_by(Variable) |>
  dunn_test(Value ~ Group, p.adjust.method = "bonferroni") |>
  ungroup()

welch_omnibus <- welch_long |>
  group_by(Variable) |>
  welch_anova_test(Value ~ Group) |>
  ungroup()
welch_posthoc <- welch_long |>
  group_by(Variable) |>
  pairwise_t_test(Value ~ Group, pool.sd = FALSE,
                  p.adjust.method = "bonferroni") |>
  ungroup()

cat("\nKruskal-Wallis omnibus (Ave_GIUR, N_Scar, Ave_RG):\n")
print(as.data.frame(kw_omnibus))
cat("\nDunn post-hoc, Bonferroni-adjusted:\n")
print(as.data.frame(kw_posthoc))
cat("\nWelch's ANOVA omnibus (Retouch_length_index, Thickness, Edge_Angle):\n")
print(as.data.frame(welch_omnibus))
cat("\nPairwise Welch t-tests post-hoc, Bonferroni-adjusted:\n")
print(as.data.frame(welch_posthoc))

write.csv(kw_omnibus,
          file.path(output_dir, "kruskal_omnibus.csv"), row.names = FALSE)
write.csv(kw_posthoc,
          file.path(output_dir, "dunn_posthoc_bonferroni.csv"), row.names = FALSE)
write.csv(welch_omnibus,
          file.path(output_dir, "welch_anova_omnibus.csv"), row.names = FALSE)
write.csv(welch_posthoc,
          file.path(output_dir, "welch_pairwise_t_bonferroni.csv"), row.names = FALSE)

## Format the post-hoc comparisons as bracket annotations, with per-facet y
## positions because the boxplots use free_y scales.
posthoc_brackets <- bind_rows(
  kw_posthoc    |> transmute(Variable, group1, group2, p.adj, p.adj.signif),
  welch_posthoc |> transmute(Variable, group1, group2, p.adj, p.adj.signif)
) |>
  mutate(Variable = factor(as.character(Variable), levels = variables))

facet_ranges <- variable_long |>
  group_by(Variable) |>
  summarise(
    ymax = max(Value, na.rm = TRUE),
    yrange = diff(range(Value, na.rm = TRUE)),
    .groups = "drop"
  )

posthoc_brackets <- posthoc_brackets |>
  group_by(Variable) |>
  mutate(step = row_number()) |>
  ungroup() |>
  left_join(facet_ranges, by = "Variable") |>
  mutate(y.position = ymax + yrange * (0.06 + 0.10 * step))

variable_boxplots <- ggplot(
  variable_long,
  aes(x = Group, y = Value)
) +
  geom_jitter(aes(color = Group), width = 0.31, height = 0,
              size = 1.4, alpha = 0.6, shape = 16) +
  geom_boxplot(color = "black", fill = NA, width = 0.62,
               linewidth = 0.6, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2,
               color = "black") +
  ggpubr::stat_pvalue_manual(
    posthoc_brackets,
    label = "p.adj.signif",
    y.position = "y.position",
    tip.length = 0.012,
    bracket.size = 0.4,
    label.size = 3,
    color = "#202124"
  ) +
  facet_wrap(
    ~ Variable, scales = "free_y", ncol = 3,
    labeller = as_labeller(function(x) gsub("_", " ", x))
  ) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    subtitle = "Pairwise post-hoc, Bonferroni-adjusted (ns / * / ** / *** / ****)",
    x = NULL,
    y = NULL
  ) +
  ordination_theme +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = file.path(output_dir, "variable_boxplots.png"),
  plot = variable_boxplots,
  width = 8.4,
  height = 6.8,
  dpi = 300
)

print(variable_boxplots)
