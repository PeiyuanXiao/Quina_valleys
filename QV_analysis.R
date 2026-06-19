## Quina Valley and Longtan scraper comparison
## Groups:
##   SC_Quina     = Quina scraper from Quina_scraper_surface.xlsx
##   LT_Quina     = Quina scraper from Longtan_lithic_tools.xlsx
##   LT_Ordinary  = Ordinary scraper from Longtan_lithic_tools.xlsx

required_packages <- c("readxl", "dplyr", "ggplot2", "vegan", "MASS")
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
library(ggplot2)
library(vegan)

set.seed(20260618)

variables <- c(
  "Thickness",
  "Retouch_length_index",
  "Ave_GIUR",
  "N_Scar",
  "Ave_RG",
  "Edge_Angle"
)

group_colors <- c(
  SC_Quina = "#0072B2",
  LT_Quina = "#D55E00",
  LT_Ordinary = "#009E73"
)

group_fills <- c(
  SC_Quina = "#0072B2",
  LT_Quina = "#D55E00",
  LT_Ordinary = "#009E73"
)

sc_path <- "H:/Quina_valleys/Quina_scraper_surface.xlsx"
lt_path <- "H:/Quina_valleys/Longtan_lithic_tools.xlsx"
output_dir <- "H:/Quina_valleys/outputs"

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

## Shared plotting helpers
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

make_centroids <- function(scores, x_var, y_var) {
  scores |>
    group_by(Group) |>
    summarise(
      x_centroid = mean(.data[[x_var]], na.rm = TRUE),
      y_centroid = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )
}

make_ordination_plot <- function(scores, x_var, y_var, x_label, y_label,
                                 title, output_file) {
  hulls <- make_convex_hulls(scores, x_var, y_var)
  centroids <- make_centroids(scores, x_var, y_var)

  ordination_plot <- ggplot(
    scores,
    aes(x = .data[[x_var]], y = .data[[y_var]], color = Group)
  ) +
    geom_polygon(
      data = hulls,
      aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        fill = Group,
        group = Group
      ),
      alpha = 0.12,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_path(
      data = hulls,
      aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        color = Group,
        group = Group
      ),
      linewidth = 0.45,
      alpha = 0.65,
      inherit.aes = FALSE
    ) +
    geom_point(size = 1.85, alpha = 0.78, shape = 16) +
    geom_point(
      data = centroids,
      aes(x = x_centroid, y = y_centroid, color = Group),
      shape = 21,
      fill = "white",
      size = 4,
      stroke = 1.1,
      inherit.aes = FALSE
    ) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_fills) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Group",
      fill = "Group"
    ) +
    coord_equal() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
      axis.ticks = element_line(color = "#202124", linewidth = 0.35),
      axis.ticks.length = grid::unit(2.5, "pt"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 15,
        margin = margin(b = 8)
      ),
      axis.title = element_text(size = 12),
      axis.text = element_text(color = "#303238"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.key = element_blank(),
      plot.background = element_rect(color = NA, fill = "white"),
      panel.background = element_rect(color = NA, fill = "white")
    ) +
    guides(
      fill = "none",
      color = guide_legend(
        override.aes = list(size = 3.2, alpha = 1, shape = 16)
      )
    )

  ggsave(
    filename = file.path(output_dir, output_file),
    plot = ordination_plot,
    width = 7.2,
    height = 5.4,
    dpi = 300
  )

  ordination_plot
}

## PCoA visualization
pcoa_result <- cmdscale(euclidean_distance, eig = TRUE, k = 2)
positive_eigenvalues <- pcoa_result$eig[pcoa_result$eig > 0]
axis_variance <- positive_eigenvalues / sum(positive_eigenvalues) * 100

pcoa_scores <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Group = complete_data$Group
)

write.csv(
  pcoa_scores,
  file = file.path(output_dir, "pcoa_scores.csv"),
  row.names = FALSE
)

pcoa_plot <- make_ordination_plot(
  scores = pcoa_scores,
  x_var = "PCoA1",
  y_var = "PCoA2",
  x_label = paste0("PCoA1 (", round(axis_variance[1], 1), "%)"),
  y_label = paste0("PCoA2 (", round(axis_variance[2], 1), "%)"),
  title = "PCoA of Scraper Metric Variables",
  output_file = "pcoa_scraper_groups.png"
)

print(pcoa_plot)

## LDA visualization
lda_data <- data.frame(
  Group = complete_data$Group,
  analysis_matrix,
  check.names = FALSE
)

lda_model <- MASS::lda(Group ~ ., data = lda_data)
lda_prediction <- predict(lda_model)
lda_variance <- lda_model$svd^2 / sum(lda_model$svd^2) * 100

lda_scores <- data.frame(
  LD1 = lda_prediction$x[, 1],
  LD2 = lda_prediction$x[, 2],
  Group = complete_data$Group
)

write.csv(
  lda_scores,
  file = file.path(output_dir, "lda_scores.csv"),
  row.names = FALSE
)

lda_plot <- make_ordination_plot(
  scores = lda_scores,
  x_var = "LD1",
  y_var = "LD2",
  x_label = paste0("LD1 (", round(lda_variance[1], 1), "%)"),
  y_label = paste0("LD2 (", round(lda_variance[2], 1), "%)"),
  title = "LDA of Scraper Metric Variables",
  output_file = "lda_scraper_groups.png"
)

print(lda_plot)
