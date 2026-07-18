# QV_analysis.R
# Technical LOCATION comparison of Quina-scraper groups (centroids); companion to
# the dispersion analysis in QV_dispersion_LT_vs_SC.R.
#
# Groups: SC_Quina (surface), LT_Quina (Longtan), LT_Ordinary (Longtan yardstick).
# 6 z-scored technical variables -> Euclidean distance.
#
# Pipeline:
#   1. Load + z-score the 6 technical variables (complete cases, 3 groups).
#   2. Overall + pairwise (BH) PERMANOVA on group centroids.
#   3. Per-variable location tests: Kruskal-Wallis + Dunn and Welch ANOVA + t
#      (Bonferroni), drawn as boxplots.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#   - data/Longtan_lithic_tools.xlsx (sheets "Quina scraper", "Ordinary scraper")
#
# Output:
#   - output/03_technical_consistency/variable_boxplots.png

# ==============================================================================
# Setup
# ==============================================================================

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

# ==============================================================================
# Global parameters
# ==============================================================================

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

sc_path <- here::here("data", "Quina_scraper_surface.xlsx")
lt_path <- here::here("data", "Longtan_lithic_tools.xlsx")
output_dir <- here::here("output", "03_technical_consistency")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. Load + prepare data
# ==============================================================================

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

# ==============================================================================
# 2. PERMANOVA (group centroids)
# ==============================================================================

# --- Overall PERMANOVA ---
permanova_result <- adonis2(
  euclidean_distance ~ Group,
  data = complete_data,
  permutations = 999
)

cat("\nOverall PERMANOVA result:\n")
print(permanova_result)


# --- Pairwise post-hoc PERMANOVA (Benjamini-Hochberg) ---
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


# Dispersion (PERMDISP) for these same groups lives in QV_dispersion_LT_vs_SC.R
# (Block A); this script covers group LOCATION only.

# ==============================================================================
# 3. Per-variable location tests
# ==============================================================================

# --- Shared plotting theme ---
plot_theme <- theme_minimal(base_size = 13) +
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

# --- Per-variable distributions (raw values) ---
variable_long <- complete_data |>
  select(Group, all_of(variables)) |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "Variable",
    values_to = "Value"
  ) |>
  mutate(Variable = factor(Variable, levels = variables))

# --- Per-variable tests: KW + Dunn (indices/counts); Welch ANOVA + t (continuous) ---
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
  plot_theme +
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
