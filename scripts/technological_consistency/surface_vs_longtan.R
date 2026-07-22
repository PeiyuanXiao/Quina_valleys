# surface_vs_longtan.R
# Technical consistency of surface-collected (SC) Quina scrapers vs excavated
# Longtan (LT) Quina scrapers, with LT ordinary scrapers as a yardstick.
#
# Two complementary questions on the technical variables:
#   Part 1 LOCATION   -- are the group centroids different?  (PERMANOVA)
#   Part 2 DISPERSION -- are the within-group spreads different?  (PERMDISP + CV/robust)
# Exploratory: rank by effect size, not p<0.05. Merged from the former QV_analysis.R
# + QV_dispersion_LT_vs_SC.R. Full dispersion guardrails -> _GUARDRAILS.txt.
#
# Pipeline:
#   Part 1  PERMANOVA (overall + pairwise BH) + per-variable KW/Welch + boxplots.
#   Part 2  A PERMDISP; B1 CV / KL-MSLRT; B2 Fligner (+ logit/sqrt); C sensitivity;
#           D cross-variable summary.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#   - data/Longtan_lithic_tools.xlsx (sheets "Quina scraper", "Ordinary scraper")
#
# Output:
#   - output/technological_consistency/variable_boxplots.png
#   - output/technological_consistency/dispersion_LT_vs_SC/ (figures + _GUARDRAILS.txt)

# ==============================================================================
# Setup
# ==============================================================================

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan",
                       "rstatix", "ggpubr", "cvequality")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install the following R packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(rstatix)
library(here)
library(grid)
library(ggpubr)
library(cvequality)

set.seed(2226)
B_BOOT   <- 5000   # bootstrap / permutation replicates
MSLR_NR  <- 1e5    # Monte-Carlo iterations for mslr_test (Krishnamoorthy-Lee)
# ==============================================================================
# Part 1 -- Location (parameters + centroid analysis)
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

sc_path <- here("data", "Quina_scraper_surface.xlsx")
lt_path <- here("data", "Longtan_lithic_tools.xlsx")
output_dir <- here("output", "technological_consistency")

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
  permutations = 9999
)

cat("\nOverall PERMANOVA result:\n")
print(permanova_result)


# --- Pairwise post-hoc PERMANOVA (Benjamini-Hochberg) ---
pairwise_permanova <- function(data, scaled_matrix, group_col = "Group",
                               permutations = 9999,
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
  permutations = 9999,
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
    axis.ticks.length = unit(2.5, "pt"),
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


# Format the post-hoc comparisons as bracket annotations, with per-facet y
# positions because the boxplots use free_y scales.
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
  stat_pvalue_manual(
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

# ==============================================================================
# Part 2 -- Dispersion (parameters, helpers + analysis)
# ==============================================================================

# Re-seed so Part 2's permutations / bootstraps reproduce the standalone run
# (Part 1's PERMANOVA permutations above advanced the shared RNG stream).
set.seed(2226)

proj_dir <- here()
out_root <- file.path(proj_dir, "output", "technological_consistency")
base_dir <- file.path(out_root, "dispersion_LT_vs_SC")
sub <- list(mv  = file.path(base_dir, "multivariate_permdisp"),
            cv  = file.path(base_dir, "cv_dimensional"),
            rob = file.path(base_dir, "robust_reduction"),
            sen = file.path(base_dir, "sensitivity"))
for (d in c(base_dir, unlist(sub))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- variables / families --------------------------------------------------
cv_vars      <- c("Length", "Width", "Thickness", "Mass", "Edge_Angle")     # CV family
bounded_vars <- c("Ave_GIUR", "Retouch_length_index")                       # [0,1] indices
count_vars   <- c("N_Scar", "Ave_RG")                                       # counts / count-like
robust_vars  <- c(bounded_vars, count_vars)
need_vars    <- c(cv_vars, robust_vars)
tech6        <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
                  "N_Scar", "Ave_RG", "Edge_Angle")  # the established technical space (Block A)
grp_levels   <- c("SC_Quina", "LT_Quina", "LT_Ordinary")

guardrails <- c(
  "DISPERSION (variability) comparison SC vs Longtan Quina scrapers -- EXPLORATORY.",
  "* Rank by effect size (dispersion ratio / mean distance to centroid), not p<0.05.",
  "* Dispersion depends on the mean -> every spread stat is reported next to the mean;",
  "  if SC and LT_Quina means differ, 'spread difference' is confounded with location.",
  "* CV only for ratio/dimensional vars (+Edge_Angle by convention; interval scale).",
  "  Bounded [0,1] indices and counts -> robust spread (MAD/IQR, Fano) + Fligner-Killeen.",
  "* Reduction-indicator dispersion = range of reduction stages sampled; SC>LT is",
  "  consistent with time-averaging but is NOT transmission-fidelity evidence.",
  "* Focus = SC_Quina vs LT_Quina; LT_Ordinary is a yardstick only.",
  "* Surface weathering adds non-behavioural measurement spread (see sensitivity)."
)
writeLines(guardrails, file.path(base_dir, "_GUARDRAILS.txt"))

# ---- shared visual style (QV idiom) ----------------------------------------
ordination_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649", margin = margin(b = 8)),
    plot.caption = element_text(size = 7, color = "#454649", hjust = 0),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "right", legend.title = element_text(face = "bold"), legend.key = element_blank(),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )
corr_theme <- ordination_theme +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.grid.major.x = element_blank())

guard_caption <- "EXPLORATORY: rank by effect size, not p<0.05. Dispersion depends on the mean (reported alongside)."

make_centroids <- function(scores) scores |>
  group_by(Group) |>
  summarise(x_centroid = mean(PC1), y_centroid = mean(PC2), .groups = "drop")
make_hulls <- function(scores) scores |>
  group_by(Group) |> filter(n() >= 3) |>
  group_modify(function(g, k) { h <- chull(g$PC1, g$PC2); bind_rows(g[h, ], g[h[1], ]) }) |>
  ungroup()

# ==============================================================================
# Helpers (base-R statistics)
# ==============================================================================
finite <- function(x) x[is.finite(x)]
cv_raw    <- function(x) { x <- finite(x); sd(x) / mean(x) }
cv_corr   <- function(x) { x <- finite(x); n <- length(x); (sd(x) / mean(x)) * (1 + 1 / (4 * n)) }  # Sokal-Rohlf
fano      <- function(x) { x <- finite(x); var(x) / mean(x) }

# bootstrap percentile CI of a one-sample statistic
boot_stat_ci <- function(x, FUN, B = B_BOOT) {
  x <- finite(x)
  rr <- replicate(B, FUN(sample(x, replace = TRUE)))
  unname(quantile(rr, c(0.025, 0.975), na.rm = TRUE))
}
# bootstrap percentile CI of a ratio FUN(x_sc)/FUN(x_lt); skip if a mean ~ 0
boot_ratio_ci <- function(x_sc, x_lt, FUN, B = B_BOOT, mean_guard = FALSE) {
  x_sc <- finite(x_sc); x_lt <- finite(x_lt)
  if (mean_guard && (abs(mean(x_sc)) < 1e-8 || abs(mean(x_lt)) < 1e-8)) return(c(NA_real_, NA_real_))
  rr <- replicate(B, FUN(sample(x_sc, replace = TRUE)) / FUN(sample(x_lt, replace = TRUE)))
  unname(quantile(rr, c(0.025, 0.975), na.rm = TRUE))
}
# CV-equality test between two groups: Krishnamoorthy & Lee (2014) modified
# signed-likelihood-ratio test (MSLRT), via mslr_test
# (Marwick & Krishnamoorthy 2019). Returns the MSLRT statistic + p.
# mslr_test is Monte-Carlo; its RNG use is INSULATED (save/restore .Random.seed +
# a fixed local seed) so every bootstrap CI in the rest of the script is unaffected
# and each variable's MSLRT is itself reproducible regardless of call order.
cv_equal_test <- function(x_sc, x_lt) {
  x_sc <- finite(x_sc); x_lt <- finite(x_lt)
  vals <- c(x_sc, x_lt)
  grp  <- rep(c("SC_Quina", "LT_Quina"), c(length(x_sc), length(x_lt)))
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv))
  set.seed(2226)                                                     # local, isolated
  ml <- mslr_test(nr = MSLR_NR, x = vals, y = grp)
  list(stat = unname(ml$MSLRT), p = unname(ml$p_value))
}
# Fligner-Killeen p for a 2-group contrast
fligner_pair <- function(a, b) {
  a <- finite(a); b <- finite(b)
  g <- factor(rep(c("SC", "LT"), c(length(a), length(b))))
  fligner.test(c(a, b), g)$p.value
}

# ==============================================================================
# Load + schema check
# ==============================================================================
cat("\n########## SCHEMA CHECK / PER-VARIABLE N ##########\n")
read_group <- function(path, sheet, g, has_site) {
  df <- read_excel(path, sheet = sheet)
  miss <- setdiff(need_vars, names(df))
  if (length(miss) > 0)
    stop("Group ", g, " [", sheet, "] is MISSING required columns: ",
         paste(miss, collapse = ", "), "\nAvailable columns: ",
         paste(names(df), collapse = ", "))
  df$Group   <- g
  df$Site_ID <- if (has_site) trimws(as.character(df$Site_ID)) else NA_character_
  df |> mutate(across(all_of(need_vars), as.numeric)) |>
    select(Group, Site_ID, all_of(need_vars))
}
dat <- bind_rows(
  read_group(sc_path, "Quina scraper",    "SC_Quina",    TRUE),
  read_group(lt_path, "Quina scraper",    "LT_Quina",    FALSE),
  read_group(lt_path, "Ordinary scraper", "LT_Ordinary", FALSE)
) |> mutate(Group = factor(Group, levels = grp_levels))

# per-variable per-group complete-case n (each variable on its own complete-case)
n_tbl <- dat |>
  pivot_longer(all_of(need_vars), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |>
  count(Variable, Group) |>
  pivot_wider(names_from = Group, values_from = n, values_fill = 0) |>
  mutate(Variable = factor(Variable, levels = need_vars)) |> arrange(Variable)
cat("\nPer-variable complete-case n by group:\n"); print(as.data.frame(n_tbl), row.names = FALSE)
cat(sprintf(paste0("\nCV-equality significance test: mslr_test",
                   " (Krishnamoorthy-Lee 2014 MSLRT, nr = %g)\n"), MSLR_NR))

# focus / proximal-exclusion masks (Block C)
proximal_ids <- c("LT", "THC")
n_prox <- sum(dat$Group == "SC_Quina" & dat$Site_ID %in% proximal_ids)
cat(sprintf("SC_Quina proximal to LT/THC: %d of %d (%.0f%% of all SC); SC excl-proximal n = %d\n",
            n_prox, sum(dat$Group == "SC_Quina"), 100 * n_prox / sum(dat$Group == "SC_Quina"),
            sum(dat$Group == "SC_Quina") - n_prox))

# ==============================================================================
# A. Multivariate dispersion (PERMDISP)
# ==============================================================================
# Runs betadisper/permutest on z-scored {tech6} -> Euclidean for a given data
# frame; returns per-group mean distance-to-centroid + pairwise permuted p.
run_permdisp <- function(df, outdir, prefix, title) {
  mvd <- df |> filter(if_all(all_of(tech6), is.finite)) |> mutate(Group = droplevels(Group))
  cat("\n[", title, "] N per group (complete-case on tech6):\n", sep = ""); print(table(mvd$Group))
  mat <- scale(as.matrix(mvd[, tech6]))
  d   <- dist(mat, method = "euclidean")
  bd  <- betadisper(d, mvd$Group)
  pt  <- permutest(bd, permutations = 9999, pairwise = TRUE)

  means <- tapply(bd$distances, mvd$Group, mean)
  dist_df <- data.frame(Group = mvd$Group, DistanceToCentroid = bd$distances)
  overall_p <- pt$tab$`Pr(>F)`[1]
  pw <- pt$pairwise$permuted

  cat("Mean distance to centroid (= dispersion size):\n"); print(round(means, 3))
  cat("permutest overall p =", signif(overall_p, 3), "\n")
  cat("pairwise permuted p:\n"); print(round(pw, 3))

  # plot 1: distance-to-centroid box + violin
  p1 <- ggplot(dist_df, aes(Group, DistanceToCentroid, fill = Group, color = Group)) +
    geom_violin(alpha = 0.18, color = NA, width = 0.9) +
    geom_boxplot(fill = NA, color = "black", width = 0.42, linewidth = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.12, height = 0, size = 1.3, alpha = 0.55, shape = 16) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.6, fill = "white", color = "black") +
    scale_fill_manual(values = group_colors) + scale_color_manual(values = group_colors) +
    labs(title = title, subtitle = sprintf("PERMDISP overall p = %s (larger mean = more dispersed)",
                                            fmt <- ifelse(overall_p < 0.001, "<0.001", signif(overall_p, 3))),
         x = NULL, y = "Distance to group centroid", caption = guard_caption) +
    ordination_theme + theme(legend.position = "none")
  ggsave(file.path(outdir, paste0(prefix, "_distance_boxplot.png")), p1,
         width = 7.0, height = 5.2, dpi = 300)

  # plot 2: PCA (= PCoA of Euclidean) ordination with convex hulls
  pca <- prcomp(mat, center = TRUE, scale. = FALSE)
  vexp <- pca$sdev^2 / sum(pca$sdev^2) * 100
  scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Group = mvd$Group)
  loadings <- data.frame(Variable = rownames(pca$rotation),
                         PC1 = pca$rotation[, 1], PC2 = pca$rotation[, 2],
                         row.names = NULL)
  cent <- make_centroids(scores); hull <- make_hulls(scores)
  p2 <- ggplot(scores, aes(PC1, PC2, color = Group)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_polygon(data = hull, aes(fill = Group, group = Group), alpha = 0.12, color = NA) +
    geom_path(data = hull, aes(group = Group), linewidth = 0.45, alpha = 0.65) +
    geom_point(size = 1.8, alpha = 0.8, shape = 16) +
    geom_point(data = cent, aes(x_centroid, y_centroid, color = Group),
               shape = 21, fill = "white", size = 4, stroke = 1.1, inherit.aes = FALSE) +
    scale_color_manual(values = group_colors) + scale_fill_manual(values = group_colors) +
    labs(title = title, subtitle = "Convex hulls: wider hull = more dispersed group",
         x = paste0("PC1 (", round(vexp[1], 1), "%)"), y = paste0("PC2 (", round(vexp[2], 1), "%)"),
         caption = guard_caption) +
    coord_equal() + ordination_theme + guides(fill = "none")
  ggsave(file.path(outdir, paste0(prefix, "_pca_ordination.png")), p2, width = 7.6, height = 5.8, dpi = 300)

  list(means = means, overall_p = overall_p, pairwise = pw,
       ratio_SC_LT = unname(means["SC_Quina"] / means["LT_Quina"]),
       # tidy data behind the ordination, for the figure scripts (see saveRDS below)
       n_by_group = table(mvd$Group),
       pca_scores = scores, pca_loadings = loadings, var_explained = vexp)
}

cat("\n########## BLOCK A: MULTIVARIATE PERMDISP ##########\n")
permA <- run_permdisp(dat, sub$mv, "permdisp_all",
                      "Technical-space dispersion (PERMDISP): SC vs Longtan")

# ==============================================================================
# B1. CV family (ratio / dimensional variables)
# ==============================================================================
cat("\n########## BLOCK B1: CV (dimensional) ##########\n")
cv_group <- list(); cv_ratio <- list()
for (v in cv_vars) {
  for (g in grp_levels) {
    x <- finite(dat[[v]][dat$Group == g])
    ci <- boot_stat_ci(x, cv_corr)
    cv_group[[length(cv_group) + 1]] <- data.frame(
      variable = v, group = g, n = length(x), mean = mean(x), sd = sd(x),
      CV = cv_raw(x), CVstar = cv_corr(x), CVstar_lo = ci[1], CVstar_hi = ci[2],
      note = if (v == "Edge_Angle") "interval scale; CV by convention only" else "")
  }
  x_sc <- finite(dat[[v]][dat$Group == "SC_Quina"])
  x_lt <- finite(dat[[v]][dat$Group == "LT_Quina"])
  ratio <- cv_corr(x_sc) / cv_corr(x_lt)
  rci <- boot_ratio_ci(x_sc, x_lt, cv_corr, mean_guard = TRUE)
  ce  <- cv_equal_test(x_sc, x_lt)
  cv_ratio[[length(cv_ratio) + 1]] <- data.frame(
    variable = v, CVstar_SC = cv_corr(x_sc), CVstar_LT = cv_corr(x_lt),
    CVstar_ratio_SC_LT = ratio, ratio_lo = rci[1], ratio_hi = rci[2],
    KL_MSLRT = ce$stat, KL_p_CVequal = ce$p,
    mean_SC = mean(x_sc), mean_LT = mean(x_lt),
    note = if (v == "Edge_Angle") "interval scale; CV by convention only" else "")
}
cv_group <- bind_rows(cv_group); cv_ratio <- bind_rows(cv_ratio)
cat("\nCV* by group:\n");        print(cv_group, row.names = FALSE)
cat("\nCV* ratio SC:LT_Quina  (KL_p_CVequal = Krishnamoorthy-Lee MSLRT CV-equality test):\n")
print(cv_ratio, row.names = FALSE)

# KL MSLRT significance summary (exploratory; rank by CV* ratio, not p<0.05)
kl_sig <- cv_ratio$variable[which(cv_ratio$KL_p_CVequal < 0.05)]  # which() drops any NA p
cat(sprintf("\nKrishnamoorthy-Lee MSLRT (nr = %g): %d of %d CV variables reject equal-CV at alpha = 0.05%s\n",
            MSLR_NR, length(kl_sig), nrow(cv_ratio),
            if (length(kl_sig) > 0) paste0(" (", paste(kl_sig, collapse = ", "), ")") else ""))

# plot: CV* by group (point + bootstrap CI)
cvg_p <- ggplot(cv_group, aes(group, CVstar, color = group)) +
  geom_pointrange(aes(ymin = CVstar_lo, ymax = CVstar_hi), size = 0.55, linewidth = 0.7) +
  facet_wrap(~ factor(variable, levels = cv_vars), scales = "free_y", nrow = 1) +
  scale_color_manual(values = group_colors) +
  labs(title = "Corrected CV* by group (bootstrap 95% CI)",
       subtitle = "Edge_Angle: interval scale, CV by convention only",
       x = NULL, y = "CV* (Sokal-Rohlf corrected)", caption = guard_caption) +
  corr_theme + theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(sub$cv, "cv_by_group.png"), cvg_p, width = 10.5, height = 4.2, dpi = 300)

# forest plot: CV* ratio SC:LT_Quina
cvr_p <- ggplot(cv_ratio, aes(CVstar_ratio_SC_LT, factor(variable, levels = rev(cv_vars)))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#202124") +
  geom_errorbarh(aes(xmin = ratio_lo, xmax = ratio_hi), height = 0.22, color = "#454649") +
  geom_point(color = "#E07C90", size = 3) +
  scale_x_continuous(trans = "log2") +
  labs(title = "CV* ratio SC_Quina : LT_Quina (>1 = SC more variable)",
       subtitle = "log2 x-axis; dashed = equal dispersion", x = "CV* ratio (SC : LT_Quina)",
       y = NULL, caption = guard_caption) +
  corr_theme
ggsave(file.path(sub$cv, "cv_ratio_forest.png"), cvr_p, width = 7.2, height = 4.2, dpi = 300)

# ==============================================================================
# B2. Robust family (reduction indicators)
# ==============================================================================
cat("\n########## BLOCK B2: robust (reduction indicators) ##########\n")
# overall (3-group) + SC-vs-LT_Quina pairwise Fligner-Killeen dispersion tests
disp_tests <- function(value, group) {
  ok <- is.finite(value)
  value <- value[ok]; group <- droplevels(factor(group[ok]))
  d3 <- data.frame(Value = value, Group = group)
  fl_all <- fligner.test(Value ~ Group, data = d3)$p.value
  pair <- d3 |> filter(Group %in% c("SC_Quina", "LT_Quina")) |> mutate(Group = droplevels(Group))
  fl_pr <- fligner.test(Value ~ Group, data = pair)$p.value
  c(fligner_overall = fl_all, fligner_SC_LTq = fl_pr)
}

rob_group <- list(); rob_ratio <- list()
for (v in robust_vars) {
  fam <- if (v %in% bounded_vars) "bounded" else "count"
  x_sc <- finite(dat[[v]][dat$Group == "SC_Quina"])
  x_lt <- finite(dat[[v]][dat$Group == "LT_Quina"])
  for (g in grp_levels) {
    x <- finite(dat[[v]][dat$Group == g])
    rob_group[[length(rob_group) + 1]] <- data.frame(
      variable = v, family = fam, group = g, n = length(x),
      mean = mean(x), median = median(x), sd = sd(x),
      MAD = mad(x), IQR = IQR(x), variance = var(x),
      Fano = if (fam == "count") fano(x) else NA_real_)
  }
  # robust dispersion ratios SC:LT_Quina (+ bootstrap CI). mean reported for confound check.
  mad_r <- mad(x_sc) / mad(x_lt); mad_ci <- boot_ratio_ci(x_sc, x_lt, function(z) mad(z))
  iqr_r <- IQR(x_sc) / IQR(x_lt); iqr_ci <- boot_ratio_ci(x_sc, x_lt, function(z) IQR(z))
  tests_raw <- disp_tests(dat[[v]], dat$Group)
  row <- data.frame(
    variable = v, family = fam,
    MAD_ratio_SC_LT = mad_r, MAD_lo = mad_ci[1], MAD_hi = mad_ci[2],
    IQR_ratio_SC_LT = iqr_r, IQR_lo = iqr_ci[1], IQR_hi = iqr_ci[2],
    fligner_overall_p = tests_raw["fligner_overall"], fligner_SC_LTq_p = tests_raw["fligner_SC_LTq"],
    mean_SC = mean(x_sc), mean_LT = mean(x_lt), row.names = NULL)
  rob_ratio[[length(rob_ratio) + 1]] <- row
}
rob_group <- bind_rows(rob_group); rob_ratio <- bind_rows(rob_ratio)
cat("\nRobust group stats (dispersion next to mean):\n"); print(rob_group, row.names = FALSE)
cat("\nRobust ratios + dispersion-equality tests (SC vs LT_Quina):\n"); print(rob_ratio, row.names = FALSE)

# plot: per-variable distributions by group + Fligner pairwise p
rob_long <- dat |> select(Group, all_of(robust_vars)) |>
  pivot_longer(all_of(robust_vars), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |> mutate(Variable = factor(Variable, levels = robust_vars))
rob_lab <- rob_ratio |>
  transmute(Variable = factor(variable, levels = robust_vars),
            label = sprintf("Fligner(SC:LTq) p %s\nMAD ratio = %.2f",
                            ifelse(fligner_SC_LTq_p < 0.001, "<0.001", paste0("= ", signif(fligner_SC_LTq_p, 2))),
                            MAD_ratio_SC_LT))
rob_p <- ggplot(rob_long, aes(Group, Value, fill = Group, color = Group)) +
  geom_jitter(width = 0.22, height = 0, size = 1.2, alpha = 0.5, shape = 16) +
  geom_boxplot(fill = NA, color = "black", width = 0.55, linewidth = 0.55, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.2, fill = "white", color = "black") +
  geom_text(data = rob_lab, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = -0.06, vjust = 1.15, size = 2.9, color = "#202124", lineheight = 0.95) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2,
             labeller = as_labeller(function(x) gsub("_", " ", x))) +
  scale_fill_manual(values = group_colors) + scale_color_manual(values = group_colors) +
  labs(title = "Reduction-indicator distributions & spread by group",
       subtitle = "white diamond = mean (read spread next to location)", x = NULL, y = NULL,
       caption = guard_caption) +
  ordination_theme + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(sub$rob, "robust_boxplots.png"), rob_p, width = 8.6, height = 7.0, dpi = 300)

# ==============================================================================
# C. Independence sensitivity (drop SC pieces proximal to LT/THC)
# ==============================================================================
cat("\n########## BLOCK C: sensitivity (SC excl. LT/THC-proximal) ##########\n")
dat_excl <- dat |> filter(!(Group == "SC_Quina" & Site_ID %in% proximal_ids))

# headline dispersion ratio (SC:LT_Quina) by family -- reused for all/excl
headline_ratio <- function(d, v) {
  fam <- if (v %in% cv_vars) "CV" else if (v %in% bounded_vars) "bounded" else "count"
  x_sc <- finite(d[[v]][d$Group == "SC_Quina"]); x_lt <- finite(d[[v]][d$Group == "LT_Quina"])
  if (fam == "CV") {
    metric <- "CVstar_ratio"; ratio <- cv_corr(x_sc) / cv_corr(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, cv_corr, mean_guard = TRUE); p <- cv_equal_test(x_sc, x_lt)$p
  } else if (fam == "bounded") {
    metric <- "MAD_ratio"; ratio <- mad(x_sc) / mad(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, function(z) mad(z)); p <- fligner_pair(x_sc, x_lt)
  } else {
    metric <- "Fano_ratio"; ratio <- fano(x_sc) / fano(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, fano); p <- fligner_pair(x_sc, x_lt)
  }
  data.frame(variable = v, family = fam, metric = metric, n_SC = length(x_sc), n_LT = length(x_lt),
             ratio_SC_LT = ratio, lo = ci[1], hi = ci[2], equal_p = p,
             mean_SC = mean(x_sc), mean_LT = mean(x_lt), row.names = NULL)
}
all_h  <- bind_rows(lapply(need_vars, function(v) headline_ratio(dat,      v)))
excl_h <- bind_rows(lapply(need_vars, function(v) headline_ratio(dat_excl, v)))

# PERMDISP re-run on SC-excl
permC <- run_permdisp(dat_excl, sub$sen, "permdisp_excl",
                      "PERMDISP sensitivity: SC (excl. LT/THC) vs Longtan")

sens_tbl <- all_h |>
  transmute(variable, family, metric,
            ratio_SCall_LT = ratio_SC_LT, p_SCall = equal_p, mean_SCall = mean_SC) |>
  left_join(excl_h |> transmute(variable,
            ratio_SCexcl_LT = ratio_SC_LT, p_SCexcl = equal_p, mean_SCexcl = mean_SC),
            by = "variable") |>
  bind_rows(data.frame(variable = "PERMDISP_meandist", family = "multivariate", metric = "meandist_ratio",
            ratio_SCall_LT = permA$ratio_SC_LT, p_SCall = permA$overall_p, mean_SCall = NA_real_,
            ratio_SCexcl_LT = permC$ratio_SC_LT, p_SCexcl = permC$overall_p, mean_SCexcl = NA_real_))
cat("\nSensitivity side-by-side (each relative to LT_Quina):\n"); print(sens_tbl, row.names = FALSE)

# plot: SC-all vs SC-excl dispersion ratio per variable
sens_long <- sens_tbl |> filter(family != "multivariate") |>
  select(variable, ratio_SCall_LT, ratio_SCexcl_LT) |>
  pivot_longer(c(ratio_SCall_LT, ratio_SCexcl_LT), names_to = "SC_set", values_to = "ratio") |>
  mutate(SC_set = recode(SC_set, ratio_SCall_LT = "SC all", ratio_SCexcl_LT = "SC excl. LT/THC"))
sens_p <- ggplot(sens_long, aes(ratio, factor(variable, levels = rev(need_vars)),
                                color = SC_set, shape = SC_set)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#202124") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(trans = "log2") +
  scale_color_manual(values = c("SC all" = "#E07C90", "SC excl. LT/THC" = "#9B4A5C")) +
  labs(title = "Dispersion ratio SC:LT_Quina -- all vs proximal-excluded",
       subtitle = "log2 x; >1 = SC more variable; shift on exclusion = composition confound",
       x = "Dispersion ratio (SC : LT_Quina)", y = NULL, color = NULL, shape = NULL,
       caption = guard_caption) +
  corr_theme
ggsave(file.path(sub$sen, "sensitivity_forest.png"), sens_p, width = 7.6, height = 4.6, dpi = 300)

# ==============================================================================
# D. Cross-variable summary
# ==============================================================================
cat("\n########## BLOCK D: cross-variable summary ##########\n")
summary_tbl <- all_h |>
  transmute(variable, family,
            dispersion_ratio_SC_LT = ratio_SC_LT, ratio_metric = metric,
            ratio_lo = lo, ratio_hi = hi, equality_p = equal_p,
            mean_SC = mean_SC, mean_LT = mean_LT,
            mean_ratio_SC_LT = mean_SC / mean_LT) |>
  arrange(desc(abs(log(dispersion_ratio_SC_LT))))

permdisp_headline <- data.frame(
  metric = "PERMDISP mean-distance-to-centroid ratio SC_Quina : LT_Quina",
  ratio = permA$ratio_SC_LT, permutest_overall_p = permA$overall_p,
  mean_dist_SC = unname(permA$means["SC_Quina"]), mean_dist_LTq = unname(permA$means["LT_Quina"]),
  mean_dist_LTo = unname(permA$means["LT_Ordinary"]))

cat("\n--- PERMDISP HEADLINE (multivariate) ---\n"); print(permdisp_headline, row.names = FALSE)
cat("\n--- dispersion_summary.csv (sorted by ratio magnitude) ---\n"); print(summary_tbl, row.names = FALSE)

# ==============================================================================
# E. Hand-off to the figure scripts
# ==============================================================================
# Tidy DATA ONLY -- never ggplot objects, which serialise their whole build
# environment and do not survive ggplot2 version changes. scripts/figures/
# owns every line of plotting code; this file owns every number.
# Block A (all SC) is what the paper figure uses; Block C is the sensitivity run.

cache_dir <- file.path(proj_dir, "output", "cache", "analysis")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    # --- panel a: PCA ordination of the technical space (z-scored tech6) ---
    pca_scores    = permA$pca_scores,      # PC1, PC2, Group (one row per specimen)
    pca_loadings  = permA$pca_loadings,    # Variable, PC1, PC2
    var_explained = permA$var_explained,   # % variance per PC
    n_by_group    = permA$n_by_group,
    # --- panel c: per-variable distributions + post-hoc brackets ---
    variable_long     = variable_long,     # Group, Variable, Value
    posthoc_brackets  = posthoc_brackets,  # incl. y.position for free_y facets
    # --- statistics quoted in panel subtitles / the figure caption ---
    permanova_overall  = as.data.frame(permanova_result),
    permanova_pairwise = posthoc_result,
    permdisp = list(means = permA$means, overall_p = permA$overall_p,
                    pairwise = permA$pairwise),
    # --- provenance ---
    meta = list(variables = variables, group_levels = grp_levels,
                source_script = "scripts/technological_consistency/surface_vs_longtan.R",
                r_version = as.character(getRversion()),
                built_at = Sys.time())
  ),
  file.path(cache_dir, "surface_vs_longtan.rds")
)

cat("\n########## DONE -> ", base_dir,
    "\n  figures in: multivariate_permdisp/  cv_dimensional/  robust_reduction/  sensitivity/",
    "\n  all statistics are printed to the console above (no CSV written).",
    "\n  figure data -> output/cache/analysis/surface_vs_longtan.rds ##########\n", sep = "")
