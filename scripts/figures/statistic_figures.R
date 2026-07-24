# statistic_figures.R
# Composite statistical figures for the manuscript.
#
# This script contains NO analysis. It reads the tidy data that the analysis
# scripts cache under output/cache/analysis/*.rds and does the plotting only,
# so that re-styling a panel never re-runs a permutation test. Run the analysis
# script first; every number shown here is computed there, not here.
#
# Figures:
#   Fig. TC -- Technological consistency: (A) PCA biplot of the technological space,
#              with marginal histograms of the PC1 and PC2 scores by group,
#              (B) per-variable distributions with post-hoc brackets.
#              <- output/cache/analysis/surface_vs_longtan.rds
#                 (scripts/technological_consistency/surface_vs_longtan.R)
#
# Output:
#   - output/figures/fig_technological_consistency.png

required_packages <- c("dplyr", "tidyr", "ggplot2", "ggpubr", "patchwork", "ggrepel")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install the following R packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(here)
library(grid)
library(ggrepel)
library(ggpubr)

proj_dir  <- here()
cache_dir <- file.path(proj_dir, "output", "cache", "analysis")
fig_dir   <- file.path(proj_dir, "output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

read_cache <- function(name) {
  f <- file.path(cache_dir, name)
  if (!file.exists(f))
    stop("Missing ", f, "\nRun the analysis script that produces it first.")
  readRDS(f)
}

# ==============================================================================
# Shared figure style
# ==============================================================================
# Elsevier double column is 190 mm; this figure is set narrower than full width.
FIG_W_MM <- 150
FIG_DPI  <- 600

# prcomp fixes eigenvector signs arbitrarily. Here PC1 loads negatively on all
# six variables, so "more reduced" plots to the LEFT. Set TRUE to mirror PC1 so
# that increasing PC1 = increasing reduction, which reads more naturally; the
# ordination is unchanged in every other respect. Kept FALSE by default so the
# paper figure matches the diagnostic PNG written by the analysis script.
FLIP_PC1 <- FALSE

group_colors <- c(SC_Quina = "#E07C90", LT_Quina = "#E6C25C", LT_Ordinary = "#6BA8CE")
group_labels <- c(SC_Quina    = "Surface Quina",
                  LT_Quina    = "Longtan Quina",
                  LT_Ordinary = "Longtan non-Quina")

# manuscript wording for the six technological variables. Units belong on the
# measurement axes of panel C; a PCA loading is unitless, so panel B uses the
# bare names.
variable_labels <- c(
  Thickness            = "Thickness (mm)",
  Retouch_length_index = "Retouched perimeter",
  Ave_GIUR             = "GIUR",
  N_Scar               = "Total scars",
  Ave_RG               = "Retouch generations",
  Edge_Angle           = "Edge angle (°)"
)
variable_labels_bare <- sub(" \\(.*\\)$", "", variable_labels)
names(variable_labels_bare) <- names(variable_labels)

fig_theme <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "#202124", fill = NA, linewidth = 0.5),
    axis.ticks       = element_line(color = "#202124", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(color = "#303238", size = 8),
    strip.text       = element_text(face = "bold", color = "#202124", size = 8.5),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    legend.title     = element_blank(),
    legend.text      = element_text(size = 8.5),
    legend.key       = element_blank(),
    plot.tag         = element_text(face = "bold", size = 11),
    plot.background  = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# p values are drawn with plotmath so that "p" is italic next to the italic R^2
fmt_p <- function(p) ifelse(p < 0.001, "italic(p) < 0.001",
                            sprintf("italic(p) == %.3f", p))

# ==============================================================================
# Fig. TC -- Technological consistency
# ==============================================================================

tc <- read_cache("surface_vs_longtan.rds")

sc_flip <- if (FLIP_PC1) -1 else 1
scores  <- tc$pca_scores  |> mutate(PC1 = PC1 * sc_flip)
loadings <- tc$pca_loadings |> mutate(PC1 = PC1 * sc_flip)
vexp    <- tc$var_explained

# --- panel a: PCA ordination -------------------------------------------------
centroids <- scores |>
  group_by(Group) |>
  summarise(cx = mean(PC1), cy = mean(PC2), .groups = "drop")

hulls <- scores |>
  group_by(Group) |>
  filter(n() >= 3) |>
  group_modify(function(g, k) { h <- chull(g$PC1, g$PC2); bind_rows(g[h, ], g[h[1], ]) }) |>
  ungroup()

# pairwise PERMANOVA table, used only by the console caption summary below
pw <- tc$permanova_pairwise

# Variable loadings are overlaid on the same panel as arrows (a biplot). Scores
# and loadings live on different scales -- PC1 scores span about 10.5, PC1
# loadings about 0.46 -- so the arrows are stretched by a common factor chosen to
# fill the plotting region without leaving it. The factor is arbitrary: arrow
# LENGTHS are comparable to one another but not to the point coordinates, and the
# axis ticks refer to the scores only.
arrow_mult <- 0.85 * min(max(abs(scores$PC1)) / max(abs(loadings$PC1)),
                         max(abs(scores$PC2)) / max(abs(loadings$PC2)))
arrows_df <- loadings |>
  mutate(x = PC1 * arrow_mult, y = PC2 * arrow_mult,
         Label = variable_labels_bare[Variable])

# The marginal strips MUST use exactly these expansions too, or the histograms
# drift out of register with the cloud they sit next to.
EXP_X <- expansion(mult = 0.20)
EXP_Y <- expansion(mult = 0.10)

p_a <- ggplot(scores, aes(PC1, PC2, color = Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey55") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey55") +
  geom_polygon(data = hulls, aes(fill = Group, group = Group), alpha = 0.12, color = NA) +
  geom_path(data = hulls, aes(group = Group), linewidth = 0.4, alpha = 0.65) +
  geom_point(size = 1.1, alpha = 0.7, shape = 16) +
  # centroids: group colour inside, rim in the same neutral grey as the loading
  # arrows so the two non-specimen elements read as one visual family
  geom_point(data = centroids, aes(cx, cy, fill = Group),
             shape = 21, color = "#5A5F66", size = 2.4, stroke = 0.6, inherit.aes = FALSE) +
  # Loadings on top of the cloud, in a neutral colour so they read as a
  # different kind of object from the group-coloured specimens. Kept
  # deliberately light -- hairline shafts, small open heads, unbolded labels --
  # because the arrows are structure, not data: at any heavier weight they
  # dominate the panel and the specimen cloud reads as their background.
  geom_segment(data = arrows_df, aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.075, "cm"),
                                   type = "open", angle = 22),
               linewidth = 0.22, color = "#5A5F66", inherit.aes = FALSE) +
  geom_text_repel(
    data = arrows_df, aes(x = x, y = y, label = Label),
    size = 2.2, color = "#3A3D42",
    bg.color = "white", bg.r = 0.12,          # halo, so labels stay legible over points
    segment.color = "grey55", segment.size = 0.2,
    min.segment.length = 0.2, box.padding = 0.35, point.padding = 0.1,
    max.overlaps = Inf, seed = 2226, inherit.aes = FALSE) +
  scale_color_manual(values = group_colors, labels = group_labels) +
  scale_fill_manual(values = group_colors, labels = group_labels) +
  # generous horizontal expansion: arrow labels sit at the arrow tips and are
  # long, so they run off the panel under a default expansion
  scale_x_continuous(expand = EXP_X) +
  scale_y_continuous(expand = EXP_Y) +
  labs(x = sprintf("PC1 (%.1f%%)", vexp[1]),
       y = sprintf("PC2 (%.1f%%)", vexp[2])) +
  fig_theme +
  # The legend sits inside the panel, in the empty top-right corner: as its own
  # layout cell it left a column of white space beside the ordination taller
  # than the keys themselves.
  theme(# no grid: the dashed lines through the origin are the only reference the
        # ordination needs, and the faint grid competes with the specimen cloud
        panel.grid.major = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.995, 0.995),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
        legend.margin = margin(2, 4, 2, 2),
        # key text matched to the axis tick labels (8 pt), and the key glyph and
        # row height pulled in to match, so the legend reads as a caption inside
        # the panel rather than a second focal object
        legend.text = element_text(size = 8),
        legend.key.height = unit(8, "pt")) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(size = 1.6, alpha = 1), ncol = 1))

# --- marginal histograms -----------------------------------------------------
# Counts would be dominated by the largest group (n = 165 vs 53 vs 79), so each
# group's histogram is scaled to its own density and the three are overlaid
# rather than stacked: the point is to compare the SHAPES, and the PC1 strip is
# where the two Quina distributions visibly coincide.
marg_theme <- theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0))

p_mx <- ggplot(scores, aes(PC1, after_stat(density), fill = Group, color = Group)) +
  geom_histogram(position = "identity", alpha = 0.35, linewidth = 0.25, bins = 26) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(expand = EXP_X) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  marg_theme + guides(fill = "none", color = "none")

p_my <- ggplot(scores, aes(y = PC2, after_stat(density), fill = Group, color = Group)) +
  geom_histogram(position = "identity", alpha = 0.35, linewidth = 0.25, bins = 22) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(expand = EXP_Y) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  marg_theme + guides(fill = "none", color = "none")

# --- panel b: per-variable distributions + post-hoc brackets -----------------
p_b <- ggplot(tc$variable_long, aes(Group, Value)) +
  geom_jitter(aes(color = Group), width = 0.3, height = 0, size = 0.8, alpha = 0.5, shape = 16) +
  geom_boxplot(color = "black", fill = NA, width = 0.6, linewidth = 0.45, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 1.4, color = "black") +
  stat_pvalue_manual(
    tc$posthoc_brackets, label = "p.adj.signif", y.position = "y.position",
    tip.length = 0.012, bracket.size = 0.3, label.size = 2.5, color = "#202124") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3,
             labeller = as_labeller(variable_labels)) +
  scale_color_manual(values = group_colors, labels = group_labels) +
  scale_x_discrete(labels = group_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(x = NULL, y = NULL) +
  fig_theme +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 7.5)) +
  guides(color = "none")   # groups are already named on the x axis

# --- assemble ----------------------------------------------------------------
# No figure caption here: it is written in the manuscript, next to the callout.
# The console summary below carries the numbers a caption would need.
#
# Alignment: neither panel uses coord_equal(). It locks the panel to its own
# data aspect, so the panel shrinks inside the cell patchwork allocates instead
# of filling it, and its edges then no longer line up with the panel below.
# Without it both panels fill their cells and share the same left margin. The
# cost is that PC1 and PC2 are not on a common visual scale, so the biplot's
# arrow ANGLES are stretched horizontally -- read direction, not angle.
#
# Row heights are RELATIVE: absolute unit(mm) heights size the panel area
# only, so axis text, facet strips and tags are added on top of them and the
# figure overflows a canvas cut to the same total.
FIG_H_MM <- 205

# Top block: the ordination with a marginal histogram above (PC1) and to the
# right (PC2). All four cells are declared in ONE layout so that patchwork
# aligns the strips to the ordination's panel edges; nesting them as separate
# rows lets each row size its own columns and the strips slip out of register.
# The top-right cell stays empty on purpose -- it is the corner where the two
# marginals meet and has no data to show.
top_row <- (p_mx + labs(tag = "A") +
              theme(plot.tag = element_text(face = "bold", size = 11))) +
  plot_spacer() + p_a + p_my +
  plot_layout(design = "AB\nCD", widths = c(5, 1), heights = c(1, 5.5))

# Tags are set by hand rather than with tag_levels: the automatic enumeration
# counts the two marginal strips as panels and would label them B and C.
fig_tc <- top_row / (p_b + labs(tag = "B")) +
  plot_layout(heights = c(1, 1.5))

ggsave(file.path(fig_dir, "fig_technological_consistency.png"), fig_tc,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI)

cat("Fig. TC written to", file.path(fig_dir, "fig_technological_consistency.{png,pdf}"), "\n")
cat("  source cache built at:", format(tc$meta$built_at), "\n")

# Numbers for the manuscript caption (the figure itself carries none).
cat("\n--- for the Fig. TC caption in manuscript.qmd ---\n")
cat(sprintf("  n = %d surface Quina, %d Longtan Quina, %d Longtan non-Quina\n",
            tc$n_by_group[["SC_Quina"]], tc$n_by_group[["LT_Quina"]],
            tc$n_by_group[["LT_Ordinary"]]))
cat(sprintf("  PC1 = %.1f%%, PC2 = %.1f%% of variance\n", vexp[1], vexp[2]))
cat("  PERMANOVA (pairwise, Bonferroni-adjusted):\n")
print(pw |> transmute(Comparison, R2 = round(R2, 3), F = round(F, 2),
                      p_adj = p_adjusted), row.names = FALSE)
cat(sprintf("  PERMDISP mean distance to centroid: %s\n",
            paste(sprintf("%s %.2f", group_labels[names(tc$permdisp$means)],
                          tc$permdisp$means), collapse = "; ")))
cat(sprintf("  PERMDISP pairwise permuted p: %s\n",
            paste(sprintf("%s %.3f", names(tc$permdisp$pairwise), tc$permdisp$pairwise),
                  collapse = "; ")))
