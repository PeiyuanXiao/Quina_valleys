# edge_angle_reduction_figure.R
# Manuscript figure: edge angle vs the three other reduction measures.
#
# Edge angle is read as an index of cumulative reduction; this figure shows it
# covaries positively with the proportion of the perimeter retouched, GIUR, and
# the number of retouch generations, which is the support cited for that reading
# (Section 4.2.1).
#
# Unlike statistic_figures.R -- which reads cached permutation results so that
# re-styling never re-runs a test -- the statistics here are three Spearman
# correlations with no permutation step, so they are recomputed inline (instant)
# rather than cached. The rho / p shown are the raw (unadjusted) values that the
# manuscript text quotes; BH-adjusted p are printed to the console for the record.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
# Output:
#   - output/figures/fig_edge_angle_reduction.png

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2")
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
library(here)
library(grid)

set.seed(2226)

proj_dir <- here()
sc_path  <- file.path(proj_dir, "data", "Quina_scraper_surface.xlsx")
fig_dir  <- file.path(proj_dir, "output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

FIG_W_MM <- 150
FIG_H_MM <- 62
FIG_DPI  <- 600

# ==============================================================================
# Shared style (identical to statistic_figures.R, so the two figures read as a set)
# ==============================================================================
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
    plot.background  = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# focal variable and the three reduction measures it is correlated against.
# Order is deliberate: rho increases left to right (0.19 -> 0.27 -> 0.43), so the
# panels read as a strengthening gradient.
focal     <- "Edge_Angle"
corr_vars <- c("Retouch_length_index", "Ave_GIUR", "Ave_RG")
corr_labels <- c(
  Retouch_length_index = "Retouched perimeter",
  Ave_GIUR             = "GIUR",
  Ave_RG               = "Retouch generations"
)

fmt_p <- function(p) ifelse(p < 0.001, "italic(p) < 0.001",
                            sprintf("italic(p) == %.3f", p))

# ==============================================================================
# Load + Spearman
# ==============================================================================
quina <- read_excel(sc_path, sheet = "Quina scraper") |>
  mutate(across(all_of(c(focal, corr_vars)), as.numeric))

spearman_one <- function(v) {
  pair <- quina |> select(all_of(c(focal, v))) |> drop_na()
  ct <- suppressWarnings(cor.test(pair[[focal]], pair[[v]],
                                  method = "spearman", exact = FALSE))
  data.frame(Variable = v, n = nrow(pair), rho = unname(ct$estimate),
             p_value = ct$p.value)
}
results <- do.call(rbind, lapply(corr_vars, spearman_one))
results$p_adjusted <- p.adjust(results$p_value, method = "BH")
cat("Spearman correlations with", focal, "(raw p shown on figure; BH for the record):\n")
print(results, row.names = FALSE)

# ==============================================================================
# Long data for the faceted scatter
# ==============================================================================
# Retouch generations is discrete (integers / half-steps), so its points stack
# into vertical bars. A small horizontal jitter -- applied to the DRAWN x only,
# never to the x the trend line is fitted on -- spreads them into a cloud. The
# two continuous measures need no jitter.
long <- quina |>
  select(all_of(c(focal, corr_vars))) |>
  pivot_longer(all_of(corr_vars), names_to = "Variable", values_to = "Value") |>
  filter(!is.na(.data[[focal]]), !is.na(Value)) |>
  mutate(
    Variable = factor(Variable, levels = corr_vars),
    x_plot   = if_else(Variable == "Ave_RG",
                       Value + runif(n(), -0.11, 0.11), Value)
  )

labels <- results |>
  mutate(Variable = factor(Variable, levels = corr_vars),
         label = sprintf("rho == %.2f * ',' ~ %s", rho, fmt_p(p_value)))

# ==============================================================================
# Plot
# ==============================================================================
p <- ggplot(long, aes(y = .data[[focal]])) +
  # trend fitted on the true x (not the jittered x); Spearman rho is reported,
  # lm is kept only as a visual guide to direction. Dashed, in the blue of the
  # Longtan non-Quina group in fig_technological_consistency (#6BA8CE), so the
  # line reads as an added guide rather than a fitted model.
  geom_smooth(aes(x = Value), method = "lm", formula = y ~ x, se = TRUE,
              color = "#6BA8CE", fill = "#CBDCEA", linewidth = 0.6, linetype = "dashed") +
  # shape 16 = solid dot, no border; deeper shade of the #7777AA muted purple
  geom_point(aes(x = x_plot), color = "#565693", alpha = 0.5, size = 1.4, shape = 16) +
  geom_text(data = labels, aes(x = -Inf, y = Inf, label = label), parse = TRUE,
            hjust = -0.08, vjust = 1.35, size = 2.5, color = "#202124",
            inherit.aes = FALSE) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3,
             labeller = as_labeller(corr_labels)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.14))) +
  scale_x_continuous(expand = expansion(mult = 0.06)) +
  labs(x = NULL, y = "Edge angle (°)") +
  fig_theme +
  theme(panel.grid.major = element_blank())

ggsave(file.path(fig_dir, "fig_edge_angle_reduction.png"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI)

cat("\nFig. written to", file.path(fig_dir, "fig_edge_angle_reduction.png"), "\n")
