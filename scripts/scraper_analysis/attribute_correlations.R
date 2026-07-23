# Quina_scraper_statistic.R
# Spearman rank correlations among Quina-scraper technical variables.
#
# Edge angle is read as an index of cumulative reduction, so it is correlated
# against the three other reduction measures: proportion of the perimeter
# retouched, GIUR, and the mean number of retouch generations.
#
# Pipeline:
#   1. Load the Quina-scraper surface assemblage.
#   2. Spearman correlations with Edge_Angle (Bonferroni-adjusted over the three tests),
#      drawn as a faceted scatter with an lm trend and rho / p labels.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#
# Output:
#   - output/scraper_analysis/spearman_EdgeAngle_scatter.png

# ==============================================================================
# Setup
# ==============================================================================

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2")
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
library(here)
library(grid)

# ==============================================================================
# Global parameters
# ==============================================================================

sc_path    <- here("data", "Quina_scraper_surface.xlsx")
output_dir <- here("output", "scraper_analysis")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

fmt_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3)))
}

corr_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = unit(2.5, "pt"),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# ==============================================================================
# 1. Load data
# ==============================================================================

quina <- read_excel(sc_path, sheet = "Quina scraper")

# ==============================================================================
# 2. Spearman analysis + faceted scatter (helper)
# ==============================================================================

run_spearman_analysis <- function(data, focal_var, corr_vars,
                                  csv_name, png_name,
                                  ncol = 2, width = 7.6, height = 6.0) {
  d <- data |>
    select(all_of(c(focal_var, corr_vars))) |>
    mutate(across(everything(), as.numeric))

  spearman_one <- function(v) {
    pair <- na.omit(d[, c(focal_var, v)])
    ct <- suppressWarnings(
      cor.test(pair[[focal_var]], pair[[v]], method = "spearman", exact = FALSE)
    )
    data.frame(
      Variable = v, n = nrow(pair),
      rho = unname(ct$estimate), S = unname(ct$statistic),
      p_value = ct$p.value, row.names = NULL
    )
  }

  results <- do.call(rbind, lapply(corr_vars, spearman_one))
  results$p_adjusted <- p.adjust(results$p_value, method = "bonferroni")

  cat("\nSpearman correlations with", focal_var, ":\n")
  print(results)

  long <- d |>
    pivot_longer(all_of(corr_vars), names_to = "Variable", values_to = "Value") |>
    filter(!is.na(.data[[focal_var]]), !is.na(Value)) |>
    mutate(Variable = factor(Variable, levels = corr_vars))

  labels <- results |>
    mutate(
      Variable = factor(Variable, levels = corr_vars),
      label = sprintf("rho = %.2f\np %s", rho, fmt_p(p_value))
    )

  p <- ggplot(long, aes(x = Value, y = .data[[focal_var]])) +
    geom_point(color = "#303238", alpha = 0.5, size = 1.6, shape = 16) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#6BA8CE", fill = "#9BC7DF", linewidth = 0.8) +
    geom_text(
      data = labels,
      aes(x = -Inf, y = Inf, label = label),
      hjust = -0.12, vjust = 1.2, size = 3.4, color = "#202124",
      lineheight = 0.95, inherit.aes = FALSE
    ) +
    facet_wrap(
      ~ Variable, scales = "free_x", ncol = ncol,
      labeller = as_labeller(function(x) gsub("_", " ", x))
    ) +
    labs(x = NULL, y = focal_var) +
    corr_theme

  ggsave(file.path(output_dir, png_name), p,
         width = width, height = height, dpi = 300)
  print(p)
  invisible(results)
}

# ==============================================================================
# 3. Correlations with edge angle
# ==============================================================================
# One focal variable, so BH adjusts over a single family of three tests.

run_spearman_analysis(
  quina,
  focal_var = "Edge_Angle",
  corr_vars = c("Retouch_length_index", "Ave_GIUR", "Ave_RG"),
  csv_name  = "spearman_EdgeAngle_correlations.csv",
  png_name  = "spearman_EdgeAngle_scatter.png",
  ncol = 3, width = 9.2, height = 3.7
)
