## Spearman correlation analyses (Quina scraper, Quina_scraper_surface.xlsx)
##   (1) Ave_RG             ~ Edge_Angle, Thickness, Ave_GIUR, Retouch_length_index
##   (2) Section_asymmetric ~ Edge_Angle, Ave_GIUR, Retouch_length_index, Ave_RG,
##                            Thickness, Invasive_index
## Each pair: Spearman's rho, visualised as faceted scatter plots with a loess
## trend and the rho / p annotation for each pair.

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

sc_path    <- "H:/Quina_valleys/data/Quina_scraper_surface.xlsx"
output_dir <- "H:/Quina_valleys/output/02_scraper_characterization"
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
    axis.ticks.length = grid::unit(2.5, "pt"),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

## ---- load once (all columns available; each analysis selects what it needs) ----
quina <- read_excel(sc_path, sheet = "Quina scraper")

## ---- reusable Spearman analysis + faceted scatter plot ----
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
  results$p_adjusted <- p.adjust(results$p_value, method = "BH")

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

## ---- (1) Ave_RG ----
run_spearman_analysis(
  quina,
  focal_var = "Ave_RG",
  corr_vars = c("Edge_Angle", "Thickness", "Ave_GIUR", "Retouch_length_index"),
  csv_name  = "spearman_AveRG_correlations.csv",
  png_name  = "spearman_AveRG_scatter.png",
  ncol = 2, width = 7.6, height = 6.0
)

## ---- (2) Section_asymmetric ----
run_spearman_analysis(
  quina,
  focal_var = "Section_asymmetric",
  corr_vars = c("Edge_Angle", "Ave_GIUR", "Retouch_length_index",
                "Ave_RG", "Thickness", "Invasive_index"),
  csv_name  = "spearman_Section_asymmetric_correlations.csv",
  png_name  = "spearman_Section_asymmetric_scatter.png",
  ncol = 3, width = 9.2, height = 6.0
)

## ---- (3) Edge_Angle ----
run_spearman_analysis(
  quina,
  focal_var = "Edge_Angle",
  corr_vars = c("Retouch_length_index", "Ave_GIUR", "Ave_RG"),
  csv_name  = "spearman_EdgeAngle_correlations.csv",
  png_name  = "spearman_EdgeAngle_scatter.png",
  ncol = 3, width = 9.2, height = 3.7
)
