## Edge angle vs resharpening-flake exterior platform angle (Quina_scraper_surface.xlsx)
##
## Rationale: a Quina resharpening (sharpening) flake detaches the working edge of a
## scraper, so its exterior platform angle (EPA) should record the parent edge angle.
## We test Quina scraper Edge_Angle vs resharpening-flake EPA with Welch's t-test
## (unequal variance) and report Cohen's d as the standardized effect size.

required_packages <- c("readxl", "dplyr", "ggplot2", "rstatix", "ggpubr")
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
library(rstatix)

set.seed(123)

sc_path    <- "H:/Quina_valleys/data/Quina_scraper_surface.xlsx"
output_dir <- "H:/Quina_valleys/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- group labels, colours, theme ----
group_display <- c(
  edge = "Scraper\nEdge angle",
  epa  = "Resharpening\nEPA"
)
group_colors <- c(
  edge = "#E07C90",
  epa  = "#E6C25C"
)

angle_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649",
                                 margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "none",
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

fmt_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3)))
}

## ---- load angles ----
edge_angle <- read_excel(sc_path, sheet = "Quina scraper")[["Edge_Angle"]] |>
  as.numeric()

epa <- read_excel(sc_path, sheet = "Resharpening flake")[["EPA"]] |>
  as.numeric()

## ============================================================================
## Welch's t-test: scraper Edge_Angle vs resharpening-flake EPA
## ============================================================================
data_a <- bind_rows(
  tibble(Group = "edge", Value = edge_angle),
  tibble(Group = "epa",  Value = epa)
) |>
  filter(!is.na(Value)) |>
  mutate(Group = factor(Group, levels = c("edge", "epa")))

welch_t <- rstatix::t_test(data_a, Value ~ Group,
                           var.equal = FALSE, detailed = TRUE)

## Effect size: Cohen's d (unequal variance, to match the Welch test), with
## bootstrap 95% CI.
set.seed(123)
effsize <- rstatix::cohens_d(data_a, Value ~ Group,
                             var.equal = FALSE, ci = TRUE, nboot = 1000)

welch_summary <- welch_t |>
  mutate(
    cohens_d    = effsize$effsize,
    d_magnitude = effsize$magnitude,
    d_conf.low  = effsize$conf.low,
    d_conf.high = effsize$conf.high
  )

cat("== Welch's t-test: Edge_Angle vs EPA ==\n")
print(as.data.frame(welch_t))
cat("\n== Effect size (Cohen's d, unequal variance) ==\n")
print(as.data.frame(effsize))

write.csv(welch_summary,
          file.path(output_dir, "edgeangle_epa_welch_t.csv"),
          row.names = FALSE)

## ---- boxplot: jittered points behind, unfilled black box, black mean dot ----
stat_a <- welch_t |>
  rstatix::add_xy_position(x = "Group") |>
  mutate(p.label = paste0("Welch t, p = ", formatC(p, format = "f", digits = 3)))

subtitle_a <- sprintf(
  "Welch's t-test: t(%.1f) = %.2f, p %s;  Cohen's d = %.2f (%s)",
  welch_t$df, welch_t$statistic, fmt_p(welch_t$p),
  effsize$effsize, effsize$magnitude
)

edge_epa_boxplot <- ggplot(data_a, aes(x = Group, y = Value)) +
  geom_jitter(aes(color = Group), width = 0.31, height = 0,
              size = 1.5, alpha = 0.55, shape = 16) +
  geom_boxplot(color = "black", fill = NA, width = 0.62,
               linewidth = 0.6, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2.4,
               color = "black") +
  ggpubr::stat_pvalue_manual(
    stat_a, label = "p.label",
    tip.length = 0.012, bracket.size = 0.4, label.size = 3.3,
    color = "#202124"
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_discrete(labels = group_display) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(subtitle = subtitle_a, x = NULL, y = "Angle (°)") +
  angle_theme

ggsave(file.path(output_dir, "edgeangle_epa_welch_boxplot.png"),
       edge_epa_boxplot, width = 5.0, height = 5.0, dpi = 300)

print(edge_epa_boxplot)
