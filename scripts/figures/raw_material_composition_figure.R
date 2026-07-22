# raw_material_composition_figure.R
# Manuscript figure: raw-material composition, available vs used, across valleys.
#
# One frame, two rows:
#   top    -- AVAILABLE: lithology of the river-gravel cobbles, by valley;
#   bottom -- USED: lithology of the Quina scrapers, by valley.
# Each cell is a 100% stacked bar for one valley. The point the figure makes is
# the contrast between the rows: available composition swings from valley to
# valley (sandstone 83 / 52 / 81%), while used composition stays ~100% trachyte
# everywhere -- selection against local abundance.
#
# Data pipeline matches scripts/raw_material_analysis/raw_material_permanova.R
# (harmonised sandstone classes; sites PJDD, ZKZ dropped; river_ID nests in
# basin). Composition is recomputed here (a couple of counts, no permutation),
# not cached.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#   - data/Site_information.xlsx
#   - data/Raw_mat_basin.xlsx (Sheet1)
# Output:
#   - output/figures/fig_raw_material_composition.png

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
library(patchwork)
library(here)
library(grid)

proj_dir   <- here()
sc_path    <- file.path(proj_dir, "data", "Quina_scraper_surface.xlsx")
site_path  <- file.path(proj_dir, "data", "Site_information.xlsx")
basin_path <- file.path(proj_dir, "data", "Raw_mat_basin.xlsx")
fig_dir    <- file.path(proj_dir, "output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

FIG_W_MM <- 150
FIG_H_MM <- 108
FIG_DPI  <- 600

drop_sites <- c("PJDD", "ZKZ")

# shared levels; palette in the muted family of fig_technological_consistency
# (pink / blue / yellow reuse its exact group hues; purple + green added to fill
# the five-material set), mapped to materials in legend order.
material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone", "Andesite")
material_colors <- c(Trachyte  = "#E07C90",   # pink
                     Sandstone = "#6BA8CE",   # blue
                     Quartz    = "#E6C25C",   # yellow
                     Mudstone  = "#9B87C4",   # purple
                     Andesite  = "#6FB98E")   # green
river_levels <- c("Sangyuan", "Liandong", "Caifeng")
layer_levels <- c("River cobbles", "Surface Quina scrapers")

harmonise_lithology <- function(x) {
  x <- trimws(as.character(x))
  recode(x, "Quartz sandstone" = "Sandstone", "Coarse sandstone" = "Sandstone")
}
strip_basin <- function(x) sub(" basin$", "", trimws(as.character(x)))

# ==============================================================================
# Load + assemble both layers into one long frame
# ==============================================================================
sites <- read_excel(site_path)
names(sites) <- trimws(names(sites))
site_key <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            river_ID = factor(trimws(river_ID), levels = river_levels))

# USED -- Quina scrapers
used <- read_excel(sc_path, sheet = "Quina scraper") |>
  transmute(Site_ID  = trimws(as.character(Site_ID)),
            Material = harmonise_lithology(Raw_material)) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  filter(!Site_ID %in% drop_sites) |>
  left_join(site_key, by = "Site_ID") |>
  filter(!is.na(river_ID)) |>
  transmute(Layer = layer_levels[2], river_ID, Material)

# AVAILABLE -- basin-survey cobbles
avail <- read_excel(basin_path, sheet = "Sheet1")
names(avail) <- trimws(names(avail))
avail <- avail |>
  transmute(river_ID = factor(trimws(river_ID), levels = river_levels),
            Material = harmonise_lithology(Lithology)) |>
  filter(!is.na(Material), !is.na(river_ID)) |>
  transmute(Layer = layer_levels[1], river_ID, Material)

both <- bind_rows(avail, used) |>
  mutate(Layer    = factor(Layer, levels = layer_levels),
         Material = factor(Material, levels = material_levels))

# composition (% within Layer x valley) + per-cell N for the bar-top labels
comp <- both |>
  count(Layer, river_ID, Material, name = "n") |>
  complete(Layer, river_ID, Material, fill = list(n = 0)) |>
  group_by(Layer, river_ID) |>
  mutate(percent = 100 * n / sum(n)) |>
  # label position precomputed on the FULL stack, in the left-to-right order that
  # geom_col uses (reverse factor order); filtering which labels to SHOW then
  # cannot shift the survivors, which is what position_stack on a subset does.
  arrange(desc(as.integer(Material)), .by_group = TRUE) |>
  mutate(pos = cumsum(percent) - percent / 2) |>
  ungroup()
counts <- both |> count(Layer, river_ID, name = "N")

cat("Composition (% by layer x valley):\n")
print(comp |> filter(percent > 0) |> arrange(Layer, river_ID, desc(percent)),
      n = 40)

# ==============================================================================
# style (matches statistic_figures.R / edge_angle_reduction_figure.R)
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
    strip.text       = element_text(face = "bold", color = "#202124", size = 9),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    legend.title     = element_text(size = 8.5),
    legend.text      = element_text(size = 8.5),
    legend.key       = element_blank(),
    legend.key.size  = unit(10, "pt"),
    plot.background  = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# valleys read top-to-bottom Sangyuan -> Caifeng (discrete axis plots first level
# at the bottom, so the limits are reversed)
valley_order <- rev(river_levels)

# --- left: horizontal 100% stacked composition, Available over Used ----------
p_comp <- ggplot(comp, aes(percent, river_ID, fill = Material)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.3) +
  # segment percentages (only the readable ones, so tiny slivers stay unlabelled)
  geom_text(data = subset(comp, percent >= 7),
            aes(x = pos, label = sprintf("%.0f%%", percent)),
            size = 2.5, colour = "white", fontface = "bold") +
  facet_wrap(~ Layer, ncol = 1) +
  scale_fill_manual(values = material_colors, drop = FALSE) +
  scale_y_discrete(limits = valley_order) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%")) +
  labs(x = "Percentage", y = "Valley", fill = "Raw material") +
  fig_theme +
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

# --- right: sample size per cell as its own bar chart ------------------------
p_n <- ggplot(counts, aes(N, river_ID)) +
  geom_col(width = 0.62, fill = "#8A8F96") +
  facet_wrap(~ Layer, ncol = 1) +
  scale_y_discrete(limits = valley_order) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Count (n)", y = NULL) +
  fig_theme +
  theme(panel.grid.major = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_blank(), strip.background = element_blank())

fig <- p_comp + p_n +
  plot_layout(widths = c(3.4, 1), guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "fig_raw_material_composition.png"), fig,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI)
cat("\nFig. written to", file.path(fig_dir, "fig_raw_material_composition.png"), "\n")
