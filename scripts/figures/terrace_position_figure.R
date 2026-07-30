# terrace_position_figure.R
# SUPERSEDED as a Figure 1 panel by scripts/figure1/traverse_profile_figure.R,
# which shows the sites on the real ground surface along the valleys instead of
# in an abstract cross-section frame. Kept because the cross-section view answers
# a different question — how the sites distribute across terrace levels — and no
# figure in the paper currently carries it. Nothing sources this script; it is
# not part of the Figure 1 pipeline.
#
# Where the Quina sites sit in the valley cross-section.
#
# The frame is a valley profile in data space:
#   x -- distance to the nearest channel (log10; d_river_m is strongly right
#        skewed, THC at 2690 m against a 596 m median, so a linear axis would
#        collapse the other 24 sites into the left margin)
#   y -- height above the LOCAL channel (linear). h_river_m rather than absolute
#        elevation: measured from the local channel, so the difference in base
#        level between the two basins cancels out. This is the same reasoning
#        as scripts/technological_consistency/landscape_structure.R.
#
# Symbols repeat the plan map exactly (fill = basin, shape = geomorphic
# position), so Fig. 1 can carry one shared legend for map + this panel.
#
# The shaded bands are the Binchuan terrace levels ONLY, and are labelled as
# such, because the terrace labels are not commensurate between the two basins:
#   Binchuan  T3 25-48 m < T4 51-73 m < hilltop 150-187 m  (clean, monotonic)
#   Heqing    T2 40-68 m, T3 35-69 m                       (fully overlapping)
# Heqing's "T2" occupies the same height band as Binchuan's "T4". Drawing the
# bands from Binchuan alone lets that mismatch show rather than hiding it.
#
# Input:
#   - data/Site_information.xlsx
# Output:
#   - output/figures/fig_terrace_position.png (+ .pdf)

required_packages <- c("readxl", "dplyr", "ggplot2", "ggrepel")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install the following R packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

library(readxl)
library(dplyr)
library(ggplot2)
library(here)

proj_dir  <- here()
site_path <- file.path(proj_dir, "data", "Site_information.xlsx")
fig_dir   <- file.path(proj_dir, "output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

FIG_W_MM <- 150
FIG_H_MM <- 90
FIG_DPI  <- 600

drop_sites <- c("PJDD", "ZKZ")
cave_sites <- c("THC")            # not a terrace site; annotated as such

# palette + symbol mapping shared with scripts/figure1/terra_map_2D.R
basin_cols      <- c(Binchuan = "#A0364B", Heqing = "#2F6489")
geomorph_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)
label_col       <- "#332F29"
band_fill       <- "#E4E0D6"

# ==============================================================================
# Load
# ==============================================================================
sites <- read_excel(site_path)
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% drop_sites) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)),
                      levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = names(geomorph_shapes)),
    # caves get flagged in the label: "terrace position" does not apply to them
    lab      = ifelse(code %in% cave_sites, paste0(code, " (cave)"), code)
  ) |>
  filter(!is.na(h_river_m), !is.na(d_river_m))

# Binchuan terrace bands, read off the data rather than hard-coded
bands <- sites |>
  filter(basin == "Binchuan") |>
  group_by(geomorph) |>
  summarise(lo = min(h_river_m), hi = max(h_river_m), n = n(), .groups = "drop") |>
  filter(n > 0) |>
  mutate(label = paste0("Binchuan ", geomorph))

cat("Height above local channel (m), by basin x geomorphic position:\n")
print(sites |> group_by(basin, geomorph) |>
        summarise(n = n(), lo = min(h_river_m), med = median(h_river_m),
                  hi = max(h_river_m), .groups = "drop"))
cat("\nDistance to channel (m): median ", median(sites$d_river_m),
    ", range ", min(sites$d_river_m), "-", max(sites$d_river_m), "\n", sep = "")

# ==============================================================================
# style (matches statistic_figures.R / raw_material_composition_figure.R)
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
    legend.title     = element_text(size = 8.5),
    legend.text      = element_text(size = 8.5),
    legend.key       = element_blank(),
    legend.key.size  = unit(10, "pt"),
    plot.background  = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

x_lims <- c(170, 3400)
y_lims <- c(15, 200)

p <- ggplot(sites, aes(d_river_m, h_river_m)) +
  # Binchuan terrace levels as background bands, labelled inside their top-right
  geom_rect(data = bands, inherit.aes = FALSE,
            aes(xmin = x_lims[1], xmax = x_lims[2], ymin = lo, ymax = hi),
            fill = band_fill, alpha = 0.55) +
  geom_text(data = bands, inherit.aes = FALSE,
            aes(x = x_lims[2] * 0.94, y = hi, label = label),
            hjust = 1, vjust = 1.35, size = 2.3, fontface = "italic",
            colour = "#7A7468") +
  geom_point(aes(fill = basin, shape = geomorph),
             size = 2.4, colour = "white", stroke = 0.45) +
  ggrepel::geom_text_repel(
    aes(label = lab), size = 2.1, fontface = "bold", colour = label_col,
    bg.color = grDevices::adjustcolor("white", alpha.f = 0.8), bg.r = 0.12,
    seed = 7, max.overlaps = Inf, force = 5, force_pull = 0.5,
    box.padding = 0.24, point.padding = 0.12, min.segment.length = 0,
    segment.color = "grey55", segment.size = 0.22) +
  scale_x_log10(limits = x_lims, breaks = c(200, 300, 500, 1000, 2000, 3000),
                labels = scales::label_comma(accuracy = 1),
                expand = expansion(mult = 0)) +
  scale_y_continuous(limits = y_lims, breaks = seq(0, 200, 25),
                     expand = expansion(mult = 0)) +
  scale_fill_manual(
    values = basin_cols, name = "Basin",
    guide = guide_legend(order = 1,
      override.aes = list(shape = 21, size = 2.6, colour = "white", stroke = 0.45))) +
  scale_shape_manual(
    values = geomorph_shapes, name = "Geomorphic position", drop = FALSE,
    guide = guide_legend(order = 2,
      override.aes = list(fill = "grey45", colour = "white", size = 2.6,
                          stroke = 0.45))) +
  labs(x = "Distance to nearest channel (m, log scale)",
       y = "Height above local channel (m)") +
  fig_theme

ggsave(file.path(fig_dir, "fig_terrace_position.png"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(fig_dir, "fig_terrace_position.pdf"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
cat("\nFig. written to", file.path(fig_dir, "fig_terrace_position.png"), "\n")
