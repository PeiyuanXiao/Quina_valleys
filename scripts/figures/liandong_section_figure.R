# liandong_section_figure.R
# SUPERSEDED as a Figure 1 panel by scripts/figure1/traverse_profile_figure.R:
# a swath shows the DISTRIBUTION of surface heights at a given cross-valley
# offset, so the sites necessarily float below the median line and need
# explaining, whereas the traverse puts them on the drawn ground. Kept for the
# note below, which is the only written record that d_river_m and h_river_m —
# both of which the landscape analyses use — disagree with the DEM. Nothing
# sources this script; it is not part of the Figure 1 pipeline.
#
# Swath cross-section of the Liandong valley (Binchuan), with the 14 Liandong
# Quina sites projected onto it.
#
# WHY THE COORDINATES ARE COMPUTED HERE RATHER THAN TAKEN FROM THE SITE TABLE
# --------------------------------------------------------------------------
# Site_information.xlsx carries d_river_m and h_river_m, but neither can serve
# as a cross-section coordinate:
#   * d_river_m is uncorrelated with any DEM-derived channel network
#     (Spearman 0.17 against the 1500-cell network; 4-8 km against the trunk).
#     It is a distance to the NEAREST channel of a branching network, not a
#     cross-valley distance: Liandong T4 sites sit 459-721 m out and the LOWER
#     T3 sites 210-944 m out, i.e. the levels do not order by distance at all.
#   * h_river_m disagrees with DEM height-above-nearest-drainage for the
#     hilltops in particular (WGQ 187 m vs 58 m, HBC 158 m vs 64 m) because it
#     is measured from the valley floor rather than the nearest gully.
# elev_m, by contrast, agrees with SRTM (Spearman 0.94, median |diff| 22 m).
#
# So the section is built entirely from the DEM plus elev_m, and the y axis is
# height above the LOCAL valley floor as measured on the same swath. Those
# numbers are therefore NOT the h_river_m of the analyses and the caption must
# say so.
#
# Method:
#   1. along-valley axis = first principal axis of the Liandong site positions;
#   2. transects perpendicular to it every 100 m along the reach, +/- 1200 m;
#   3. per transect, valley floor = minimum elevation -> height above floor;
#   4. pooled over transects by cross-valley offset -> min/quartile/max envelope;
#   5. sites projected onto the same axes, height above the floor of their own
#      transect.
#
# Input:  data/cache/dem.tif, data/Site_information.xlsx
# Output: output/figures/fig_liandong_section.png (+ .pdf)

required_packages <- c("readxl", "dplyr", "ggplot2", "ggrepel", "sf", "terra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install: ", paste(missing_packages, collapse = ", "))
}

library(sf); library(terra); library(dplyr); library(readxl); library(ggplot2)
library(here)
sf::sf_use_s2(FALSE)

proj_dir  <- here()
cache_dir <- file.path(proj_dir, "data", "cache")
fig_dir   <- file.path(proj_dir, "output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

FIG_W_MM <- 150; FIG_H_MM <- 78; FIG_DPI <- 600

half_width  <- 3000   # transect half-length, m (WYW sits 2.46 km off the axis)
step_cross  <- 30     # sampling interval across the section, m
step_along  <- 100    # spacing of transects along the reach, m
## The Liandong sites span 11.7 km along the valley and the floor drops 172 m
## over that distance. Pooling every transect over the whole spread averages
## away the terrace treads and returns a generic V. The swath is therefore cut
## to a homogeneous reach centred on the dense core; sites outside it are
## reported and excluded rather than projected in from kilometres away.
reach_half  <- 2200   # half-length of the pooled reach, m
y_max       <- 180    # clip: a single steep hillside otherwise sets the scale

basin_col       <- "#A0364B"
geomorph_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)
label_col       <- "#332F29"

# ==============================================================================
# Sites + DEM
# ==============================================================================
sites <- read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(trimws(river_ID) == "Liandong") |>
  mutate(geomorph = factor(geomorph, levels = names(geomorph_shapes))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(32647)
xy <- st_coordinates(sites)
cat("Liandong sites:", nrow(sites), "\n")

dem <- terra::rast(file.path(cache_dir, "dem.tif")) |>
  terra::project("EPSG:32647", res = 30, method = "bilinear")

# ---- along-valley axis from the site cloud ----------------------------------
ctr <- colMeans(xy)
pc  <- prcomp(xy, center = TRUE, scale. = FALSE)
u   <- pc$rotation[, 1]                       # along-valley
v   <- c(-u[2], u[1])                         # cross-valley
along <- as.numeric((xy[, 1] - ctr[1]) * u[1] + (xy[, 2] - ctr[2]) * u[2])
cross <- as.numeric((xy[, 1] - ctr[1]) * v[1] + (xy[, 2] - ctr[2]) * v[2])
cat(sprintf("valley axis bearing %.0f deg; sites span %.0f m along, %.0f m across\n",
            (90 - atan2(u[2], u[1]) * 180 / pi) %% 180,
            diff(range(along)), diff(range(cross))))
cat("variance explained by the along-valley axis:",
    round(100 * pc$sdev[1]^2 / sum(pc$sdev^2)), "%\n")

# ==============================================================================
# Swath: transects perpendicular to the axis
# ==============================================================================
a_mid <- median(along)
in_reach <- abs(along - a_mid) <= reach_half
cat("\nreach = median along-position +/-", reach_half, "m;",
    sum(in_reach), "of", nrow(sites), "sites inside\n")
cat("excluded (too far along-valley to belong to this section):",
    paste(sites$code[!in_reach], collapse = ", "), "\n")

a_seq <- seq(a_mid - reach_half, a_mid + reach_half, by = step_along)
c_seq <- seq(-half_width, half_width, by = step_cross)

grid <- expand.grid(a = a_seq, c = c_seq)
grid$X <- ctr[1] + grid$a * u[1] + grid$c * v[1]
grid$Y <- ctr[2] + grid$a * u[2] + grid$c * v[2]
grid$z <- terra::extract(dem, cbind(grid$X, grid$Y))[, 1]

floors <- grid |>
  group_by(a) |>
  summarise(floor = min(z, na.rm = TRUE), .groups = "drop")
cat("valley-floor elevation along the reach:",
    paste(round(range(floors$floor)), collapse = " - "), "m\n")

swath <- grid |>
  left_join(floors, by = "a") |>
  mutate(h = z - floor) |>
  filter(!is.na(h)) |>
  group_by(c) |>
  summarise(p10 = quantile(h, 0.10), q25 = quantile(h, 0.25),
            med = median(h), q75 = quantile(h, 0.75),
            p90 = quantile(h, 0.90), .groups = "drop")

## put the channel, not the site centroid, at x = 0
c0 <- swath$c[which.min(swath$med)]
cat("channel offset from the site centroid:", round(c0), "m (shifted to x = 0)\n")
swath$c <- swath$c - c0

# ---- sites onto the same frame ----------------------------------------------
site_floor <- approx(floors$a, floors$floor, xout = along, rule = 2)$y
sites <- sites |>
  mutate(cross_m = cross - c0,
         h_dem   = elev_m - site_floor,
         in_reach = in_reach)
cat("\nsite height above the local valley floor (DEM-derived) vs table h_river_m:\n")
print(as.data.frame(sites |> st_drop_geometry() |>
        transmute(code, geomorph, cross_m = round(cross_m),
                  h_dem = round(h_dem), table_h = h_river_m,
                  diff = round(h_dem - h_river_m), in_reach) |>
        arrange(cross_m)), row.names = FALSE)

sec_sites <- sites |> filter(in_reach, abs(cross_m) <= half_width)

# ==============================================================================
# Plot
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

p <- ggplot() +
  # full range of the swath, then the interquartile core, then the median form
  geom_ribbon(data = swath, aes(c, ymin = p10, ymax = p90),
              fill = "#D8D3C6", alpha = 0.55) +
  geom_ribbon(data = swath, aes(c, ymin = q25, ymax = q75),
              fill = "#B3AC99", alpha = 0.65) +
  geom_line(data = swath, aes(c, med), colour = "#5E5849", linewidth = 0.5) +
  geom_hline(yintercept = 0, colour = "#86A6BB", linewidth = 0.5) +
  geom_point(data = sec_sites, aes(cross_m, h_dem, shape = geomorph),
             fill = basin_col, size = 2.4, colour = "white", stroke = 0.45) +
  ggrepel::geom_text_repel(
    data = sec_sites, aes(cross_m, h_dem, label = code),
    size = 2.1, fontface = "bold", colour = label_col,
    bg.color = grDevices::adjustcolor("white", alpha.f = 0.8), bg.r = 0.12,
    seed = 3, max.overlaps = Inf, force = 5, force_pull = 0.5,
    box.padding = 0.25, point.padding = 0.12, min.segment.length = 0,
    segment.color = "grey55", segment.size = 0.22) +
  scale_shape_manual(values = geomorph_shapes, name = "Geomorphic position",
                     drop = FALSE,
                     guide = guide_legend(override.aes = list(
                       fill = "grey45", colour = "white", size = 2.6,
                       stroke = 0.45))) +
  scale_x_continuous(breaks = seq(-3000, 3000, 1000),
                     expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(NA, y_max)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.04))) +
  labs(x = "Distance across the valley (m)",
       y = "Height above valley floor (m)") +
  fig_theme

ggsave(file.path(fig_dir, "fig_liandong_section.png"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(fig_dir, "fig_liandong_section.pdf"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
cat("\nFig. written to", file.path(fig_dir, "fig_liandong_section.png"), "\n")
