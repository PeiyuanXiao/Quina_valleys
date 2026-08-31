# traverse_profile_figure.R
# Figure 1C: topographic long profile along a traverse that visits every Quina
# site, with the sites sitting on the real ground surface. Run setup.R first.
# fig01_export_panels.R re-sources this script to place the panel at its final
# size; the standalone export below is a full-width version of the same plot.
#
# The route is a polyline through the sites, ordered within each river transect
# by position along that valley's own principal axis, NOT by latitude: the
# Liandong reach runs partly E-W, so ordering by latitude scrambles the
# along-valley sequence (that was the flaw in the earlier profile this figure
# replaced). The three transects are then chained end to end, oriented so that
# the connectors between them are as short as they can be.
#
# Transect sequence: Binchuan basin -> Liandong -> Huangping basin, i.e.
# Sangyuan valley -> Liandong valley -> Caifeng valley. The names in the table
# are the valleys, and each maps onto exactly one of those basin units:
# Sangyuan is the Binchuan basin floor (6 sites, 1359-1542 m), Liandong the
# upland valley behind it (16 sites, Binchuan basin), Caifeng the Huangping
# (Heqing) basin (5 sites). The traverse therefore runs basin floor -> tributary
# valley -> the neighbouring basin.
#
# Elevation is sampled from the SRTM DEM every 30 m along the route, so each
# site plots at its own ground surface rather than being projected onto some
# unrelated section line. This is the reason for a traverse rather than the
# cross-valley swath of scripts/figures/liandong_section_figure.R: a swath shows the
# DISTRIBUTION of surface heights at a given offset, so sites necessarily float
# below the median line and need explaining. Here they sit on the ground.
#
# Sites are plotted at the table's elev_m, with a hairline dropping to the DEM
# surface beneath them. Plotting them at the DEM elevation instead would put
# every point exactly on the drawn ground, but it misplaces the three sites
# where 30 m SRTM cannot represent the landform: THC (a cave in a cliff) lands
# 295 m too high, SP +131 m, WGQ -116 m. For the other 22 sites the two agree
# to within 40 m and the hairline is invisible at this scale, so the
# discrepancy is shown where it exists rather than smoothed away.
#
# Input:  data/cache/dem.tif, data/Site_information.xlsx
# Output: output/figures/fig_traverse_profile.png (+ .pdf)

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
sample_m <- 30        # DEM sampling interval along the route

basin_cols      <- c(Binchuan = "#A0364B", Huangping = "#2F6489")
geomorph_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)
label_col       <- "#332F29"
ground_fill     <- "#DCD6C8"
ground_line     <- "#5E5849"

# transect order along the traverse: Binchuan basin -> Liandong -> Huangping
transect_levels <- c("Sangyuan valley", "Liandong valley", "Caifeng valley")

# ==============================================================================
# Sites
# ==============================================================================
sites <- read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = names(basin_cols)),
    geomorph = factor(geomorph, levels = names(geomorph_shapes)),
    transect = factor(paste0(trimws(river_ID), " valley"),
                      levels = transect_levels)
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(32647)
stopifnot(!any(is.na(sites$transect)))

dem <- terra::rast(file.path(cache_dir, "dem.tif")) |>
  terra::project("EPSG:32647", res = 30, method = "bilinear")

# ---- order sites within each transect by that valley's own principal axis ----
order_along_valley <- function(g) {
  xy <- st_coordinates(g)
  if (nrow(xy) < 3) return(g[order(xy[, 1]), ])
  u <- prcomp(xy, center = TRUE, scale. = FALSE)$rotation[, 1]
  g[order(as.numeric(xy %*% u)), ]
}
groups <- lapply(transect_levels, function(tl)
  order_along_valley(sites[sites$transect == tl, ]))

# ---- orient the groups ------------------------------------------------------
# The transect ORDER is fixed by transect_levels; only the direction each one is
# walked in is free, and order_along_valley() returns an arbitrary direction
# along the principal axis. With three groups there are just 2^3 = 8 ways to
# orient them, so take the one with the shortest total connector length rather
# than chaining greedily. Greedy chaining is locally optimal only: it enters
# Liandong at its north end and then has to run 19 km across empty ground to
# reach Caifeng, where entering Liandong from the south and leaving by the north
# costs 6 km less over the traverse as a whole.
ends <- lapply(groups, function(g) {
  xy <- st_coordinates(g); list(first = xy[1, ], last = xy[nrow(xy), ])
})
combos <- expand.grid(rep(list(c(FALSE, TRUE)), length(groups)))
connector_km <- apply(combos, 1, function(fl) {
  e <- lapply(seq_along(ends), function(i)
    if (fl[i]) list(first = ends[[i]]$last, last = ends[[i]]$first) else ends[[i]])
  sum(vapply(seq_along(e)[-1], function(i)
    sqrt(sum((e[[i]]$first - e[[i - 1]]$last)^2)), numeric(1))) / 1000
})
flip <- as.logical(combos[which.min(connector_km), ])
for (i in seq_along(groups))
  if (flip[i]) groups[[i]] <- groups[[i]][rev(seq_len(nrow(groups[[i]]))), ]
cat("connectors between transects:", round(min(connector_km), 1), "km",
    sprintf("(worst orientation: %.1f km)", max(connector_km)), "\n")
## `route_sites` and, further down, the plot object `p` are both read from
## outside this script: fig01_export_panels.R sources this file into its own
## environment and takes them by name — `p` to re-render panel C at its placed
## size, `route_sites` to draw the traverse onto panel A. Do not rename either.
route_sites <- do.call(rbind, groups)

# ==============================================================================
# Route + terrain profile
# ==============================================================================
pts <- st_coordinates(route_sites)
seg <- sqrt(rowSums(diff(pts)^2))
route_sites$dist_km <- c(0, cumsum(seg)) / 1000
cat("traverse length:", round(max(route_sites$dist_km), 1), "km over",
    nrow(route_sites), "sites\n")

line <- st_sfc(st_linestring(pts), crs = 32647) |>
  st_segmentize(dfMaxLength = sample_m)
lc <- st_coordinates(line)[, 1:2]
prof <- data.frame(
  dist_km = c(0, cumsum(sqrt(rowSums(diff(lc)^2)))) / 1000,
  elev    = terra::extract(dem, lc)[, 1]
)
prof <- prof[!is.na(prof$elev), ]

route_sites$dem_elev <- terra::extract(dem, pts)[, 1]
cat("terrain along the route:", paste(round(range(prof$elev)), collapse = " - "),
    "m\n")

# vertical exaggeration, which the caption must state
vex <- (diff(range(prof$elev)) / (max(prof$dist_km) * 1000)) ^ -1 *
       (FIG_H_MM / FIG_W_MM)
cat(sprintf("approximate vertical exaggeration at %.0f x %.0f mm: %.0f x\n",
            FIG_W_MM, FIG_H_MM, vex))

cat("\nsite elevation: DEM (plotted) vs table elev_m\n")
print(as.data.frame(route_sites |> st_drop_geometry() |>
        transmute(code, transect, dist_km = round(dist_km, 2),
                  dem = round(dem_elev), table = elev_m,
                  diff = round(dem_elev - elev_m))), row.names = FALSE)

# ---- transect brackets ------------------------------------------------------
brackets <- route_sites |>
  st_drop_geometry() |>
  group_by(transect) |>
  summarise(x0 = min(dist_km), x1 = max(dist_km), .groups = "drop")

y_top <- max(prof$elev)
y_bot <- min(prof$elev)
pad   <- diff(c(y_bot, y_top))
br_y  <- y_top + 0.16 * pad

# ==============================================================================
# Plot
# ==============================================================================
fig_theme <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_line(color = "#CFD3D8", linewidth = 0.3,
                                      linetype = "22"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border     = element_rect(color = "#202124", fill = NA, linewidth = 0.5),
    axis.ticks       = element_line(color = "#202124", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(color = "#303238", size = 8),
    legend.title     = element_text(size = 8.5),
    legend.text      = element_text(size = 8.5),
    legend.key       = element_blank(),
    legend.key.size  = unit(10, "pt"),
    legend.position  = "right",
    plot.background  = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

p <- ggplot() +
  geom_ribbon(data = prof, aes(dist_km, ymin = y_bot - 0.1 * pad, ymax = elev),
              fill = ground_fill) +
  geom_line(data = prof, aes(dist_km, elev),
            colour = ground_line, linewidth = 0.35) +
  # transect brackets
  geom_segment(data = brackets, aes(x = x0, xend = x1, y = br_y, yend = br_y),
               colour = "#7A7468", linewidth = 0.35) +
  geom_segment(data = brackets, aes(x = x0, xend = x0,
                                    y = br_y, yend = br_y - 0.03 * pad),
               colour = "#7A7468", linewidth = 0.35) +
  geom_segment(data = brackets, aes(x = x1, xend = x1,
                                    y = br_y, yend = br_y - 0.03 * pad),
               colour = "#7A7468", linewidth = 0.35) +
  geom_text(data = brackets, aes((x0 + x1) / 2, br_y, label = transect),
            vjust = -0.6, size = 2.4, colour = "#5A5750") +
  # hairline from the DEM ground up (or down) to the recorded site elevation
  # dashed, so it is not mistaken for the solid ggrepel label leader above
  geom_segment(data = route_sites,
               aes(x = dist_km, xend = dist_km, y = dem_elev, yend = elev_m),
               colour = "#6E675C", linewidth = 0.35, linetype = "12") +
  geom_point(data = route_sites,
             aes(dist_km, elev_m, fill = basin, shape = geomorph),
             size = 2.2, colour = "white", stroke = 0.4) +
  ggrepel::geom_text_repel(
    data = route_sites, aes(dist_km, elev_m, label = code),
    size = 2.0, fontface = "bold", colour = label_col,
    bg.color = grDevices::adjustcolor("white", alpha.f = 0.8), bg.r = 0.12,
    seed = 11, max.overlaps = Inf, direction = "y", nudge_y = 0.10 * pad,
    force = 3, box.padding = 0.16, point.padding = 0.1,
    min.segment.length = 0, segment.color = "grey55", segment.size = 0.2) +
  scale_fill_manual(
    values = basin_cols, name = "Basin",
    guide = guide_legend(order = 1,
      override.aes = list(shape = 21, size = 2.6, colour = "white", stroke = 0.45))) +
  scale_shape_manual(
    values = geomorph_shapes, name = "Geomorphic position", drop = FALSE,
    guide = guide_legend(order = 2,
      override.aes = list(fill = "grey45", colour = "white", size = 2.6,
                          stroke = 0.45))) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     labels = function(x) paste0(x, if (max(x, na.rm = TRUE) > 0) "" else "")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = "Distance along traverse (km)", y = "Elevation (m a.s.l.)") +
  fig_theme

ggsave(file.path(fig_dir, "fig_traverse_profile.png"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(fig_dir, "fig_traverse_profile.pdf"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
cat("\nFig. written to", file.path(fig_dir, "fig_traverse_profile.png"), "\n")
