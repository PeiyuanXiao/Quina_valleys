## 01_map.R — clean terrain site map (Morandi palette): hillshade + smoothed
## contours + water + faint basin hulls + sites. Self-rendered from the SRTM DEM
## and a river vector, so it contains NO roads and NO basemap place-names.
##
## Rivers/lakes: loaded from whichever of these exists (in this order):
##   data/cache/rivers.gpkg      (OSM, from 00_setup.R — often empty here)
##   data/cache/rivers_dem.gpkg  (DEM-derived, from 00b_rivers_from_dem.R)
## A local shapefile is best: set rivers_local_path below.

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
sf::sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- display toggles -----------------------------------------------------
show_site_labels  <- TRUE
show_river_labels <- FALSE
rivers_local_path <- NA_character_   # e.g. here::here("data_raw", "rivers.shp") (best)

## ---- Morandi (muted, low-saturation) palette -----------------------------
## higher-contrast but still muted: deeper sage/umber lows, paler stone highs
morandi_dem   <- c("#6F7E64", "#8B977A", "#AEB191", "#CABF9D", "#C2A074",
                   "#A87C57", "#E7E0D2")
basin_cols    <- c(Binchuan = "#8E2F39", Heqing = "#23506E")  # deep, to stand off the basemap
water_col     <- "#5E86A0"                                    # rivers + lakes share one colour
contour_minor <- "#8C857A"; contour_index <- "#6E685D"
label_col     <- "#4A463F"

## ---- sites (Site_information.xlsx = source of truth; drop PJDD/ZKZ -> 27) -
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = c("T2", "T3", "T4", "hilltop"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
anchor <- subset(sites, code %in% c("LT", "THC"))

## faint convex-hull outline per basin (groups the sites visually)
hulls <- sites |>
  group_by(basin) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  st_convex_hull() |>
  st_transform(32647) |> st_buffer(900) |> st_transform(4326)

## ---- cached DEM + rivers + (optional) lakes ------------------------------
read_if <- function(f, reader) if (file.exists(f)) reader(f) else NULL
dem <- read_if(file.path(cache_dir, "dem.tif"), terra::rast)

if (!is.na(rivers_local_path) && file.exists(rivers_local_path)) {
  rivers <- st_read(rivers_local_path, quiet = TRUE) |> st_transform(4326)
} else {
  rivers <- read_if(file.path(cache_dir, "rivers.gpkg"),
                    function(f) st_read(f, quiet = TRUE)) %||%
            read_if(file.path(cache_dir, "rivers_dem.gpkg"),
                    function(f) st_read(f, quiet = TRUE))
}
lakes <- read_if(file.path(cache_dir, "water.gpkg"),
                 function(f) st_read(f, quiet = TRUE))

## hillshade + smoothed DEM (for cleaner contours) + robust colour limits
hill <- dem_s <- dem_lims <- NULL
if (!is.null(dem)) {
  ## vertical exaggeration + low sun + multidirectional -> stronger 3-D relief
  z_exag <- 1.8
  dem_z  <- dem * z_exag
  slope  <- terra::terrain(dem_z, "slope",  unit = "radians")
  aspect <- terra::terrain(dem_z, "aspect", unit = "radians")
  hl   <- lapply(c(300, 337, 15),
                 function(d) terra::shade(slope, aspect, angle = 35, direction = d))
  hill <- terra::app(terra::rast(hl), mean)
  ## contrast-stretch the hillshade (2-98%): deeper shadows, brighter highlights
  hr   <- as.numeric(stats::quantile(terra::values(hill, mat = FALSE),
                                     c(0.02, 0.98), na.rm = TRUE))
  hill <- (terra::clamp(hill, hr[1], hr[2]) - hr[1]) / (hr[2] - hr[1])
  names(hill) <- "hillshade"

  dem_s    <- terra::focal(dem, w = 9, fun = "mean", na.rm = TRUE)  # smoother contours
  ## clamp elevation colour ramp to the 2-98% range so the ramp is spent where
  ## the terrain actually is (boosts contrast on the valley floor).
  dem_lims <- as.numeric(stats::quantile(terra::values(dem, mat = FALSE),
                                         c(0.02, 0.98), na.rm = TRUE))
}

## ---- build map -----------------------------------------------------------
p <- ggplot()

if (!is.null(dem)) {
  p <- p +
    tidyterra::geom_spatraster(data = dem) +
    scale_fill_gradientn(colors = morandi_dem, na.value = NA, name = "Elevation (m)",
                         limits = dem_lims, oob = scales::squish) +
    ggnewscale::new_scale_fill()
  if (!is.null(hill)) {
    p <- p +
      tidyterra::geom_spatraster(data = hill, show.legend = FALSE, alpha = 0.50) +
      scale_fill_gradient(low = "black", high = "white", na.value = NA) +
      ggnewscale::new_scale_fill()
  }
  p <- p +
    tidyterra::geom_spatraster_contour(data = dem_s, breaks = seq(1000, 5000, 100),
        color = contour_minor, linewidth = 0.10, alpha = 0.45) +
    tidyterra::geom_spatraster_contour(data = dem_s, breaks = seq(1000, 5000, 500),
        color = contour_index, linewidth = 0.28, alpha = 0.6)
}

## water: lakes (polygons) under rivers (lines)
if (!is.null(lakes)) {
  p <- p + geom_sf(data = lakes, fill = water_col, color = water_col,
                   linewidth = 0.2, alpha = 0.85)
}
if (!is.null(rivers)) {
  p <- p + geom_sf(data = rivers, color = water_col, linewidth = 0.5, alpha = 0.95)
}

## basin hulls (under the points): faint tint + clearer coloured dashed outline
p <- p +
  geom_sf(data = hulls, aes(fill = basin, color = basin), linewidth = 0.55,
          alpha = 0.08, linetype = "22", show.legend = FALSE)

## sites with white halo
p <- p +
  geom_sf(data = sites, aes(fill = basin, size = n_lithics),
          shape = 21, color = "white", stroke = 0.5, alpha = 0.98) +
  geom_sf(data = anchor, shape = 8, size = 5.0, color = "white", stroke = 1.5) +
  geom_sf(data = anchor, shape = 8, size = 3.9, color = label_col, stroke = 0.9) +
  scale_fill_manual(values = basin_cols, name = "Basin",
                    guide = guide_legend(override.aes = list(shape = 21, size = 3))) +
  scale_color_manual(values = basin_cols, guide = "none") +   # hull outlines
  scale_size_continuous(name = "Lithics (n)\n(display only)", range = c(1.8, 7),
                        breaks = c(1, 5, 10, 30, 60))

if (show_site_labels) {
  p <- p + ggrepel::geom_text_repel(
    data = sites, aes(geometry = geometry, label = code),
    stat = "sf_coordinates", size = 2.4, fontface = "bold", color = label_col,
    bg.color = "white", bg.r = 0.15, max.overlaps = 30,
    min.segment.length = 0, segment.color = "grey55", segment.size = 0.2)
}
if (show_river_labels && !is.null(rivers)) {
  river_labels <- data.frame(label = c("Sangyuan R.", "Liandong R.", "Caifeng R."),
                             lon = c(100.545, 100.495, 100.430),
                             lat = c(25.985,  25.875,  26.010))
  p <- p + geom_text(data = river_labels, aes(lon, lat, label = label),
                     inherit.aes = FALSE, color = water_col, fontface = "italic",
                     size = 3)
}

p <- p +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm")) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Quina sites of the Binchuan and Heqing basins",
       caption = paste("Terrain & contours from SRTM (elevatr).",
                       "Stars = LT (Longtan), THC (Tianhua Cave).")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        panel.grid = element_line(color = grey(0.88), linewidth = 0.15),
        plot.caption = element_text(size = 7, hjust = 0))

## ---- export (no inset) ---------------------------------------------------
ggsave(file.path(output_dir, "map_quina_sites.pdf"), p, width = 9, height = 8)
ggsave(file.path(output_dir, "map_quina_sites.png"), p, width = 9, height = 8, dpi = 300)
message("01_map.R done -> output/map_quina_sites.(pdf|png)")
