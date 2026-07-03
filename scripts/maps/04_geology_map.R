## 04_geology_map.R — simplified GEOLOGICAL map of the Binchuan & Heqing basins,
## built to match 01_map.R (same hillshade base, water, basin hulls, sites, fonts).
##
## Purpose: give the raw-material argument a bedrock context. The tool assemblages
## are dominated by trachyte (Cenozoic volcanic rock) while the basin gravels are
## dominated by sandstone (Mesozoic red beds); andesite/hornfels are read as
## non-local. A geology map makes that availability story legible.
##
## DATA SOURCES & HONESTY NOTE -------------------------------------------------
##  * Quaternary basin-fill / river-terrace extent is DERIVED FROM THE SRTM DEM
##    (low-slope, low-elevation valley floor) — reproducible, and it is exactly
##    the surface on which the artefacts were collected.
##  * BEDROCK units (Neogene volcanics, Mesozoic red beds, Palaeozoic carbonate /
##    basement) and the fault trace are SCHEMATIC polygons generalised from
##    regional 1:2.5 M geology and published accounts of the Binchuan Cenozoic
##    potassic volcanic field. Their boundaries are APPROXIMATE and meant as
##    context, not as surveyed contacts. They live in clearly-marked, editable
##    blocks below. To make a publication-final version, either edit the vertices
##    or set `geology_gis_path` to a real geological shapefile (see that switch).
## -----------------------------------------------------------------------------

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
sf::sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

proj_dir   <- "H:/Quina_valleys"
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- switches --------------------------------------------------------------
show_site_labels <- TRUE
geology_gis_path <- NA_character_   # set to a real geol. shapefile to override the
                                    # schematic bedrock (it must have a lithology/unit
                                    # column; see the `if (!is.na(...))` block below)

## Quaternary basin-fill derivation thresholds (see honesty note).
qt_slope_deg <- 4      # valley floors are flat (tighter = confined to true alluvium)
qt_elev_max  <- 1900   # below the mountain fronts (sites span 1359-1805 m)

## ---- palette (geological, tuned to harmonise with the raw-material chart) ---
## Raw-material chart uses Okabe-Ito: Trachyte #D55E00, Sandstone #E69F00 ...
## so volcanics echo the trachyte colour and red beds echo the sandstone colour.
geo_levels <- c(
  "Quaternary alluvium & terraces",
  "Neogene volcanic rocks (trachyte, latite)",
  "Mesozoic red beds (sandstone, mudstone)",
  "Palaeozoic carbonate & basement"
)
geo_cols <- c(
  "Quaternary alluvium & terraces"             = "#F2E3B3",  # pale yellow
  "Neogene volcanic rocks (trachyte, latite)"  = "#CF6A45",  # muted vermillion
  "Mesozoic red beds (sandstone, mudstone)"    = "#BD7B4C",  # reddish brown (red beds)
  "Palaeozoic carbonate & basement"            = "#8DA7B3"   # carbonate blue-grey
)
basin_cols  <- c(Binchuan = "#8E2F39", Heqing = "#23506E")
water_col   <- "#5E86A0"
fault_col   <- "#2B2B2B"
label_col   <- "#3A352E"

## ---- sites (mirror 01_map.R exactly so the two maps agree) ------------------
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

hulls <- sites |>
  group_by(basin) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  st_convex_hull() |>
  st_transform(32647) |> st_buffer(900) |> st_transform(4326)

## ---- DEM + hillshade (same recipe as 01_map.R) -----------------------------
read_if <- function(f, reader) if (file.exists(f)) reader(f) else NULL
dem <- read_if(file.path(cache_dir, "dem.tif"), terra::rast)
stopifnot(!is.null(dem))

z_exag <- 1.8
dem_z  <- dem * z_exag
slope_r  <- terra::terrain(dem_z, "slope",  unit = "radians")
aspect_r <- terra::terrain(dem_z, "aspect", unit = "radians")
hl   <- lapply(c(300, 337, 15),
               function(d) terra::shade(slope_r, aspect_r, angle = 35, direction = d))
hill <- terra::app(terra::rast(hl), mean)
hr   <- as.numeric(stats::quantile(terra::values(hill, mat = FALSE),
                                   c(0.02, 0.98), na.rm = TRUE))
hill <- (terra::clamp(hill, hr[1], hr[2]) - hr[1]) / (hr[2] - hr[1])
names(hill) <- "hillshade"

bb_poly <- st_as_sfc(st_bbox(dem)) |> st_set_crs(4326)

## ---- water + rivers (from cache, same as 01_map.R) -------------------------
rivers <- read_if(file.path(cache_dir, "rivers.gpkg"),
                  function(f) st_read(f, quiet = TRUE)) %||%
          read_if(file.path(cache_dir, "rivers_dem.gpkg"),
                  function(f) st_read(f, quiet = TRUE))
lakes  <- read_if(file.path(cache_dir, "water.gpkg"),
                  function(f) st_read(f, quiet = TRUE))

## ===========================================================================
## (A) QUATERNARY basin-fill  —  DERIVED FROM THE DEM (reproducible)
## ===========================================================================
quat <- tryCatch({
  slope_deg <- terra::terrain(dem, "slope", unit = "degrees")
  m  <- terra::ifel(slope_deg < qt_slope_deg & dem < qt_elev_max, 1, NA)
  qp <- terra::as.polygons(m, dissolve = TRUE) |> st_as_sf() |> st_make_valid()
  qp <- st_transform(st_union(qp), 32647)
  qp <- st_buffer(st_buffer(qp, 250), -250)          # morphological close (fill gaps)
  qp <- st_simplify(qp, dTolerance = 150)            # smooth jagged cell edges
  parts <- suppressWarnings(st_cast(qp, "POLYGON"))
  sites_utm <- st_transform(sites, 32647)
  near <- st_buffer(st_convex_hull(st_union(sites_utm)), 6000)
  keep <- lengths(st_intersects(parts, near)) > 0 &
          as.numeric(st_area(parts)) > 3e5           # drop specks (<0.3 km^2)
  st_sf(unit = factor(geo_levels[1], levels = geo_levels),
        geometry = st_transform(st_union(parts[keep]), 4326))
}, error = function(e) {
  message("  Quaternary DEM derivation failed (", conditionMessage(e),
          "); falling back to basin hulls.")
  st_sf(unit = factor(geo_levels[1], levels = geo_levels),
        geometry = st_geometry(st_union(hulls)))
})

## ===========================================================================
## (B) BEDROCK units  —  SCHEMATIC, generalised from regional geology.
##     EDIT THESE VERTICES (lon, lat) to refine, or override via geology_gis_path.
## ===========================================================================
mk <- function(coords, unit)
  st_sf(unit = factor(unit, levels = geo_levels),
        geometry = st_sfc(st_polygon(list(rbind(coords, coords[1, , drop = FALSE]))),
                          crs = 4326))

if (!is.na(geology_gis_path) && file.exists(geology_gis_path)) {
  ## --- real GIS override: expects a column you map to `unit` (edit as needed) -
  bedrock <- st_read(geology_gis_path, quiet = TRUE) |> st_transform(4326) |>
    st_make_valid()
  ## TODO: recode the source's lithology field into `geo_levels`, e.g.
  ## bedrock <- bedrock |> mutate(unit = factor(recode(LITHOLOGY, ...), levels = geo_levels))
  bedrock <- st_intersection(bedrock, bb_poly)
} else {
  ## --- schematic framework (NW = older basement/carbonate; centre-E = Mesozoic
  ##     red beds; SE near Binchuan = Neogene volcanics). Boundaries approximate. -
  basement <- mk(matrix(c(
    100.305, 25.880,  100.640, 26.130,  100.305, 26.130), ncol = 2, byrow = TRUE),
    "Palaeozoic carbonate & basement")
  volcanics <- mk(matrix(c(
    100.455, 25.735,  100.640, 25.735,  100.640, 26.000,
    100.560, 26.010,  100.470, 25.900,  100.452, 25.800), ncol = 2, byrow = TRUE),
    "Neogene volcanic rocks (trachyte, latite)")
  red_full <- mk(matrix(c(
    100.305, 25.735,  100.640, 25.735,  100.640, 26.130,
    100.305, 26.130), ncol = 2, byrow = TRUE),
    "Mesozoic red beds (sandstone, mudstone)")
  basement  <- st_intersection(basement,  bb_poly)
  volcanics <- st_intersection(volcanics, bb_poly)
  ## red beds = whole frame minus the other two bedrock zones
  redbeds <- st_difference(red_full, st_union(st_geometry(basement),
                                              st_geometry(volcanics)))
  bedrock <- rbind(basement, volcanics, redbeds)
}

## Quaternary (real) overrides bedrock where the valley floors are.
bedrock <- suppressWarnings(st_difference(bedrock, st_union(st_geometry(quat))))
geology <- rbind(bedrock[, "unit"], quat[, "unit"]) |> st_make_valid()

## ===========================================================================
## (C) FAULT trace — SCHEMATIC (generalised Chenghai fault zone). Edit freely.
## ===========================================================================
faults <- st_sf(
  name = "Chenghai Fault zone (generalised)",
  geometry = st_sfc(st_linestring(matrix(c(
    100.395, 26.125,  100.420, 25.990,  100.445, 25.880,
    100.470, 25.800,  100.485, 25.735), ncol = 2, byrow = TRUE)), crs = 4326))

## ===========================================================================
## (D) BUILD MAP
## ===========================================================================
p <- ggplot() +
  ## geology fill
  geom_sf(data = geology, aes(fill = unit), color = "grey35",
          linewidth = 0.12, alpha = 0.92) +
  scale_fill_manual(values = geo_cols, breaks = geo_levels, name = "Geology",
                    drop = FALSE,
                    guide = guide_legend(order = 1, override.aes = list(alpha = 1))) +
  ggnewscale::new_scale_fill() +
  ## relief on top (translucent, gives the geology a hillshaded texture)
  tidyterra::geom_spatraster(data = hill, alpha = 0.32, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  ggnewscale::new_scale_fill()

## faults
p <- p +
  geom_sf(data = faults, color = fault_col, linewidth = 0.75, alpha = 0.9) +
  geom_sf(data = faults, color = fault_col, linewidth = 0.75, alpha = 0.9,
          linetype = "11")   # subtle dashed overlay reads as a fault symbol

## water
if (!is.null(lakes))
  p <- p + geom_sf(data = lakes, fill = water_col, color = water_col,
                   linewidth = 0.2, alpha = 0.8)
if (!is.null(rivers))
  p <- p + geom_sf(data = rivers, color = water_col, linewidth = 0.45, alpha = 0.95)

## basin hulls (faint, like 01_map.R)
p <- p +
  geom_sf(data = hulls, aes(color = basin), fill = NA, linewidth = 0.55,
          linetype = "22", show.legend = FALSE)

## sites (fixed size; geology + basin are the message here)
p <- p +
  geom_sf(data = sites, aes(fill = basin), size = 2.7,
          shape = 21, color = "white", stroke = 0.5, alpha = 0.98) +
  geom_sf(data = anchor, shape = 8, size = 5.0, color = "white", stroke = 1.5) +
  geom_sf(data = anchor, shape = 8, size = 3.9, color = label_col, stroke = 0.9) +
  scale_fill_manual(values = basin_cols, name = "Basin",
                    guide = guide_legend(order = 2,
                            override.aes = list(shape = 21, size = 3))) +
  scale_color_manual(values = basin_cols, guide = "none")

if (show_site_labels)
  p <- p + ggrepel::geom_text_repel(
    data = sites, aes(geometry = geometry, label = code),
    stat = "sf_coordinates", size = 2.4, fontface = "bold", color = label_col,
    bg.color = "white", bg.r = 0.15, max.overlaps = 30,
    min.segment.length = 0, segment.color = "grey55", segment.size = 0.2)

## fault label
p <- p + ggrepel::geom_text_repel(
  data = faults, aes(geometry = geometry, label = name),
  stat = "sf_coordinates", size = 2.5, fontface = "italic", color = fault_col,
  bg.color = "white", bg.r = 0.12, nudge_x = -0.03, segment.size = 0.2)

p <- p +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm")) +
  coord_sf(xlim = st_bbox(dem)[c("xmin", "xmax")],
           ylim = st_bbox(dem)[c("ymin", "ymax")], expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Simplified geology of the Binchuan and Heqing basins",
       subtitle = "Bedrock context for raw-material availability in the Quina valleys",
       caption = paste0(
         "SCHEMATIC. Bedrock units & fault generalised from regional 1:2.5 M geology and ",
         "published accounts of the Binchuan Cenozoic\npotassic volcanic field; contacts approximate. ",
         "Quaternary basin-fill derived from the SRTM DEM (slope < ", qt_slope_deg,
         "°, elevation < ", qt_elev_max, " m).\n",
         "Stars = LT (Longtan), THC (Tianhua Cave).")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        panel.grid = element_line(color = grey(0.88), linewidth = 0.15),
        plot.subtitle = element_text(size = 9, color = "grey25"),
        plot.caption = element_text(size = 7, hjust = 0))

## ---- export ----------------------------------------------------------------
ggsave(file.path(output_dir, "map_geology.pdf"), p, width = 9.3, height = 8)
ggsave(file.path(output_dir, "map_geology.png"), p, width = 9.3, height = 8, dpi = 300)
message("04_geology_map.R done -> output/map_geology.(pdf|png)")
