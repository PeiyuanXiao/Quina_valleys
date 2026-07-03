## 01_map_tiles.R — ggmap-style terrain basemap WITHOUT a Google API key.
## Reproduces the "terrain tiles + red points + angled labels" effect using
## OpenTopoMap tiles (via maptiles). Keeps sf points, excavation stars, scale
## bar, north arrow, and the location inset. Tiles are reprojected to UTM 47N
## so the scale bar is accurate, and cached so re-runs need no network.
##
## ggmap reference effect:
##   ggmap(get_map(..., maptype="terrain", source="google")) +
##     geom_point(color="red", size=4) +
##     geom_text(aes(label=name), angle=60, hjust=0, color="yellow")

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
library(patchwork)
sf::sf_use_s2(FALSE)

if (!requireNamespace("maptiles", quietly = TRUE)) install.packages("maptiles")

proj_dir   <- "H:/Quina_valleys"
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- sites (Site_information.xlsx = source of truth; drop PJDD/ZKZ -> 27) ----
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>
  mutate(basin = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing"))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sites_utm <- st_transform(sites, 32647)
anchor    <- subset(sites_utm, code %in% c("LT", "THC"))

## ---- OpenTopoMap tiles (cached as GeoTIFF in UTM 47N) ----
tiles_tif <- file.path(cache_dir, "tiles_opentopomap.tif")
if (file.exists(tiles_tif)) {
  tiles <- terra::rast(tiles_tif)
  message("Using cached OpenTopoMap tiles.")
} else {
  message("Fetching OpenTopoMap tiles via maptiles (one-time download) ...")
  area  <- st_buffer(sites_utm, 6000)                 # ~6 km around the sites
  tiles <- maptiles::get_tiles(area, provider = "OpenTopoMap", zoom = 13,
                               crop = TRUE, cachedir = cache_dir)
  tiles <- terra::project(tiles, "EPSG:32647", method = "near")
  terra::writeRaster(tiles, tiles_tif, overwrite = TRUE)
}

## ---- optional cached rivers + approximate river-name labels (sf) ----
rivers <- if (file.exists(file.path(cache_dir, "rivers.gpkg")))
  st_read(file.path(cache_dir, "rivers.gpkg"), quiet = TRUE) |> st_transform(32647) else NULL

river_labels <- st_as_sf(
  data.frame(label = c("Sangyuan R.", "Liandong R.", "Caifeng R."),
             lon   = c(100.545, 100.495, 100.430),
             lat   = c(25.985,  25.875,  26.010)),
  coords = c("lon", "lat"), crs = 4326) |>
  st_transform(32647)

## ---- map (ggmap aesthetic) ----
p <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = tiles, maxcell = 5e6)
if (!is.null(rivers)) {
  p <- p + geom_sf(data = rivers, color = "#2c7fb8", linewidth = 0.4)
}
p <- p +
  geom_sf(data = sites_utm, color = "red", size = 3) +
  geom_sf(data = anchor, shape = 8, size = 5, color = "black", stroke = 1.1) +
  ## angled labels in the ggmap style (leading spaces offset the text off the dot).
  ## Dark text reads on the light OpenTopoMap background; use "yellow" for Google.
  geom_sf_text(data = sites_utm, aes(label = paste0("  ", code)),
               angle = 60, hjust = 0, size = 2.6, color = "grey10",
               fontface = "bold")
if (!is.null(rivers)) {
  p <- p + geom_sf_text(data = river_labels, aes(label = label),
                        color = "#225e8a", fontface = "italic", size = 3)
}
p <- p +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm")) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Quina sites — OpenTopoMap terrain basemap",
       caption = paste("Basemap (C) OpenTopoMap (CC-BY-SA), map data (C) OpenStreetMap",
                       "contributors, SRTM. Stars = LT (Longtan), THC (Tianhua Cave).")) +
  theme_bw(base_size = 11) +
  theme(plot.caption = element_text(size = 7, hjust = 0))

## ---- location inset (reuses cached admin boundaries if present) ----
china  <- if (file.exists(file.path(cache_dir, "china.gpkg")))
  st_read(file.path(cache_dir, "china.gpkg"), quiet = TRUE) else NULL
yunnan <- if (file.exists(file.path(cache_dir, "yunnan.gpkg")))
  st_read(file.path(cache_dir, "yunnan.gpkg"), quiet = TRUE) else NULL

final <- p
if (!is.null(china)) {
  box <- st_buffer(sites_utm, 6000) |> st_bbox() |> st_as_sfc() |> st_transform(4326)
  inset <- ggplot() +
    geom_sf(data = china, fill = "grey92", color = "grey60", linewidth = 0.2)
  if (!is.null(yunnan)) {
    inset <- inset + geom_sf(data = yunnan, fill = "grey75", color = "grey50",
                             linewidth = 0.2)
  }
  inset <- inset +
    geom_sf(data = box, fill = NA, color = "red", linewidth = 0.7) +
    coord_sf(xlim = c(97, 123), ylim = c(20, 42), expand = FALSE) +
    annotate("text", x = 101, y = 33, label = "SE Tibetan\nPlateau margin",
             size = 2.6, lineheight = 0.9) +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = "grey40"))
  final <- p + patchwork::inset_element(inset, left = 0.0, bottom = 0.0,
                                        right = 0.32, top = 0.32)
}

## ---- export (separate names so the DEM-hillshade map is not overwritten) ----
ggsave(file.path(output_dir, "map_quina_sites_terrain.pdf"), final, width = 9, height = 9)
ggsave(file.path(output_dir, "map_quina_sites_terrain.png"), final, width = 9, height = 9,
       dpi = 300)
message("01_map_tiles.R done -> output/map_quina_sites_terrain.(pdf|png)")
