## 00b_rivers_from_dem.R — derive a stream network from the cached DEM.
## OSM (osmdata) returned no waterways for these valleys, so rivers are extracted
## hydrologically from data/cache/dem.tif via WhiteboxTools. Also tries to fetch
## lake/water polygons from OSM. Run once after 00_setup.R; 01_map.R then picks
## up data/cache/rivers_dem.gpkg (and water.gpkg) automatically.
##
## BEST alternative: if you have a river shapefile (e.g. from the thesis GIS),
## skip this and set `rivers_local_path` in 01_map.R — that is cleaner than a
## DEM-derived network.

library(sf)
library(terra)

proj_dir  <- here::here()
cache_dir <- file.path(proj_dir, "data", "cache")
dem_path  <- file.path(cache_dir, "dem.tif")
stopifnot(file.exists(dem_path))

if (!requireNamespace("whitebox", quietly = TRUE)) install.packages("whitebox")
library(whitebox)
if (!whitebox::check_whitebox_binary()) {
  message("Installing WhiteboxTools binary (one-time, ~70 MB) ...")
  whitebox::install_whitebox()
}

## intermediate rasters (underscore-prefixed; safe to delete afterwards)
filled <- file.path(cache_dir, "_dem_filled.tif")
d8ptr  <- file.path(cache_dir, "_d8_pointer.tif")
d8acc  <- file.path(cache_dir, "_d8_accum.tif")
strast <- file.path(cache_dir, "_streams.tif")
strshp <- file.path(cache_dir, "_streams.shp")

## stream extraction threshold = minimum upstream contributing cells.
## Larger -> fewer, larger rivers only; smaller -> denser drainage. Tune to taste.
stream_threshold <- 1500

wbt_fill_depressions(dem = dem_path, output = filled)
wbt_d8_pointer(dem = filled, output = d8ptr)
wbt_d8_flow_accumulation(input = filled, output = d8acc, out_type = "cells")
wbt_extract_streams(flow_accum = d8acc, output = strast, threshold = stream_threshold)
wbt_raster_streams_to_vector(streams = strast, d8_pntr = d8ptr, output = strshp)

rivers <- sf::st_read(strshp, quiet = TRUE)
if (is.na(sf::st_crs(rivers))) sf::st_crs(rivers) <- terra::crs(terra::rast(dem_path))
rivers <- sf::st_transform(rivers, 4326)
sf::st_write(rivers, file.path(cache_dir, "rivers_dem.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)
message("Wrote rivers_dem.gpkg (", nrow(rivers), " line features). ",
        "Adjust `stream_threshold` if there are too many/few channels.")

## ---- optional: OSM lake / reservoir polygons -----------------------------
## (Often empty here — the Heqing palaeolake is drained and the valleys are dry.)
if (requireNamespace("osmdata", quietly = TRUE) &&
    file.exists(file.path(cache_dir, "bbox_4326.rds"))) {
  ok <- tryCatch({
    bb   <- readRDS(file.path(cache_dir, "bbox_4326.rds"))
    wat  <- osmdata::opq(bbox = bb) |>
      osmdata::add_osm_feature("natural", "water") |>
      osmdata::osmdata_sf()
    poly <- wat$osm_polygons
    if (!is.null(poly) && nrow(poly) > 0) {
      sf::st_write(poly["osm_id"], file.path(cache_dir, "water.gpkg"),
                   delete_dsn = TRUE, quiet = TRUE)
      message("Wrote water.gpkg (", nrow(poly), " polygons).")
    } else {
      message("No OSM water polygons in the area (expected — valleys are dry).")
    }
    TRUE
  }, error = function(e) { message("OSM water fetch failed: ", conditionMessage(e)); FALSE })
}
