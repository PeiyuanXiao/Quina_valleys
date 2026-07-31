## setup.R — everything the Figure 1 panel scripts read: the packages, the clean
## site table, and the cached spatial layers (DEM and hillshade, the channel
## network, the administrative boundaries for the location inset).
## Run order:  setup.R  ->  terra_map_2D.R (A) / terra_map_3D.R (B) /
##                          traverse_profile_figure.R (C)  ->  fig01_export_panels.R
##
## Every download and every derived layer is written to data/cache/ and skipped
## when it is already there, so this script is cheap to re-run and the panel
## scripts then need no network at all. To rebuild one layer, delete its file
## from data/cache/ and run this again.
##
## data/cache/dem.tif is also read from outside this folder, by
## scripts/figures/liandong_section_figure.R.
##
## Rivers come from whichever source is available, in this order of preference —
## the same order terra_map_2D.R reads them in:
##   1. a real river shapefile, if `rivers_local_path` below points at one (best)
##   2. OSM waterways           -> rivers.gpkg      (part 4; empty in these valleys)
##   3. the DEM, hydrologically -> rivers_dem.gpkg  (part 6; what the figure uses)

## ---- packages ------------------------------------------------------------
## whitebox is deliberately not in this list: it carries a ~70 MB binary, so it
## is installed in part 6 only if the channel network actually has to be built.
pkgs <- c(
  "sf", "terra", "tidyterra", "elevatr", "ggplot2", "ggspatial",
  "rnaturalearth", "rnaturalearthdata",
  "dplyr", "readr", "readxl", "patchwork", "viridis", "osmdata",
  "ggrepel", "ggnewscale"
)
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

library(sf)
library(terra)
library(dplyr)
library(readxl)
sf::sf_use_s2(FALSE)            # planar ops are fine for this small study area

## ---- paths ---------------------------------------------------------------
proj_dir   <- here::here()
site_xlsx  <- file.path(proj_dir, "data", "Site_information.xlsx")
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- switches ------------------------------------------------------------
## A local line shapefile (e.g. from the thesis GIS) overrides both the OSM
## download and the DEM extraction, and is cleaner than either.
rivers_local_path <- NA_character_   # e.g. here::here("data_raw", "rivers.shp")

## Stream-extraction threshold, as a minimum number of upstream contributing
## cells: larger keeps only the bigger rivers, smaller gives a denser net.
stream_threshold <- 1500

## ---- 1. sites (Site_information.xlsx = source of truth) ------------------
## The 27 Quina-bearing localities, all of which are analysed. The xlsx
## `geomorph` column was previously corrupted (mojibake) and has been repaired
## in-file to the canonical T2/T3/T4/hilltop tokens; headers carry trailing
## spaces so are trimmed on read, and `basin` is stored as "<name> basin", so
## the " basin" suffix is stripped.
sites <- readxl::read_excel(site_xlsx)
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = c("T2", "T3", "T4", "hilltop"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

sites_utm <- st_transform(sites, 32647)   # EPSG:32647 (UTM 47N), metres

cat("Sites loaded:\n")
print(table(sites$basin, sites$geomorph))

## ---- 2. download frame: site extent + 8 km -------------------------------
## Cached as well as held in memory: it is the only record of the exact frame
## the downloads below were made on.
bb    <- st_bbox(st_buffer(sites_utm, 8000)) |> st_as_sfc() |> st_transform(4326)
bb_sf <- st_as_sf(bb)
saveRDS(st_bbox(bb), file.path(cache_dir, "bbox_4326.rds"))

## ---- 3. DEM + hillshade (elevatr) ----------------------------------------
dem_tif  <- file.path(cache_dir, "dem.tif")
hill_tif <- file.path(cache_dir, "hillshade.tif")
if (!file.exists(dem_tif) || !file.exists(hill_tif)) {
  message("Downloading DEM via elevatr ...")
  ok <- tryCatch({
    dem <- elevatr::get_elev_raster(bb_sf, z = 11, clip = "bbox") |> terra::rast()
    names(dem) <- "elev"
    slope  <- terra::terrain(dem, "slope",  unit = "radians")
    aspect <- terra::terrain(dem, "aspect", unit = "radians")
    hill   <- terra::shade(slope, aspect, angle = 45, direction = 315)
    names(hill) <- "hillshade"
    terra::writeRaster(dem,  dem_tif,  overwrite = TRUE)
    terra::writeRaster(hill, hill_tif, overwrite = TRUE)
    TRUE
  }, error = function(e) { message("  DEM download failed: ", conditionMessage(e)); FALSE })
  if (!ok) message("  -> nothing downstream can run without the DEM: part 6 and ",
                   "both map scripts will stop until this succeeds.")
} else {
  message("Using cached DEM/hillshade.")
}

## ---- 4. rivers from OSM (a local shapefile takes precedence) -------------
## Kept because it costs one request and would be the better source anywhere OSM
## has coverage; here it returns nothing, and part 6 supplies the channels.
rivers_gpkg <- file.path(cache_dir, "rivers.gpkg")
if (!file.exists(rivers_gpkg)) {
  if (!is.na(rivers_local_path) && file.exists(rivers_local_path)) {
    message("Reading rivers from local file: ", rivers_local_path)
    rivers <- st_read(rivers_local_path, quiet = TRUE) |> st_transform(4326)
    st_write(rivers, rivers_gpkg, delete_dsn = TRUE, quiet = TRUE)
  } else {
    message("Downloading rivers via osmdata ...")
    ok <- tryCatch({
      osm <- osmdata::opq(bbox = st_bbox(bb)) |>
        osmdata::add_osm_feature("waterway", c("river", "stream", "canal")) |>
        osmdata::osmdata_sf()
      rivers <- osm$osm_lines
      if (is.null(rivers) || nrow(rivers) == 0) stop("no waterways returned")
      rivers <- rivers["osm_id"]
      st_write(rivers, rivers_gpkg, delete_dsn = TRUE, quiet = TRUE)
      TRUE
    }, error = function(e) { message("  river download failed: ", conditionMessage(e)); FALSE })
    if (!ok) message("  -> expected here; part 6 extracts the channels from the DEM.")
  }
} else {
  message("Using cached rivers.")
}

## ---- 5. admin boundaries for the location inset (rnaturalearth) ----------
china_gpkg  <- file.path(cache_dir, "china.gpkg")
yunnan_gpkg <- file.path(cache_dir, "yunnan.gpkg")
if (!file.exists(china_gpkg) || !file.exists(yunnan_gpkg)) {
  message("Downloading admin boundaries via rnaturalearth ...")
  ok <- tryCatch({
    china <- rnaturalearth::ne_countries(country = "China", scale = "medium",
                                         returnclass = "sf")
    prov  <- rnaturalearth::ne_states(country = "China", returnclass = "sf")
    nm    <- intersect(c("name_en", "name", "gn_name", "woe_name"), names(prov))[1]
    yunnan <- prov[grepl("Yunnan", prov[[nm]], ignore.case = TRUE), ]
    st_write(china,  china_gpkg,  delete_dsn = TRUE, quiet = TRUE)
    st_write(yunnan, yunnan_gpkg, delete_dsn = TRUE, quiet = TRUE)
    TRUE
  }, error = function(e) { message("  admin download failed: ", conditionMessage(e)); FALSE })
  if (!ok) message("  -> location inset will be skipped.")
} else {
  message("Using cached admin boundaries.")
}

## ---- 6. channel network from the DEM (WhiteboxTools) ---------------------
## OSM returns no waterways for these valleys, so the channels the maps draw are
## extracted hydrologically from the cached DEM: fill depressions -> D8 flow
## directions -> flow accumulation -> threshold -> vectorise.
##
## The intermediates are kept rather than cleaned up: terra_map_2D.R reads
## _d8_accum.tif to rank each channel by the upstream area draining through it,
## which is how the map keeps the trunk rivers and drops the hillslope rills.
## They are therefore part of the cache, not scratch files, and the whole chain
## is skipped only when both the vector network and that raster are present.
rivers_dem_gpkg <- file.path(cache_dir, "rivers_dem.gpkg")
filled <- file.path(cache_dir, "_dem_filled.tif")
d8ptr  <- file.path(cache_dir, "_d8_pointer.tif")
d8acc  <- file.path(cache_dir, "_d8_accum.tif")
strast <- file.path(cache_dir, "_streams.tif")
strshp <- file.path(cache_dir, "_streams.shp")

if (!file.exists(dem_tif)) {
  message("No DEM in the cache — skipping the channel network.")
} else if (file.exists(rivers_dem_gpkg) && file.exists(d8acc)) {
  message("Using cached DEM-derived channel network.")
} else {
  if (!requireNamespace("whitebox", quietly = TRUE)) install.packages("whitebox")
  library(whitebox)
  if (!whitebox::check_whitebox_binary()) {
    message("Installing WhiteboxTools binary (one-time, ~70 MB) ...")
    whitebox::install_whitebox()
  }

  message("Extracting the channel network from the DEM ...")
  wbt_fill_depressions(dem = dem_tif, output = filled)
  wbt_d8_pointer(dem = filled, output = d8ptr)
  wbt_d8_flow_accumulation(input = filled, output = d8acc, out_type = "cells")
  wbt_extract_streams(flow_accum = d8acc, output = strast, threshold = stream_threshold)
  wbt_raster_streams_to_vector(streams = strast, d8_pntr = d8ptr, output = strshp)

  rivers_dem <- sf::st_read(strshp, quiet = TRUE)
  if (is.na(sf::st_crs(rivers_dem)))
    sf::st_crs(rivers_dem) <- terra::crs(terra::rast(dem_tif))
  rivers_dem <- sf::st_transform(rivers_dem, 4326)
  sf::st_write(rivers_dem, rivers_dem_gpkg, delete_dsn = TRUE, quiet = TRUE)
  message("Wrote rivers_dem.gpkg (", nrow(rivers_dem), " line features). ",
          "Adjust `stream_threshold` if there are too many/few channels.")
}

## ---- 7. lake / reservoir polygons from OSM (optional) -------------------
## Often empty here — the Heqing palaeolake is drained and the valleys are dry —
## but both map scripts draw water.gpkg if it exists.
water_gpkg <- file.path(cache_dir, "water.gpkg")
if (!file.exists(water_gpkg)) {
  ok <- tryCatch({
    wat  <- osmdata::opq(bbox = st_bbox(bb)) |>
      osmdata::add_osm_feature("natural", "water") |>
      osmdata::osmdata_sf()
    poly <- wat$osm_polygons
    if (!is.null(poly) && nrow(poly) > 0) {
      sf::st_write(poly["osm_id"], water_gpkg, delete_dsn = TRUE, quiet = TRUE)
      message("Wrote water.gpkg (", nrow(poly), " polygons).")
    } else {
      message("No OSM water polygons in the area (expected — the valleys are dry).")
    }
    TRUE
  }, error = function(e) { message("OSM water fetch failed: ", conditionMessage(e)); FALSE })
} else {
  message("Using cached water polygons.")
}

message("\nsetup.R done. Cache: ", cache_dir)
