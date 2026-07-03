## 00_setup.R — packages, clean data, projections, and one-time downloads.
## Run order:  00_setup.R  ->  01_map.R  ->  02_spatial_stats.R
## Downloads (DEM / rivers / admin) are cached in data/cache/ so that 01 and 02
## need no network. Re-running 00 reuses any cache that already exists.

## ---- packages ------------------------------------------------------------
pkgs <- c(
  "sf", "terra", "tidyterra", "elevatr", "ggplot2", "ggspatial",
  "rnaturalearth", "rnaturalearthdata", "spatstat", "spatstat.geom",
  "spatstat.explore", "dplyr", "readr", "readxl", "patchwork", "viridis", "osmdata",
  "ggrepel", "ggnewscale", "maptiles"
)
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}

library(sf)
library(dplyr)
library(readxl)
sf::sf_use_s2(FALSE)            # planar ops are fine for this small study area

## ---- paths ---------------------------------------------------------------
proj_dir   <- "H:/Quina_valleys"
site_xlsx  <- file.path(proj_dir, "data", "Site_information.xlsx")
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- decision-point switches (spec section 2) ----------------------------
## D1 rivers: set a local line shapefile here to override the osmdata download.
rivers_local_path <- NA_character_   # e.g. "H:/Quina_valleys/data_raw/rivers.shp"

## ---- A.1 read sites from Site_information.xlsx, build sf, project ---------
## Site_information.xlsx is the single source of truth (29 sites). We drop PJDD
## and ZKZ to keep the analysed "clean 27". The xlsx `geomorph` column was
## previously corrupted (mojibake) and has been repaired in-file to the canonical
## T2/T3/T4/hilltop tokens; headers carry trailing spaces so are trimmed on read,
## and `basin` is stored as "<name> basin" so the " basin" suffix is stripped.
sites <- readxl::read_excel(site_xlsx)
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>                 # the clean 27
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = c("T2", "T3", "T4", "hilltop"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

sites_utm <- st_transform(sites, 32647)   # EPSG:32647 (UTM 47N), metres — for C

cat("Sites loaded:\n")
print(table(sites$basin, sites$geomorph))

## ---- bounding box (station extent + 8 km) for downloads ------------------
bb    <- st_bbox(st_buffer(sites_utm, 8000)) |> st_as_sfc() |> st_transform(4326)
bb_sf <- st_as_sf(bb)
saveRDS(st_bbox(bb), file.path(cache_dir, "bbox_4326.rds"))

## ---- A.2 DEM + hillshade (elevatr; cached as GeoTIFF) --------------------
dem_tif  <- file.path(cache_dir, "dem.tif")
hill_tif <- file.path(cache_dir, "hillshade.tif")
if (!file.exists(dem_tif) || !file.exists(hill_tif)) {
  message("Downloading DEM via elevatr ...")
  ok <- tryCatch({
    library(terra)
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
  if (!ok) message("  -> 01_map.R will fall back to a plain basemap (D4 fallback).")
} else {
  message("Using cached DEM/hillshade.")
}

## ---- A.3 rivers (osmdata default; local shapefile fallback = D1) ----------
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
    if (!ok) message("  -> river layer and dist-to-river (C.5) skipped until provided.")
  }
} else {
  message("Using cached rivers.")
}

## ---- A.5 admin boundaries for the location inset (rnaturalearth) ----------
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

message("\n00_setup.R done. Cache: ", cache_dir)
