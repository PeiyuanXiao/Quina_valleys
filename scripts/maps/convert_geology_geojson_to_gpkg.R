## Convert extracted geology GeoJSON layers to GeoPackage.
## Run in an R environment with sf installed.

suppressPackageStartupMessages(library(sf))

proj_dir <- here::here()
in_dir <- file.path(proj_dir, "data", "derived", "geology_extraction")
out_gpkg <- file.path(in_dir, "geology_extracted_units.gpkg")

layers <- c(
  units_polygons = file.path(in_dir, "geology_units_polygons_epsg4326.geojson"),
  broad_groups_polygons = file.path(in_dir, "geology_broad_groups_polygons_epsg4326.geojson"),
  units_grid = file.path(in_dir, "geology_units_grid_epsg4326.geojson"),
  broad_groups_grid = file.path(in_dir, "geology_broad_groups_grid_epsg4326.geojson")
)

if (file.exists(out_gpkg)) unlink(out_gpkg)

for (layer_name in names(layers)) {
  x <- st_read(layers[[layer_name]], quiet = TRUE)
  st_crs(x) <- 4326
  x <- st_make_valid(x)
  st_write(x, out_gpkg, layer = layer_name, quiet = TRUE)
}

message("Wrote: ", out_gpkg)
