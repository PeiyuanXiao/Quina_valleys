## Plot extracted geology polygons over the project DEM.
## This is a styling script: edit colours/alpha/linewidth here without rerunning
## the scan extraction.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(ggplot2)
  library(tidyterra)
  library(ggnewscale)
})

proj_dir <- here::here()
in_dir <- file.path(proj_dir, "data", "derived", "geology_extraction")
out_dir <- file.path(proj_dir, "output", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dem_path <- file.path(proj_dir, "data", "cache", "dem.tif")
geology_path <- file.path(in_dir, "geology_units_polygons_epsg4326.geojson")
legend_path <- file.path(in_dir, "legend_units.csv")

stopifnot(file.exists(dem_path), file.exists(geology_path), file.exists(legend_path))

dem <- rast(dem_path)
geology <- st_read(geology_path, quiet = TRUE)
legend <- read.csv(legend_path, stringsAsFactors = FALSE)

if (st_crs(geology)$epsg != 4326) geology <- st_transform(geology, 4326)
geology <- st_transform(geology, crs(dem))

slope <- terrain(dem, "slope", unit = "radians")
aspect <- terrain(dem, "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = 40, direction = 315)
names(hillshade) <- "hillshade"

pal <- setNames(legend$color, legend$unit_id)

p <- ggplot() +
  geom_spatraster(data = hillshade, aes(fill = hillshade), show.legend = FALSE) +
  scale_fill_gradient(low = "grey20", high = "white") +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = geology,
    aes(fill = unit_id),
    color = NA,
    alpha = 0.62,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal, na.value = "transparent") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Extracted geology over DEM hillshade") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.02))

ggsave(file.path(out_dir, "map_geology_extracted_over_dem.png"), p, width = 8, height = 9.5, dpi = 300)
ggsave(file.path(out_dir, "map_geology_extracted_over_dem.pdf"), p, width = 8, height = 9.5)

message("Wrote output/map_geology_extracted_over_dem.(png|pdf)")
