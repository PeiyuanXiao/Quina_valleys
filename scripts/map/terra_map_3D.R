## terra_map_3D.R — Figure 1B: oblique 3-D view of the Binchuan and Heqing basins.
## Run setup.R first; everything read here comes from data/cache/.
##
## Companion to terra_map_2D.R: same DEM, same site table, same channel network,
## same hypsometric palette, so the 3-D panel and the plan map read as one figure.
## What differs is the shading model — in 3-D the relief is carried by ray-traced
## cast shadows and ambient occlusion (rayshader) rather than by the analytical
## hillshade of the plan map.
##
## Pipeline:
##   DEM (EPSG:4326) -> reproject to UTM 47N with SQUARE cells (rayshader assumes
##   square cells; the geographic grid here is 27 m x 43 m and would shear the
##   terrain) -> height_shade() hypsometric wash -> ray_shade() cast shadows ->
##   ambient_shade() occlusion -> river/lake/site overlays -> plot_3d() ->
##   render_highquality() path tracing (rayrender).
##
## Two passes, controlled by `preview`:
##   preview = TRUE  : 60 m cells, few samples  -> ~1-2 min, for framing the camera
##   preview = FALSE : 30 m cells, many samples -> slow, the publication render
##
## Output: output/maps/terrain_3d[_preview].png

library(sf)
library(dplyr)
library(readxl)
library(terra)
library(rayshader)
sf::sf_use_s2(FALSE)
options(rgl.useNULL = TRUE)   # no interactive window; render_highquality still works

proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- what to render -------------------------------------------------------
preview   <- FALSE         # TRUE = fast framing pass, FALSE = publication render
slab_units <- 6            # slab thickness below the terrain, in scene units
cell_m    <- if (preview) 60 else 30     # DEM resample, metres
n_samples <- if (preview) 80 else 400    # path-tracing samples per pixel
out_w     <- if (preview) 1500 else 3543 # 3543 px = 150 mm @ 600 dpi
out_h     <- if (preview) 1125 else 2657

## site symbols, specified in METRES so they are identical at either DEM
## resolution (rayshader sizes them in cells, which halve when cell_m halves)
site_radius_m <- 260
halo_radius_m <- 420

## ---- camera ---------------------------------------------------------------
## theta = azimuth (0 = viewer due south), phi = elevation above the horizon.
## theta 20 / phi 30 looks NNW up the two basins with the western ranges left.
## Corner-on, high, orthographic — the rayshader-README block-model framing.
cam_theta <- 45             # 45 = a corner of the block faces the viewer
cam_phi   <- 38
cam_zoom  <- 0.80           # smaller = closer; orthographic needs a larger value
cam_fov   <- 0              # 0 = orthographic (parallel block edges)
z_exag    <- 3.0            # vertical exaggeration
sun_az    <- 315            # NW sun, matching the plan map's lighting
sun_alt   <- 45

## Surface colouring:
##   "desert" / "imhof1".."imhof4" / "bw" — rayshader's built-in sphere_shade
##     textures, which colour by ASPECT (slope direction). This is what gives
##     the README render its brown/olive modelled look; elevation is no longer
##     encoded by colour.
##   "fig1" — a create_texture() built from the plan map's palette, i.e. the
##     same aspect-based shading but in the Fig. 1 colour family.
texture_style <- "desert"

## ---- palette (identical to terra_map_2D.R, including the paper blend) -----
hyps_cols  <- c("#7E8E74", "#93A184", "#A9B195", "#BFBCA4",
                "#D1C9B3", "#DCD4C1", "#E6DECB")
paper_col     <- "#E9E4D8"
## the plan map blends the tint toward paper (0.72) because there the tint alone
## has to carry the terrain; in 3-D the geometry does that, so the wash is left
## nearly at full strength — otherwise the whole block reads as one flat green
hyps_strength <- 0.95
blend_to <- function(cols, to, k) {
  a <- grDevices::col2rgb(cols) / 255; b <- as.numeric(grDevices::col2rgb(to)) / 255
  m <- a * k + b * (1 - k)
  grDevices::rgb(m[1, ], m[2, ], m[3, ])
}
hyps_cols  <- blend_to(hyps_cols, paper_col, hyps_strength)
basin_cols <- c(Binchuan = "#A0364B", Heqing = "#2F6489")
water_col  <- "#7FCFE6"     # bright cyan, as in the README block model
river_min_acc <- 60000
lake_min_ha   <- 5

## ---- DEM: reproject to UTM 47N, square cells ------------------------------
dem0 <- terra::rast(file.path(cache_dir, "dem.tif"))
dem  <- terra::project(dem0, "EPSG:32647", res = cell_m, method = "bilinear")
message(sprintf("DEM: %d x %d cells at %d m", nrow(dem), ncol(dem), cell_m))

elmat <- rayshader::raster_to_matrix(dem)
ext_utm <- terra::ext(dem)

## ---- sites + water, in the same CRS ---------------------------------------
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(basin = factor(sub(" basin$", "", trimws(basin)),
                        levels = c("Binchuan", "Heqing"))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(32647)
anchor <- subset(sites, code %in% c("LT", "THC"))

read_if <- function(f, reader) if (file.exists(f)) reader(f) else NULL
rivers <- read_if(file.path(cache_dir, "rivers_dem.gpkg"),
                  function(f) st_read(f, quiet = TRUE))
if (!is.null(rivers) && file.exists(file.path(cache_dir, "_d8_accum.tif"))) {
  acc <- terra::rast(file.path(cache_dir, "_d8_accum.tif"))
  rivers$acc <- terra::extract(acc, terra::vect(rivers), fun = max, na.rm = TRUE)[, 2]
  rivers <- rivers |> filter(!is.na(acc), acc >= river_min_acc) |>
    st_transform(32647) |> st_simplify(dTolerance = 60, preserveTopology = TRUE)
}
lakes <- read_if(file.path(cache_dir, "water.gpkg"),
                 function(f) st_read(f, quiet = TRUE))
if (!is.null(lakes)) {
  lakes <- st_make_valid(lakes) |> st_transform(32647)
  lakes <- lakes[as.numeric(st_area(lakes)) >= lake_min_ha * 1e4, ]
  if (nrow(lakes) == 0) lakes <- NULL
}

## ---- shading --------------------------------------------------------------
## sphere_shade() colours each pixel by the direction its surface faces, then
## cast shadows and ambient occlusion are baked on top — the standard rayshader
## recipe, and the source of the README block model's modelled look.
message("Shading ...")
zsc <- cell_m / z_exag
tex_base <- if (identical(texture_style, "fig1")) {
  rayshader::create_texture(
    lightcolor  = hyps_cols[7], shadowcolor = "#454B3E",
    leftcolor   = hyps_cols[3], rightcolor  = hyps_cols[5],
    centercolor = hyps_cols[4])
} else texture_style

tex <- rayshader::sphere_shade(elmat, texture = tex_base, sunangle = sun_az,
                               zscale = zsc) |>
  rayshader::add_shadow(
    rayshader::ray_shade(elmat, sunaltitude = sun_alt, sunangle = sun_az,
                         zscale = zsc, multicore = TRUE),
    max_darken = 0.35) |>                       # 1 = no bake, 0 = black
  rayshader::add_shadow(
    rayshader::ambient_shade(elmat, zscale = zsc, maxsearch = 60,
                             multicore = TRUE),
    max_darken = 0.05)

## ---- overlays: water, then sites ------------------------------------------
if (!is.null(lakes)) {
  tex <- tex |> rayshader::add_overlay(
    rayshader::generate_polygon_overlay(
      lakes, extent = ext_utm, heightmap = elmat,
      linewidth = 0, palette = water_col), alphalayer = 0.9)
}
if (!is.null(rivers)) {
  tex <- tex |> rayshader::add_overlay(
    rayshader::generate_line_overlay(
      rivers, extent = ext_utm, heightmap = elmat,
      color = water_col, linewidth = if (preview) 2 else 4), alphalayer = 0.95)
}
## A white disc is painted under each site so the sphere reads against the pale
## basin floor; the sphere itself is a real 3-D object added after plot_3d().
tex <- tex |> rayshader::add_overlay(
  rayshader::generate_polygon_overlay(
    st_buffer(sites, halo_radius_m), extent = ext_utm, heightmap = elmat,
    linewidth = 0, palette = "white"), alphalayer = 0.85)

## ---- 3-D scene ------------------------------------------------------------
message("Building 3-D scene ...")
rgl::close3d()
rayshader::plot_3d(
  tex, elmat,
  zscale = cell_m / z_exag,          # smaller zscale = more vertical exaggeration
  fov = cam_fov, theta = cam_theta, phi = cam_phi, zoom = cam_zoom,
  windowsize = c(1200, 900),
  ## no rgl shadow plane: it inflates the scene bounding box and the automatic
  ## camera framing then zooms out to fit it. render_highquality() supplies its
  ## own ground plane below.
  background = "white", shadow = FALSE,
  ## Thin dark slab with a crisp edge, as in the README block model. soliddepth
  ## is in SCENE units (elevation / zscale), so it has to track z_exag: the
  ## terrain base sits at min(elev)/zscale, and anything much below that gives a
  ## slab taller than the mountains.
  solid = TRUE, soliddepth = min(elmat, na.rm = TRUE) / zsc - slab_units,
  ## renders lighter than specified: the white ambient dome lifts it
  solidcolor = "#0B0B0A",
  solidlinecolor = "#101010")

## Sites as real spheres, coloured by basin. render_points(size) is in CELLS and
## render_highquality(point_radius) multiplies it, so size is pinned to 1 here
## and the radius is set from metres in the render call below. The lift keeps
## most of the sphere above the surface at either DEM resolution.
site_xy <- st_coordinates(sites)
for (b in levels(sites$basin)) {
  k <- sites$basin == b
  rayshader::render_points(
    long = site_xy[k, "X"], lat = site_xy[k, "Y"], extent = ext_utm,
    heightmap = elmat, zscale = cell_m / z_exag,
    offset = 0.55 * site_radius_m / z_exag,
    color = basin_cols[[b]], size = 1, clear_previous = FALSE)
}

## ---- path-traced render ---------------------------------------------------
out_png <- file.path(output_dir,
                     if (preview) "terrain_3d_preview.png" else "terrain_3d.png")
message("Path tracing (", n_samples, " samples) -> ", basename(out_png))
rayshader::render_highquality(
  filename = out_png, samples = n_samples, width = out_w, height = out_h,
  light = TRUE, lightdirection = c(sun_az, sun_az + 150),
  ## the ambient white dome is already a strong fill: pushing the key light
  ## harder just blows the surface out, so the modelling is carried by the
  ## baked ray_shade/ambient_shade above and these stay moderate
  lightaltitude = c(sun_alt, 80), lightintensity = c(700, 130),
  ## effective sphere radius = render_points(size) * point_radius, in cell units
  point_radius = site_radius_m / cell_m, clamp_value = 10,
  ## rayrender renders a BLACK sky unless ambient_light = TRUE — without it the
  ## backgroundhigh/low colours are silently ignored and the figure comes out on
  ## black. The ground plane catches the contact shadow.
  ambient_light = TRUE, backgroundhigh = "#FFFFFF", backgroundlow = "#E9E5DD",
  ground_material = rayrender::diffuse(color = "#F4F1EB"), ground_size = 1e5)

rgl::close3d()
message("terra_map_3D.R done -> ", out_png)
