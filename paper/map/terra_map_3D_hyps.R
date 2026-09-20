## terra_map_3D_hyps.R — the Figure 1 block model (panel B).
##
## The surface is painted with the same hypsometric ramp as the 2-D relief
## sheets, so that a given elevation is a given colour on both panels: the ramp
## is anchored to the REGIONAL sheet's elevation limits rather than stretched
## to this block's own range, and carries a warm offset (`warm_mix`) so that
## the block stays recognisably warmer than the plan map above it.
##
## The ramp is an albedo map only.  The analytical hillshade of the 2-D scripts
## is deliberately not baked in: under a path-traced render lit from the same
## sun the two shadow sets would compound.  All modelling comes from ray_shade,
## ambient_shade and the path tracer's own lights.
##
##   Rscript paper/map/terra_map_3D_hyps.R [variant] [warm_mix] [albedo_gain]
##
## A variant tag names the output and forces a fast preview; no arguments
## renders the settled publication pass, whose defaults are set below.
##
## Output: output/maps/terrain_3d_hyps[_<variant>].png

library(sf)
library(dplyr)
library(readxl)
library(terra)
library(rayshader)
sf::sf_use_s2(FALSE)
options(rgl.useNULL = TRUE)

proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- what to render -------------------------------------------------------
args    <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args) >= 1) args[1] else ""
preview <- nzchar(variant)
## Base slab depth, in METRES below the lowest DEM cell, NOT in scene units:
## plot_3d's numeric branch divides by zscale itself, so a value in scene units
## ends up roughly 1200 m of exaggerated relief too low.  At 0 the base sits on
## the lowest cell in the frame, which is as thin as it can be made without the
## terrain breaking through the bottom of the block; what is left of the dark
## band after that is not decoration but the real side walls, whose height is
## simply the edge of the DEM standing above its own lowest point.
slab_m <- 0
cell_m    <- if (preview) 60 else 30
n_samples <- if (preview) 80 else 400
out_w     <- if (preview) 1500 else 3543
out_h     <- if (preview) 1125 else 2657

site_radius_m <- 260
halo_radius_m <- 420

## ---- camera ---------------------------------------------------------------
cam_theta <- 45
cam_phi   <- 38
cam_zoom  <- 0.80
cam_fov   <- 0
z_exag    <- 3.0
sun_az    <- 315
sun_alt   <- 45

## ---- the colour model ------------------------------------------------------
## The 8-stop widened ramp of the regional 2-D sheet, not the 7-stop local one:
## the block sits beside the regional sheet, and it is that sheet's meaning of
## each colour that is being borrowed.
## Three ramps.  "sheet" is the regional 2-D sheet's own, dark green low and
## cream high.  "landscape" reverses it — pale grey-yellow low, green high —
## which is the truer picture of these dry-hot valleys, whose floors are
## sparsely vegetated and whose forest is on the flanks above about 2200 m; but
## it breaks the shared key with the 2-D sheets, which run the other way.
## "green" is the settled compromise, defined below.
palette_mode <- "green"

## With drape_2d = TRUE the block is painted with the RGB raster terra_map_2D.R
## prints, sourced for its `shaded` object and reprojected onto this grid, so
## there is no second implementation of the wash to drift.  That raster already
## contains a hillshade, so the path tracer must not light it hard or the two
## shadings compound (see drape_* below).  It belongs to "landscape" mode;
## "green" computes its own flat wash and leaves the modelling to the tracer.
drape_2d <- identical(palette_mode, "landscape")

hyps_cols_sheet <- c("#4E6B54", "#6B8263", "#889777", "#A3A98B",
                     "#BAB79F", "#CEC7B0", "#DED6C2", "#EFE8D9")
## grey-yellow valley floor -> khaki -> olive -> montane green
hyps_cols_landscape <- c("#CFC49E", "#C6BC93", "#B9B489", "#A6AC80",
                         "#8DA075", "#74936A", "#5C8460", "#46704F")
## "green": yellow-green confined to the bottom two stops and a deliberately
## short value range, 69 % to 46 %.  The wash is a ground colour, not a model
## of the terrain; the relief is left to the light.
hyps_cols_green <- c("#C8CB96", "#B6BD86", "#9FAF77", "#8CA46D",
                     "#7C9A66", "#6E9160", "#62885B", "#587F56")
pick_ramp <- function(mode) switch(mode,
  landscape = hyps_cols_landscape,
  green     = hyps_cols_green,
  hyps_cols_sheet)
hyps_cols <- pick_ramp(palette_mode)
paper_col  <- "#E9E4D8"
hyps_strength <- 0.95      # as on the regional sheets
## Gamma bends where the ramp spends its colour.  >1 pushes change toward the
## top of the range, which is what the regional sheets want.  Here the line to
## draw is between basin floor and mountain, around 1800-2000 m, so 0.65: a
## 2200 m slope lands past the middle of the ramp and is green, a 1400 m basin
## floor is still at a fifth of it and stays grey-yellow.
hyps_gamma <- if (palette_mode %in% c("landscape", "green")) 0.65 else 1.30

## Height above nearest drainage, mixed into the ramp position, because what
## the eye reads as "mountain" is height above the local valley bottom rather
## than above sea level.  data/cache/_hand.tif is that layer on the dem.tif
## grid.  relief_mix 0 = pure elevation, 1 = pure HAND.
relief_mix  <- 0.15
hand_ref_m  <- 420        # HAND at which ground counts as fully "mountain"
hand_gamma  <- 0.80

## the regional sheet's own ramp limits, so equal elevations get equal colours.
## Set ramp_mode to "own" to stretch to this block's range instead.
ramp_mode <- "shared"
ramp_shared <- c(lo = 1203, hi = 3686)

## Warm offset: 0 = the 2-D sheet's colours exactly, 1 = a warm brown block.
## Under "landscape" and "green" the low ground is already warm, so a large mix
## here only muddies the green tops.
warm_mix  <- 0.00
warm_col  <- "#967B5E"

## Albedo is not display colour: the wash is fed to the path tracer as a
## reflectance and then lit, which lifts it about 25 lightness points.  This
## gain scales it down so that what comes OUT matches the 2-D sheet.
albedo_gain <- 0.62

## Contrast on the wash, applied about mid grey per channel. Deepens the low
## ground and opens the summits; because this palette is desaturated, pushing
## the channels apart also lifts saturation a little, which is wanted here.
albedo_contrast <- 1.00

## Aspect term, added as a greyscale multiplier rather than a colour blend, so
## that it modulates form without touching hue: without it slopes of similar
## orientation merge.  0 = pure elevation colour, 0.5 = heavy modelling.
aspect_mix <- 0.15

## Cast shadow and occlusion, baked light: with no aspect term in the albedo
## the path tracer does more of the modelling, and a heavier bake on top of it
## drives the block some 20 lightness points below the 2-D sheet.
shadow_darken  <- 0.35     # 1 = no bake, 0 = black
ambient_darken <- 0.10

## Key light.  Raise it much and the tracer overrides the wash where it
## matters: a basin floor is flat, so it faces the light squarely and burns
## pale while the steep slopes above it stay dark, inverting the hypsometric
## reading.  The white ambient dome carries more of the fill instead.
key_intensity <- 700

## Lighting used when drape_2d is TRUE. The drape carries the relief already, so
## the renderer's job shrinks to giving the block its solidity: a low key, most
## of the fill from the white ambient dome, and only a trace of baked shadow.
drape_key      <- 260
drape_shadow   <- 0.88     # 1 = no bake
drape_ambient  <- 0.45
drape_gain     <- 1.00

## Chroma multiplier on the drape.  Gain alone cannot hit both targets: the
## ambient dome adds a roughly constant white fill, so scaling the albedo down
## to fix lightness washes the colour out.  Boosting chroma first compensates.
drape_sat      <- 1.00

## BASE SLAB, in the project's own dark neutrals rather than near-black, which
## against a re-coloured block reads as a hole punched in the page.  The
## renderer lifts the base as it lifts the terrain — roughly
## output = 18 + 1.25 x input — so a slab that READS dark grey is given dark.
slab_col  <- "#171614"
slab_line <- "#0D0C0B"

## Water: the 2-D sheets' slate family, deepened one step.  Their own #86A6BB
## reads as a thin line on a pale ground, but on mid-green of nearly the same
## lightness the only separation is hue and the channels disappear.
water_col  <- "#5F87A6"
lake_col   <- "#6E93AF"

## Two width classes, split at the 60th percentile of upstream area, so that
## the trunks read as rivers while the feeder network stays fine.  The widths
## are in TEXTURE pixels and the texture is the heightmap, so they double when
## cell_m halves or the publication render draws them at half the preview's
## on-page weight.
river_lw_minor <- if (preview) 2.4 else 4.8
river_lw_trunk <- if (preview) 5.0 else 10.0
river_split_q  <- 0.60
basin_cols <- c(Binchuan = "#A0364B", Huangping = "#2F6489")
river_min_acc <- 60000
lake_min_ha   <- 5

if (length(args) >= 2 && nzchar(args[2])) warm_mix    <- as.numeric(args[2])
if (length(args) >= 3 && nzchar(args[3])) albedo_gain   <- as.numeric(args[3])
if (length(args) >= 4 && nzchar(args[4])) key_intensity   <- as.numeric(args[4])
if (length(args) >= 5 && nzchar(args[5])) aspect_mix      <- as.numeric(args[5])
if (length(args) >= 6 && nzchar(args[6])) albedo_contrast <- as.numeric(args[6])
if (length(args) >= 7 && nzchar(args[7])) shadow_darken   <- as.numeric(args[7])
if (length(args) >= 8 && nzchar(args[8])) slab_m          <- as.numeric(args[8])
if (length(args) >= 9 && nzchar(args[9])) palette_mode    <- args[9]
if (length(args) >= 10 && nzchar(args[10])) relief_mix  <- as.numeric(args[10])
if (length(args) >= 11 && nzchar(args[11])) drape_key   <- as.numeric(args[11])
if (length(args) >= 12 && nzchar(args[12])) drape_shadow <- as.numeric(args[12])
if (length(args) >= 13 && nzchar(args[13])) drape_gain  <- as.numeric(args[13])
if (length(args) >= 14 && nzchar(args[14])) drape_sat   <- as.numeric(args[14])
## palette_mode may have just arrived on the command line, so everything
## derived from it above has to be derived again -- drape_2d included, which
## otherwise quietly overrides the wash the new mode exists to compute.
hyps_cols  <- pick_ramp(palette_mode)
hyps_gamma <- if (palette_mode %in% c("landscape", "green")) 0.65 else 1.30
drape_2d   <- identical(palette_mode, "landscape")
message(sprintf(paste("variant '%s': warm %.2f  gain %.2f  key %.0f",
                      "aspect %.2f  contrast %.2f  shadow %.2f"),
                variant, warm_mix, albedo_gain, key_intensity,
                aspect_mix, albedo_contrast, shadow_darken))
message(sprintf("           palette '%s'  slab %.0f m  relief_mix %.2f",
                palette_mode, slab_m, relief_mix))

## ---- DEM: the site extent + 8 km ------------------------------------------
dem0 <- terra::rast(file.path(cache_dir, "dem.tif"))
dem  <- terra::project(dem0, "EPSG:32647", res = cell_m, method = "bilinear")
message(sprintf("DEM: %d x %d cells at %d m", nrow(dem), ncol(dem), cell_m))

elmat <- rayshader::raster_to_matrix(dem)
ext_utm <- terra::ext(dem)

## HAND on the DEM's own grid, so the two matrices are element-for-element
handmat <- NULL
hand_f <- file.path(cache_dir, "_hand.tif")
if (relief_mix > 0 && file.exists(hand_f)) {
  handmat <- rayshader::raster_to_matrix(
    terra::project(terra::rast(hand_f), dem, method = "bilinear"))
  handmat[is.na(handmat)] <- 0
  stopifnot(identical(dim(handmat), dim(elmat)))
  message(sprintf("HAND: median %.0f m, 95th %.0f m",
                  median(handmat), quantile(handmat, 0.95)))
} else if (relief_mix > 0) {
  message("_hand.tif not found - falling back to pure elevation colour")
  relief_mix <- 0
}

## ---- sites + water --------------------------------------------------------
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(basin = factor(sub(" basin$", "", trimws(basin)),
                        levels = c("Binchuan", "Huangping"))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(32647)

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

## ---- the hypsometric albedo ------------------------------------------------
## Built straight off `elmat`, so it inherits the matrix's orientation and no
## transposition can creep in between the colour and the surface it paints.
zsc <- cell_m / z_exag

## ---- the drape -------------------------------------------------------------
drape_tex <- NULL
if (isTRUE(drape_2d)) {
  message("Sourcing terra_map_2D.R for its shaded raster ...")
  env2 <- new.env()
  assign("PALETTE_MODE", palette_mode, envir = env2)
  assign("SKIP_EXPORT", TRUE, envir = env2)
  sys.source(file.path(proj_dir, "paper", "map", "terra_map_2D.R"), envir = env2)
  sh <- terra::project(env2$shaded, dem, method = "bilinear")
  m3 <- terra::values(sh) / 255
  m3[is.na(m3)] <- 1
  drape_tex <- array(c(matrix(m3[, 1], nrow(elmat)),
                       matrix(m3[, 2], nrow(elmat)),
                       matrix(m3[, 3], nrow(elmat))),
                     dim = c(nrow(elmat), ncol(elmat), 3))
  if (drape_sat != 1) {
    lum <- (drape_tex[, , 1] + drape_tex[, , 2] + drape_tex[, , 3]) / 3
    for (i in 1:3)
      drape_tex[, , i] <- lum + (drape_tex[, , i] - lum) * drape_sat
  }
  drape_tex[] <- pmin(1, pmax(0, drape_tex * drape_gain))
  cat(sprintf("  drape mean #%02X%02X%02X\n",
              round(mean(drape_tex[, , 1]) * 255),
              round(mean(drape_tex[, , 2]) * 255),
              round(mean(drape_tex[, , 3]) * 255)))
  key_intensity  <- drape_key
  shadow_darken  <- drape_shadow
  ambient_darken <- drape_ambient
  aspect_mix     <- 0
}

message("Painting the hypsometric wash ...")
rng <- if (identical(ramp_mode, "shared")) ramp_shared else
  c(lo = min(elmat, na.rm = TRUE), hi = max(elmat, na.rm = TRUE))
message(sprintf("  ramp %s: %.0f to %.0f m (block spans %.0f to %.0f m), gamma %.2f",
                ramp_mode, rng[["lo"]], rng[["hi"]],
                min(elmat, na.rm = TRUE), max(elmat, na.rm = TRUE), hyps_gamma))

hyps_tex <- local({
  u <- pmin(1, pmax(0, (as.numeric(elmat) - rng[["lo"]]) /
                       (rng[["hi"]] - rng[["lo"]]))) ^ hyps_gamma
  u[is.na(u)] <- 0
  if (relief_mix > 0 && !is.null(handmat)) {
    uh <- pmin(1, pmax(0, as.numeric(handmat) / hand_ref_m)) ^ hand_gamma
    u  <- (1 - relief_mix) * u + relief_mix * uh
  }
  m <- grDevices::colorRamp(hyps_cols, space = "Lab")(u) / 255
  ## compress toward the paper neutral, exactly as the 2-D scripts do
  pap <- as.numeric(grDevices::col2rgb(paper_col)) / 255
  m <- m * hyps_strength + rep(pap, each = length(u)) * (1 - hyps_strength)
  ## then pull back toward the warm offset
  wc <- as.numeric(grDevices::col2rgb(warm_col)) / 255
  m <- m * (1 - warm_mix) + rep(wc, each = length(u)) * warm_mix
  ## contrast about mid grey, then level. m[] keeps the dim attribute;
  ## pmin/pmax on a matrix would drop it
  m[] <- pmin(1, pmax(0, (0.5 + (m - 0.5) * albedo_contrast) * albedo_gain))
  array(c(matrix(m[, 1], nrow(elmat)),
          matrix(m[, 2], nrow(elmat)),
          matrix(m[, 3], nrow(elmat))),
        dim = c(nrow(elmat), ncol(elmat), 3))
})
## the greyscale aspect multiplier, folded in after the ramp so that hue is
## untouched: sphere_shade("bw") is 0.5 on a surface facing the viewer, brighter
## on sunlit aspects and darker on shaded ones, so (2*lum - 1) is a signed
## modulation and aspect_mix is its amplitude.
if (aspect_mix > 0) {
  sph <- rayshader::sphere_shade(elmat, texture = "bw", sunangle = sun_az,
                                 zscale = zsc)
  ## sphere_shade() returns the TRANSPOSE of the heightmap layout while the
  ## wash was built in elmat's own, so t() puts them in the same frame.  On a
  ## square DEM, where they would still be conformable, omitting it would land
  ## the modulation rotated.
  stopifnot(identical(dim(sph)[1:2], rev(dim(hyps_tex)[1:2])))
  f <- t(1 + aspect_mix * (2 * sph[, , 1] - 1))
  for (i in 1:3) hyps_tex[, , i] <- pmin(1, pmax(0, hyps_tex[, , i] * f))
  message(sprintf("  aspect term folded in at %.2f", aspect_mix))
}
cat(sprintf("  albedo mean #%02X%02X%02X\n",
            round(mean(hyps_tex[, , 1]) * 255), round(mean(hyps_tex[, , 2]) * 255),
            round(mean(hyps_tex[, , 3]) * 255)))

## ---- shading: cast shadow + occlusion, no aspect term ---------------------
message("Shading ...")
if (!is.null(drape_tex)) hyps_tex <- drape_tex
tex <- hyps_tex |>
  rayshader::add_shadow(
    rayshader::ray_shade(elmat, sunaltitude = sun_alt, sunangle = sun_az,
                         zscale = zsc, multicore = TRUE),
    max_darken = shadow_darken) |>
  rayshader::add_shadow(
    rayshader::ambient_shade(elmat, zscale = zsc, maxsearch = 60,
                             multicore = TRUE),
    max_darken = ambient_darken)

## ---- overlays: water, then sites ------------------------------------------
if (!is.null(lakes)) {
  tex <- tex |> rayshader::add_overlay(
    rayshader::generate_polygon_overlay(
      lakes, extent = ext_utm, heightmap = elmat,
      linewidth = 0, palette = lake_col), alphalayer = 0.92)
}
if (!is.null(rivers) && nrow(rivers) > 0) {
  ## minor first, trunks over the top, so confluences read as one channel
  cut <- stats::quantile(rivers$acc, river_split_q, na.rm = TRUE)
  for (cls in list(list(sel = rivers$acc <  cut, lw = river_lw_minor),
                   list(sel = rivers$acc >= cut, lw = river_lw_trunk))) {
    if (!any(cls$sel, na.rm = TRUE)) next
    tex <- tex |> rayshader::add_overlay(
      rayshader::generate_line_overlay(
        rivers[which(cls$sel), ], extent = ext_utm, heightmap = elmat,
        color = water_col, linewidth = cls$lw), alphalayer = 0.98)
  }
  message(sprintf("rivers: %d minor + %d trunk segments drawn",
                  sum(rivers$acc < cut, na.rm = TRUE),
                  sum(rivers$acc >= cut, na.rm = TRUE)))
}
tex <- tex |> rayshader::add_overlay(
  rayshader::generate_polygon_overlay(
    st_buffer(sites, halo_radius_m), extent = ext_utm, heightmap = elmat,
    linewidth = 0, palette = "white"), alphalayer = 0.85)

## ---- 3-D scene ------------------------------------------------------------
message("Building 3-D scene ...")
rgl::close3d()
rayshader::plot_3d(
  tex, elmat,
  zscale = zsc,
  fov = cam_fov, theta = cam_theta, phi = cam_phi, zoom = cam_zoom,
  windowsize = c(1200, 900),
  background = "white", shadow = FALSE,
  solid = TRUE, soliddepth = min(elmat, na.rm = TRUE) - slab_m,   # METRES
  solidcolor = slab_col,
  solidlinecolor = slab_line)

site_xy <- st_coordinates(sites)
for (b in levels(sites$basin)) {
  k <- sites$basin == b
  rayshader::render_points(
    long = site_xy[k, "X"], lat = site_xy[k, "Y"], extent = ext_utm,
    heightmap = elmat, zscale = zsc,
    offset = 0.55 * site_radius_m / z_exag,
    color = basin_cols[[b]], size = 1, clear_previous = FALSE)
}

## ---- path-traced render ---------------------------------------------------
out_png <- file.path(output_dir,
                     if (preview) paste0("terrain_3d_hyps_", variant, ".png") else "terrain_3d_hyps.png")
message("Path tracing (", n_samples, " samples) -> ", basename(out_png))
rayshader::render_highquality(
  filename = out_png, samples = n_samples, width = out_w, height = out_h,
  light = TRUE, lightdirection = c(sun_az, sun_az + 150),
  lightaltitude = c(sun_alt, 80), lightintensity = c(key_intensity, 130),
  point_radius = site_radius_m / cell_m, clamp_value = 10,
  ambient_light = TRUE, backgroundhigh = "#FFFFFF", backgroundlow = "#E9E5DD",
  ground_material = rayrender::diffuse(color = "#F4F1EB"), ground_size = 1e5)

rgl::close3d()
message("terra_map_3D_hyps.R done -> ", out_png)
