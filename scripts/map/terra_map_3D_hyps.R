## terra_map_3D_hyps.R — the Figure 1 block model, re-coloured to sit in the same
## colour family as the 2-D relief sheets.
##
## This is scripts/map/terra_map_3D.R with the SURFACE COLOUR changed and nothing
## else. Camera, DEM, frame, vertical exaggeration, slab, lighting, sample counts
## and site spheres are all exactly as that script leaves them, so the block is
## the same block from the same angle; only what it is painted with differs.
## terra_map_3D.R is untouched and still writes output/maps/terrain_3d.png.
##
## WHAT WAS WRONG
## Measured over the terrain pixels of the two renders:
##
##                    hue median   hue IQR   sat median   light median
##   2-D relief sheet     77.5 deg  60-91 deg     13.7 %        63.1 %
##   3-D block (desert)   29.4 deg  28-31 deg     29.4 %        43.1 %
##
## A 48-degree hue gap and twice the saturation, but the number that explains it
## is the hue IQR: 3 degrees. rayshader's `desert` texture is sphere_shade(),
## which colours by ASPECT, so the block is one hue modulated in value and its
## colour carries no elevation at all. The 2-D sheets colour by ELEVATION. The
## two panels were not two shades of the same map, they were two different
## encodings, and no amount of hue-tweaking the desert texture would have fixed
## that.
##
## WHAT THIS DOES
## plot_3d()'s first argument is any RGB array, so the surface is painted with
## the 2-D sheets' own hypsometric ramp instead. The division of labour is the
## point:
##
##   colour  <- elevation        (what the 2-D sheets do)
##   shading <- geometry         (what ray tracing does, and does better)
##
## so the ANALYTICAL hillshade of the 2-D scripts is deliberately NOT baked in.
## Baking it would put a plan-view hillshade under a path-traced render lit from
## the same sun and the two shadow sets would compound into mud. Here the ramp is
## an albedo map and every bit of modelling comes from ray_shade, ambient_shade
## and the path tracer's own lights.
##
## Two deliberate departures from a plain colour match:
##
##   * THE RAMP IS ANCHORED TO THE REGIONAL SHEET'S ELEVATION LIMITS, not
##     stretched to this block's own range. The block spans 1229-3259 m and the
##     regional sheet 1203-3686 m; stretched to its own range the block would
##     paint 1500 m in the colour the regional sheet uses for 1900 m, and the two
##     figures would disagree about what a colour means. Anchored, a given
##     elevation is a given colour on both, which is the strongest form of
##     "organic combination" available here — the wash becomes a shared key.
##
##   * A WARM OFFSET of `warm_mix` toward the old desert mean (#967B5E). Matching
##     the 2-D sheets exactly would flatten the block into the same grey-green as
##     the map above it and lose the physical-model reading. The offset keeps it
##     recognisably warmer while staying in the same family.
##
## THE SETTLED CONFIGURATION (these are the defaults below; a run with no
## arguments reproduces it at publication resolution):
##
##   palette_mode  green      green throughout, yellow-green only in the bottom
##                            two stops, lightness compressed to 69-46 % so the
##                            wash stays a ground colour and does not model
##   relief_mix    0.15       a light touch of height-above-drainage, enough to
##                            keep the yellow-green in the basin floors
##   key 700 / shadow 0.35 / aspect 0.15
##                            the relief is carried by the light, as it was under
##                            rayshader's own `desert` texture
##   albedo_gain   0.62       the renderer lifts a surface about 25 points, so
##                            the albedo is given darker than it should print
##   slab_m 0, slab #171614   the base as thin as the geometry allows, and dark
##
## Measured over the block's terrain: hue 88.4 deg, saturation 14.8 %, lightness
## 60.0 %, contrast 33.5 (the original desert render: 28.4 / 24.8 / 37.3 / 32.9).
##
## Variants explored on the way are in output/maps/terrain_3d_hyps_*.png; the
## argument list at the top of the file re-runs any of them as a fast preview.
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

## ---- what to render (terra_map_3D.R, unchanged) ---------------------------
## Rscript scripts/map/terra_map_3D_hyps.R [variant] [warm_mix] [albedo_gain]
## A variant tag names the output and forces the fast preview; no arguments
## renders the settled publication pass.
args    <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args) >= 1) args[1] else ""
preview <- nzchar(variant)
## Base slab depth, in METRES below the lowest DEM cell.
##
## NOT in scene units, which is what terra_map_3D.R believes it is passing. That
## script computes min(elmat)/zscale - slab_units and hands it to plot_3d, whose
## numeric branch then divides by zscale AGAIN, so the base ends up roughly
## 1200 m of exaggerated relief below where the comment intends — which is the
## whole reason the slab is as tall as the mountains standing on it. Passing
## metres puts it where it is asked to go. At 0 the base sits exactly on the
## lowest cell in the frame, which is as thin as it can be made without the
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

## ---- camera (terra_map_3D.R, unchanged) -----------------------------------
cam_theta <- 45
cam_phi   <- 38
cam_zoom  <- 0.80
cam_fov   <- 0
z_exag    <- 3.0
sun_az    <- 315
sun_alt   <- 45

## ==========================================================================
## THE COLOUR MODEL — the only thing that differs from terra_map_3D.R
## ==========================================================================
## The 8-stop widened ramp of the regional 2-D sheets (terra_map_2D_SE_tibet.R
## and terra_map_2D_regional_locator.R), not the 7-stop local one: the block has
## to sit beside the regional sheet, and it is the regional sheet's meaning of
## each colour that is being borrowed.
## TWO RAMPS.
##
## "sheet" is the regional 2-D sheet's own: dark green low, cream high. It is a
## conventional hypsometric tint, and its job on those maps is to separate 3.5 km
## of relief across a 200 km frame.
##
## "landscape" reverses the sense of it — pale grey-yellow low, green high — and
## on THIS block that is the truer picture. These are dry-hot valleys: the Jinsha
## tributary floors at 1300-1500 m are sparsely vegetated tan and grey-yellow,
## and it is the flanking ranges above about 2200 m that carry the forest. The
## sheet ramp paints the basin floors the darkest green on the block, which is
## the opposite of what is on the ground.
##
## NOTE WHAT THIS COSTS. The ramp was anchored to the regional sheet's elevation
## limits so that one elevation meant one colour across both figures. Switching
## the block to "landscape" while the 2-D sheets stay on "sheet" breaks that: the
## two figures then run their elevation colour in opposite directions. Either the
## 2-D sheets move to the same ramp, or the shared-key argument is given up and
## the block is simply a landscape view rather than a second hypsometric map.
palette_mode <- "green"

## DRAPE THE PRINTED MAP.
##
## Everything below this line — the ramp stops, the HAND blend, the warm offset,
## the gain, the contrast, the aspect term — was an attempt to arrive at the 2-D
## sheet's colours by computing them again on this side. It never quite lands,
## because the two pipelines differ in one place that matters: the 2-D map bakes
## its own multi-light hillshade into the HSL lightness channel, and here that
## was deliberately left out so the path tracer could do the modelling instead.
## The result is the same palette but not the same picture.
##
## With drape_2d = TRUE the block is instead painted with the RGB raster
## terra_map_2D.R actually prints. That script is sourced for its `shaded` object
## and nothing else; the raster is reprojected onto this block's grid and used as
## the albedo verbatim. There is then no second implementation to drift, and the
## block is literally the map wrapped over its own terrain.
##
## Because that raster ALREADY contains a hillshade, the path tracer must not
## light it hard or the two shadings compound: the key light drops and the baked
## ray_shade/ambient_shade are backed most of the way off. See drape_* below.
## The drape belongs to "landscape" mode, whose whole point was to reproduce the
## printed map exactly. "green" mode computes its own wash instead, precisely so
## that the albedo can stay flat and the path tracer can do the modelling.
drape_2d <- identical(palette_mode, "landscape")

hyps_cols_sheet <- c("#4E6B54", "#6B8263", "#889777", "#A3A98B",
                     "#BAB79F", "#CEC7B0", "#DED6C2", "#EFE8D9")
## grey-yellow valley floor -> khaki -> olive -> montane green
hyps_cols_landscape <- c("#CFC49E", "#C6BC93", "#B9B489", "#A6AC80",
                         "#8DA075", "#74936A", "#5C8460", "#46704F")
## "green": green throughout, with the yellow-green confined to the bottom two
## stops, and a deliberately SHORT value range — lightness runs 69 % to 46 %
## against the landscape ramp's 78 % to 44 %. The wash is not meant to model the
## terrain here; it is a ground colour, and the relief is left to the light, the
## way rayshader's own `desert` texture leaves it.
hyps_cols_green <- c("#C8CB96", "#B6BD86", "#9FAF77", "#8CA46D",
                     "#7C9A66", "#6E9160", "#62885B", "#587F56")
pick_ramp <- function(mode) switch(mode,
  landscape = hyps_cols_landscape,
  green     = hyps_cols_green,
  hyps_cols_sheet)
hyps_cols <- pick_ramp(palette_mode)
paper_col  <- "#E9E4D8"
hyps_strength <- 0.95      # as on the regional sheets
## Gamma bends where the ramp spends its colour. >1 pushes change toward the top
## of the elevation range, which is what the regional sheets want (their subject
## is 3.5 km of relief and the summits have to separate). The landscape ramp
## wants the opposite: the line it has to draw is the one between basin floor and
## mountain, which here sits around 1800-2000 m, and at gamma 1.30 anchored on
## 1203-3686 the green does not arrive until about 2700 m — so all the flanks
## came out khaki and only the top ridges read as mountain. At 0.65 a 2200 m
## slope lands past the middle of the ramp and is green, while a 1400 m basin
## floor is still at a fifth of it and stays grey-yellow.
hyps_gamma <- if (palette_mode %in% c("landscape", "green")) 0.65 else 1.30

## HEIGHT ABOVE NEAREST DRAINAGE, mixed into the ramp.
##
## Colouring by absolute elevation alone produces the complaint that ground which
## is obviously mountainside comes out yellow: a flank at 1800 m sits low on a
## ramp anchored at 1203 m, so it is painted the same khaki as a basin floor
## 400 m below it. Elevation above sea level is simply not what the eye reads as
## "mountain" — height above the local valley bottom is.
##
## data/cache/_hand.tif is already that layer, on exactly the dem.tif grid: 0 m
## along the channels, a median of 104 m and a 95th percentile of 553 m over this
## frame. Mixing it into the ramp position lets a slope be green because it
## STANDS above its valley, not because of where sea level happens to be, while
## the basin floors — which are by definition near their own channels — stay
## grey-yellow whatever their elevation.
##
## relief_mix 0 = pure elevation (the previous behaviour), 1 = pure HAND.
relief_mix  <- 0.15
hand_ref_m  <- 420        # HAND at which ground counts as fully "mountain"
hand_gamma  <- 0.80

## the regional sheet's own ramp limits, so equal elevations get equal colours.
## Set ramp_mode to "own" to stretch to this block's range instead.
ramp_mode <- "shared"
ramp_shared <- c(lo = 1203, hi = 3686)

## how far back toward the old desert block to pull the result. 0 = the 2-D
## sheet's colours exactly, 1 = the old warm brown. The mean of the desert
## render, measured over its terrain pixels, is the colour being mixed in.
## With palette_mode "landscape" the low ground is already grey-yellow, so most
## of the warmth the offset used to supply is in the ramp itself; a large mix
## here just muddies the green tops.
warm_mix  <- 0.00
warm_col  <- "#967B5E"

## ALBEDO IS NOT DISPLAY COLOUR. The wash above is what the 2-D sheets PRINT,
## but here it is fed to a path tracer as a reflectance and then lit by a key
## light, a white ambient dome and a bounce off the ground plane. Measured on the
## first pass, an albedo of mean lightness 48 % rendered at 73 % — a 25-point
## lift — against a 2-D sheet that sits at 61 %. This gain scales the wash down
## so that what comes OUT of the renderer matches the sheet, rather than what
## goes in. Calibrated by rendering and measuring; see the sweep in the header.
albedo_gain <- 0.62

## Contrast on the wash, applied about mid grey per channel. Deepens the low
## ground and opens the summits; because this palette is desaturated, pushing
## the channels apart also lifts saturation a little, which is wanted here.
albedo_contrast <- 1.00

## ASPECT TERM. Dropping sphere_shade() is what cost this block its crispness:
## an aspect texture gives every slope facing a different way its own tone, and
## without it slopes of similar orientation merge and the relief has to be
## carried by cast shadow alone. It is added back as a GREYSCALE MULTIPLIER
## rather than as a colour blend, so it modulates form without touching hue —
## which is the whole point of having gone hypsometric in the first place.
## 0 = pure elevation colour, 0.5 = heavy modelling.
aspect_mix <- 0.15

## Cast shadow and occlusion, baked lighter than in terra_map_3D.R (0.35/0.05).
## With no aspect term in the albedo any more, the path tracer is doing more of
## the modelling, and the old bake on top of it drove the block 20 lightness
## points below the 2-D sheet.
shadow_darken  <- 0.35     # 1 = no bake, 0 = black
ambient_darken <- 0.10

## Key light. terra_map_3D.R uses 700, and at that strength the path tracer
## overrides the wash exactly where the wash matters most: a basin floor is FLAT,
## so it faces the light squarely and burns pale, while the steep mid-slopes above
## it stay dark. That inverts the hypsometric reading — low ground is supposed to
## be the DARK end of this ramp. Dropping the key and letting the white ambient
## dome carry more of the fill lets the elevation colour through; the cost is a
## little modelling contrast, which the baked ray_shade above partly returns.
key_intensity <- 700

## Lighting used when drape_2d is TRUE. The drape carries the relief already, so
## the renderer's job shrinks to giving the block its solidity: a low key, most
## of the fill from the white ambient dome, and only a trace of baked shadow.
drape_key      <- 260
drape_shadow   <- 0.88     # 1 = no bake
drape_ambient  <- 0.45
drape_gain     <- 1.00

## Chroma multiplier on the drape, applied about each pixel's own luminance.
## Needed because gain alone cannot hit both targets: the renderer adds a roughly
## CONSTANT white fill from the ambient dome, so scaling the albedo down to fix
## lightness makes that fill a larger share of the result and washes the colour
## out. Measured — gain 0.58 landed lightness at 67.8 % against a target of 68.8,
## but saturation at 12.9 % against 20.8. Boosting chroma before the scaling
## compensates for the dilution that follows it.
drape_sat      <- 1.00

## BASE SLAB. terra_map_3D.R uses #0B0B0A with a #101010 edge — effectively
## black, and against a re-coloured block that reads as a hole punched in the
## page rather than as the side of a model. These are the project's own dark
## neutrals (#5E5849 is the ground line of the profile figure), so the base now
## belongs to the same palette as everything else and stops competing with the
## site markers for the darkest thing on the sheet.
## The renderer lifts the base the same way it lifts the terrain: measured on
## three passes, an input lightness of 4 / 23 / 33 % came back as 22 / 51 / 59 %,
## i.e. roughly output = 18 + 1.25 x input. So a slab that READS dark grey has to
## be given as near-black. #292826 lands at about 37 %: clearly dark, without
## going back to the hole-in-the-page black of terra_map_3D.R.
slab_col  <- "#171614"
slab_line <- "#0D0C0B"

## water: the 2-D sheets' slate, not the block model's bright cyan (#7FCFE6),
## which was the single loudest thing telling the reader these were two figures
## Water. #86A6BB is the 2-D sheets' slate, and on those maps it reads because it
## is a thin line on a pale ground. On the block it sits on mid-green of almost
## the same lightness, so the only separation is hue and the channels disappear.
## Deepened one step — still the same slate family, but far enough down in value
## to hold against the green.
water_col  <- "#5F87A6"
lake_col   <- "#6E93AF"

## Channels are drawn in two width classes rather than one. A single width has to
## be either too heavy for the tributaries or too light for the trunks; splitting
## at the 60th percentile of upstream area lets the Sangyuan, Liandong and
## Caifeng read as rivers while the feeder network stays fine.
##
## The widths are in TEXTURE pixels, and the texture is the heightmap, so they
## have to double when cell_m halves or the publication render would draw them at
## half the on-page weight of the preview.
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
## palette_mode may have just arrived on the command line, so everything that
## was derived from it above has to be derived again — including drape_2d,
## which otherwise stays TRUE from the default and quietly overrides the wash
## this mode exists to compute.
hyps_cols  <- pick_ramp(palette_mode)
hyps_gamma <- if (palette_mode %in% c("landscape", "green")) 0.65 else 1.30
drape_2d   <- identical(palette_mode, "landscape")
message(sprintf(paste("variant '%s': warm %.2f  gain %.2f  key %.0f",
                      "aspect %.2f  contrast %.2f  shadow %.2f"),
                variant, warm_mix, albedo_gain, key_intensity,
                aspect_mix, albedo_contrast, shadow_darken))
message(sprintf("           palette '%s'  slab %.0f m  relief_mix %.2f",
                palette_mode, slab_m, relief_mix))

## ---- DEM (terra_map_3D.R, unchanged: the site extent + 8 km) --------------
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

## ==========================================================================
## THE HYPSOMETRIC ALBEDO
## ==========================================================================
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
  sys.source(file.path(proj_dir, "scripts", "map", "terra_map_2D.R"), envir = env2)
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
  ## then pull back toward the old block's warmth
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
  ## sphere_shade() returns the TRANSPOSE of the heightmap layout — for a
  ## 265 x 349 elmat it hands back 349 x 265 x 3 — while the wash above was built
  ## in elmat's own layout. t() puts them in the same frame; without it the two
  ## are non-conformable and, on a square DEM where they would not be, the
  ## modulation would land rotated.
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

## ---- overlays: water, then sites (terra_map_3D.R, unchanged) --------------
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

## ---- 3-D scene (terra_map_3D.R, unchanged) --------------------------------
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

## ---- path-traced render (terra_map_3D.R, unchanged) -----------------------
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
