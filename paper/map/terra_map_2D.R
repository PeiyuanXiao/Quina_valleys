## terra_map_2D.R — Figure 1A: plan terrain site map of the Binchuan and
## Huangping basins. Self-rendered from the cached SRTM DEM and a DEM-derived stream
## network, so it contains NO roads and NO basemap place-names.
## Run setup.R first; everything read here comes from data/cache/.
##
## Cartography (deliberately matched to the rest of the project):
##   * relief shading in the HSL L-channel plus a warm-light / cool-shadow tint:
##     the hillshade modulates the lightness of each pixel's own colour rather
##     than sitting over it as a grey alpha overlay, so the tint colours stay
##     clean and the relief reads as form rather than dirt;
##   * pale, low-saturation hypsometric wash, so the site symbols carry the
##     highest contrast on the page;
##   * stream network thinned by upstream contributing area, line width scaled
##     to it: trunk rivers read, hillslope rills disappear;
##   * symbols -- fill = basin (the manuscript pink/blue, darkened for legibility
##     on terrain), shape = geomorphic position (the same mapping as
##     scripts/figures/traverse_profile_figure.R, so that panels A and C of
##     Figure 1 share one legend), one fixed size for every site;
##   * theme, type sizes and export geometry copied from scripts/figures/*.R
##     (theme_minimal(9), #202124 panel border, 150 mm wide @ 600 dpi).
##
## Rivers/lakes: loaded from whichever of these exists (in this order), both
## written by setup.R:
##   data/cache/rivers.gpkg      (OSM — often empty here)
##   data/cache/rivers_dem.gpkg  (DEM-derived, WhiteboxTools — the usual source)
## A local shapefile is best: set rivers_local_path below.

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
sf::sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- display toggles -----------------------------------------------------
for_manuscript    <- TRUE   # TRUE -> drop title/subtitle/caption (the .qmd carries them)
show_elev_legend  <- FALSE  # the elevation colourbar; off for the Fig. 1 panel
show_site_labels  <- TRUE
show_basin_tint   <- FALSE  # convex hulls read as marquee boxes
show_river_labels <- FALSE
rivers_local_path <- NA_character_   # e.g. here::here("data_raw", "rivers.shp") (best)

## ---- palette mode --------------------------------------------------------
## Declared before the cartographic parameters because hyps_strength below
## depends on it. A caller assigns PALETTE_MODE in the environment it sources
## this script into; unset, everything behaves exactly as it always did.
palette_mode <- if (exists("PALETTE_MODE", inherits = FALSE)) PALETTE_MODE else "sheet"

## ---- optional frame override ---------------------------------------------
## Unset, the map is drawn on data/cache/dem.tif, which is the site extent + 8 km
## and is what every earlier version of this panel used. A caller can widen it by
## assigning MAP_EXT (xmin, xmax, ymin, ymax) before sourcing — used to pull the
## north edge up to the Jinsha. Any frame larger than dem.tif is served from the
## regional cache that terra_map_2D_regional.R built for exactly this ground:
## dem_regional.tif and its breached-and-routed channels reach 27.11 N.
map_ext <- if (exists("MAP_EXT", inherits = FALSE)) MAP_EXT else NULL
use_regional <- !is.null(map_ext)

## ---- optional single-site highlight --------------------------------------
## HIGHLIGHT_CODE names one locality to pick out: its marker ring turns from
## white to yellow and its label from the common ink colour to red. Everything
## else about the symbol — fill by basin, shape by geomorphic position, size —
## is untouched, so the highlight adds emphasis without adding a category.
highlight_code  <- if (exists("HIGHLIGHT_CODE", inherits = FALSE)) HIGHLIGHT_CODE else NA_character_
highlight_label <- "#C62828"

## The target ring around the highlighted site is off: the red label alone is
## enough to pick it out, and at this density the ring necessarily enclosed its
## neighbour 1 km away as well.
highlight_ring_show <- FALSE
highlight_ring      <- "#EFB537"

## ---- bare-map options ----------------------------------------------------
## Unset, both are TRUE and the sheet is drawn exactly as before. A caller can
## strip the reference furniture — graticule, axis text, ticks, legend — for a
## panel that is placed inside a composite figure where those are carried
## elsewhere or not wanted at all. The scale bar and north arrow are NOT part of
## this: they are the only things left that give the panel a scale.
show_graticule <- if (exists("SHOW_GRID", inherits = FALSE)) SHOW_GRID else TRUE
show_legend    <- if (exists("SHOW_LEGEND", inherits = FALSE)) SHOW_LEGEND else TRUE

## Symbol and label size. The defaults are the values this sheet has always used;
## a caller raises them when the panel is placed larger, or when the frame is
## narrow enough that the old sizes read as specks.
site_size  <- if (exists("SITE_SIZE",  inherits = FALSE)) SITE_SIZE  else 2.2
site_stroke <- if (exists("SITE_STROKE", inherits = FALSE)) SITE_STROKE else 0.45
label_size <- if (exists("LABEL_SIZE", inherits = FALSE)) LABEL_SIZE else 2.0

## Panel border. 0.5 is the house hairline, shared with every analysis figure;
## on a bare map, where the border is the only edge the sheet has left, it needs
## to be a frame rather than a hairline.
border_lw  <- if (exists("BORDER_WIDTH", inherits = FALSE)) BORDER_WIDTH else 0.5

## ---- cartographic parameters ---------------------------------------------
z_exag        <- 1.8              # hillshade vertical exaggeration
sun_angle     <- 35               # sun elevation (deg)
sun_dirs      <- c(300, 337, 15)  # multi-light azimuths, averaged
## Relief is applied as a headroom-aware interpolation of the HSL L channel
## (L -> 1 on lit faces, L -> 0 in shadow), NOT as a multiplicative gain: a
## multiplicative gain clips to pure white wherever the base tint is already
## pale, which blows out the high ground and destroys the shading detail there.
relief_hi     <- 0.42             # 0..1, pull toward white on lit faces
relief_lo     <- 0.38             # 0..1, pull toward black in shadow
warm_gain     <- 0.10             # warm tint added on lit slopes
cool_gain     <- 0.20             # cool tint added in shadow
## 0.72 is right for the sheet ramp, which is only a wash under a hillshade that
## carries the map. The landscape ramp has a job of its own — telling valley
## floor from mountainside — and at 0.72 it washes into the paper neutral.
hyps_strength <- if (identical(palette_mode, "landscape")) 0.88 else 0.72
river_min_acc <- 60000            # min upstream cells for a channel to be drawn
lake_min_ha   <- 5                # drop OSM ponds smaller than this
cont_minor    <- 250              # contour intervals (m)
cont_index    <- 500
grat_step     <- 0.05             # graticule / axis-break spacing (degrees)

## ---- palette --------------------------------------------------------------
## pale hypsometric wash: sage valley floors -> sand -> stone highlands.
## Kept deliberately narrow in value: with hyps_strength the relief, not the
## elevation tint, carries the terrain, so the sheet stays even and the site
## symbols keep the strongest contrast on the page.
## the top stop is deliberately NOT near-white: the relief shading needs
## headroom above it, or the summits render as a featureless white blob
## TWO RAMPS. The default is unchanged, so nothing that sourced this script
## before sees any difference; a caller that wants the landscape ramp assigns
## PALETTE_MODE <- "landscape" in the environment before sourcing.
##
## "landscape" reverses the sense of the tint — pale grey-yellow low, green high
## — because that is what is actually on the ground here: these are dry-hot
## valleys whose floors at 1300-1500 m are sparsely vegetated tan, and it is the
## flanking ranges that carry the forest. It is the same ramp the 3-D block uses
## (paper/map/terra_map_3D_hyps.R), on the same elevation anchors, so the two
## panels agree about what a colour means.

hyps_cols_sheet <- c("#7E8E74", "#93A184", "#A9B195", "#BFBCA4",
                     "#D1C9B3", "#DCD4C1", "#E6DECB")
hyps_cols_landscape <- c("#CFC49E", "#C6BC93", "#B9B489", "#A6AC80",
                         "#8DA075", "#74936A", "#5C8460", "#46704F")
hyps_cols  <- if (identical(palette_mode, "landscape"))
  hyps_cols_landscape else hyps_cols_sheet

## Landscape mode also takes the block's ramp anchors, gamma and HAND blend, so
## that a given piece of ground is the same colour on the map and on the block.
lsc_ramp       <- c(lo = 1203, hi = 3686)
lsc_gamma      <- 0.65
lsc_relief_mix <- 0.45     # weight on height above nearest drainage
lsc_hand_ref   <- 420
lsc_hand_gamma <- 0.80
paper_col  <- "#E9E4D8"           # neutral the tint is blended toward
## the wash actually painted on the map, so the colourbar matches the sheet
blend_to <- function(cols, to, k) {
  a <- grDevices::col2rgb(cols) / 255; b <- as.numeric(grDevices::col2rgb(to)) / 255
  m <- a * k + b * (1 - k)
  grDevices::rgb(m[1, ], m[2, ], m[3, ])
}
## basin colours = colorspace::darken() of the manuscript hues #E07C90 / #6BA8CE,
## so Fig. 1 sits in the same colour family as the analysis figures
basin_cols <- c(Binchuan = "#A0364B", Huangping = "#2F6489")
water_col  <- "#86A6BB"
contour_col<- "#6B6357"
label_col  <- "#332F29"
grat_col   <- grDevices::adjustcolor("white", alpha.f = 0.78)
## shape = geomorphic position, identical to the traverse profile (Fig. 1C)
geomorph_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)

## ---- sites (Site_information.xlsx = source of truth) ----------------------
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Huangping")),
    geomorph = factor(geomorph, levels = names(geomorph_shapes))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

## faint tinted hull per basin (groups the sites without drawing a marquee box)
hulls <- sites |>
  group_by(basin) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  st_convex_hull() |>
  st_transform(32647) |> st_buffer(1100) |> st_transform(4326)

## ---- cached DEM + rivers + (optional) lakes ------------------------------
read_if <- function(f, reader) if (file.exists(f)) reader(f) else NULL
dem <- if (use_regional) {
  f <- file.path(cache_dir, "dem_regional.tif")
  if (!file.exists(f))
    stop("MAP_EXT set but data/cache/dem_regional.tif is missing - run ",
         "paper/map/terra_map_2D_regional.R once to build the regional cache.")
  terra::crop(terra::rast(f), terra::ext(map_ext[c("xmin", "xmax", "ymin", "ymax")]))
} else read_if(file.path(cache_dir, "dem.tif"), terra::rast)
if (is.null(dem)) stop("data/cache/dem.tif not found — run setup.R first.")

if (!is.na(rivers_local_path) && file.exists(rivers_local_path)) {
  rivers <- st_read(rivers_local_path, quiet = TRUE) |> st_transform(4326)
} else {
  rivers <- if (use_regional)
    read_if(file.path(cache_dir, "rivers_dem_regional.gpkg"),
            function(f) st_read(f, quiet = TRUE)) else
    read_if(file.path(cache_dir, "rivers.gpkg"),
                    function(f) st_read(f, quiet = TRUE)) %||%
            read_if(file.path(cache_dir, "rivers_dem.gpkg"),
                    function(f) st_read(f, quiet = TRUE))
}
lakes <- read_if(file.path(cache_dir,
                           if (use_regional) "water_regional.gpkg" else "water.gpkg"),
                 function(f) st_read(f, quiet = TRUE))

## ---- thin the drainage: keep channels by upstream contributing area -------
## The WhiteboxTools network from setup.R is a full drainage net; at this scale every
## hillslope rill shows and the map reads as noise. Rank each line by the flow
## accumulation at its outlet, keep the trunks, and scale line width to it.
acc_path <- file.path(cache_dir,
                      if (use_regional) "_d8_accum_regional.tif" else "_d8_accum.tif")
river_km2 <- NA_real_
if (!is.null(rivers) && file.exists(acc_path)) {
  acc <- terra::rast(acc_path)
  rivers$acc <- terra::extract(acc, terra::vect(rivers), fun = max, na.rm = TRUE)[, 2]
  ## cell area in km2 (geographic grid, evaluated at the centre latitude)
  lat0 <- mean(c(terra::ymin(acc), terra::ymax(acc)))
  cell_km2 <- (terra::xres(acc) * 111.32 * cos(lat0 * pi / 180)) *
              (terra::yres(acc) * 111.32)
  river_km2 <- river_min_acc * cell_km2
  rivers <- rivers |>
    filter(!is.na(acc), acc >= river_min_acc) |>
    mutate(w = log10(acc))
  ## the raster-traced lines are stair-stepped; simplify in metres to smooth them
  rivers <- rivers |> st_transform(32647) |>
    st_simplify(dTolerance = 60, preserveTopology = TRUE) |> st_transform(4326)
  message(sprintf("Rivers: %d channels kept (>= %.0f km2 upstream).",
                  nrow(rivers), river_km2))
}
if (!is.null(lakes)) {
  lakes <- st_make_valid(lakes)
  lakes <- lakes[as.numeric(st_area(st_transform(lakes, 32647))) >= lake_min_ha * 1e4, ]
  if (nrow(lakes) == 0) lakes <- NULL
}

## ---- relief shading (HSL L-channel + warm/cool light) ---------------------
rgb2hsl <- function(r, g, b) {
  mx <- pmax(r, g, b); mn <- pmin(r, g, b); l <- (mx + mn) / 2; d <- mx - mn
  s <- ifelse(d == 0, 0, d / (1 - abs(2 * l - 1)))
  h <- ifelse(d == 0, 0,
       ifelse(mx == r, ((g - b) / d) %% 6,
       ifelse(mx == g, ((b - r) / d) + 2, ((r - g) / d) + 4))) / 6
  list(h = h %% 1, s = s, l = l)
}
hsl2rgb <- function(h, s, l) {
  c <- (1 - abs(2 * l - 1)) * s; hp <- h * 6; x <- c * (1 - abs(hp %% 2 - 1))
  r <- g <- b <- numeric(length(h))
  i <- hp < 1;             r[i] <- c[i]; g[i] <- x[i]
  i <- hp >= 1 & hp < 2;   r[i] <- x[i]; g[i] <- c[i]
  i <- hp >= 2 & hp < 3;   g[i] <- c[i]; b[i] <- x[i]
  i <- hp >= 3 & hp < 4;   g[i] <- x[i]; b[i] <- c[i]
  i <- hp >= 4 & hp < 5;   r[i] <- x[i]; b[i] <- c[i]
  i <- hp >= 5;            r[i] <- c[i]; b[i] <- x[i]
  m <- l - c / 2; list(r = r + m, g = g + m, b = b + m)
}

dem_z  <- dem * z_exag
slope  <- terra::terrain(dem_z, "slope",  unit = "radians")
aspect <- terra::terrain(dem_z, "aspect", unit = "radians")
hl     <- lapply(sun_dirs,
                 function(d) terra::shade(slope, aspect, angle = sun_angle, direction = d))
hill   <- terra::app(terra::rast(hl), mean)
hill   <- terra::focal(hill, w = 3, fun = "mean", na.rm = TRUE)   # de-speckle SRTM
hr     <- as.numeric(stats::quantile(terra::values(hill, mat = FALSE),
                                     c(0.02, 0.98), na.rm = TRUE))
hill   <- (terra::clamp(hill, hr[1], hr[2]) - hr[1]) / (hr[2] - hr[1])

## hypsometric wash over the full elevation range: clamping to 2-98% flattened
## the highest ground (the NW ridges) onto a single ramp stop
dem_lims <- as.numeric(terra::minmax(dem))
ev <- terra::values(dem)[, 1]
if (identical(palette_mode, "landscape")) {
  ## fixed anchors, not this frame's own min/max, so the colour of a given
  ## elevation does not move when the frame does
  u <- pmin(1, pmax(0, (ev - lsc_ramp[["lo"]]) /
                       (lsc_ramp[["hi"]] - lsc_ramp[["lo"]]))) ^ lsc_gamma
  ## HAND, mixed in for the reason the block script sets out: absolute elevation
  ## paints a 1800 m mountainside the same khaki as a basin floor 400 m below it,
  ## because what the eye reads as "mountain" is height above the local valley,
  ## not height above the sea. _hand.tif is already on this exact grid.
  ## The tight frame's _hand.tif stops at 26.12 N, so a widened frame needs its
  ## own. It is derived once from the regional breached DEM and stream raster the
  ## drainage step already cached, then reused.
  hand_f <- file.path(cache_dir, if (use_regional) "_hand_regional.tif" else "_hand.tif")
  if (use_regional && !file.exists(hand_f)) {
    message("Building _hand_regional.tif (height above nearest drainage) ...")
    if (requireNamespace("whitebox", quietly = TRUE)) {
      whitebox::wbt_elevation_above_stream(
        dem     = file.path(cache_dir, "_dem_breached_regional.tif"),
        streams = file.path(cache_dir, "_streams_regional.tif"),
        output  = hand_f)
    } else message("  whitebox not installed - the HAND term will be skipped")
  }
  if (file.exists(hand_f) && use_regional) {
    hr <- terra::crop(terra::rast(hand_f), terra::ext(dem))
    hr <- terra::resample(hr, dem, method = "bilinear")
    terra::writeRaster(hr, file.path(cache_dir, "_hand_frame.tif"), overwrite = TRUE)
    hand_f <- file.path(cache_dir, "_hand_frame.tif")
  }
  if (lsc_relief_mix > 0 && file.exists(hand_f)) {
    hd <- terra::values(terra::rast(hand_f))[, 1]
    hd[is.na(hd)] <- 0
    stopifnot(length(hd) == length(ev))
    uh <- pmin(1, pmax(0, hd / lsc_hand_ref)) ^ lsc_hand_gamma
    u  <- (1 - lsc_relief_mix) * u + lsc_relief_mix * uh
  }
  message(sprintf("landscape ramp: %.0f-%.0f m, gamma %.2f, HAND mix %.2f",
                  lsc_ramp[["lo"]], lsc_ramp[["hi"]], lsc_gamma, lsc_relief_mix))
} else {
  u <- pmin(1, pmax(0, (ev - dem_lims[1]) / diff(dem_lims)))
}
ok <- !is.na(u)
base_hex <- rep(NA_character_, length(u))
base_hex[ok] <- scales::colour_ramp(hyps_cols)(u[ok])

m   <- grDevices::col2rgb(ifelse(is.na(base_hex), "#000000", base_hex)) / 255
## compress the hypsometric contrast toward a paper neutral
pap <- as.numeric(grDevices::col2rgb(paper_col)) / 255
m   <- m * hyps_strength + pap * (1 - hyps_strength)
hsl <- rgb2hsl(m[1, ], m[2, ], m[3, ])
hh  <- terra::values(hill)[, 1]; hh[is.na(hh)] <- 0.5
tt  <- 2 * hh - 1                       # -1 = full shadow ... +1 = full light
## interpolate toward the endpoints by the REMAINING headroom, so neither the
## pale summits nor the dark gorges ever clip and both keep their shading
Ln  <- hsl$l + ifelse(tt > 0, relief_hi * tt * (1 - hsl$l), relief_lo * tt * hsl$l)
Ln  <- pmin(1, pmax(0, Ln))
o   <- hsl2rgb(hsl$h, hsl$s, Ln)
## warm light / cool shade: nudges lit faces to cream and shadows to slate-blue
w_lit <- pmax(0, tt) * warm_gain
w_shd <- pmax(0, -tt) * cool_gain
warm  <- grDevices::col2rgb("#FFF4E2") / 255
cool  <- grDevices::col2rgb("#4C5A6B") / 255
keep  <- 1 - w_lit - w_shd
RR <- (o$r * keep + warm[1] * w_lit + cool[1] * w_shd) * 255
GG <- (o$g * keep + warm[2] * w_lit + cool[2] * w_shd) * 255
BB <- (o$b * keep + warm[3] * w_lit + cool[3] * w_shd) * 255
RR[!ok] <- NA; GG[!ok] <- NA; BB[!ok] <- NA
shaded <- terra::rast(dem, nlyr = 3)
terra::values(shaded) <- cbind(RR, GG, BB)
shaded <- terra::clamp(shaded, 0, 255)
names(shaded) <- c("r", "g", "b")

dem_s   <- terra::focal(dem, w = 9, fun = "mean", na.rm = TRUE)   # smoother contours
dem_key <- terra::aggregate(dem, 8, fun = "mean", na.rm = TRUE)   # legend only (covered)

## `bbx` and, below, the plot object `p` are both read from outside this script:
## fig01_export_panels.R sources this file into its own environment and takes
## them by name to re-render the panel at its placed size. Do not rename either.
bbx <- as.numeric(as.vector(terra::ext(dem)))
lon_breaks <- seq(ceiling(bbx[1] / grat_step) * grat_step, bbx[2], by = grat_step)
lat_breaks <- seq(ceiling(bbx[3] / grat_step) * grat_step, bbx[4], by = grat_step)

## ---- build map -----------------------------------------------------------
p <- ggplot()

## The dummy raster exists only to carry the elevation colourbar (the shaded RGB
## covers it). With the colourbar off there is nothing for it to do, so skip the
## layer and its scale entirely rather than drawing 500k covered cells.
if (show_elev_legend) {
  p <- p +
    tidyterra::geom_spatraster(data = dem_key, maxcell = 5e5) +
    scale_fill_gradientn(
      colours = blend_to(hyps_cols, paper_col, hyps_strength),
      limits = dem_lims, oob = scales::squish, na.value = NA,
      name = "Elevation (m)",
      guide = guide_colourbar(
        order = 4,
        theme = theme(legend.key.width  = unit(3.2, "mm"),
                      legend.key.height = unit(20, "mm"),
                      legend.ticks = element_blank(),
                      legend.frame = element_rect(colour = "grey55",
                                                  linewidth = 0.2)))) +
    ggnewscale::new_scale_fill()
}

p <- p +
  tidyterra::geom_spatraster_rgb(data = shaded, maxcell = 2e6) +
  ## contours: texture, not information — barely there
  tidyterra::geom_spatraster_contour(
    data = dem_s, breaks = seq(1000, 5000, cont_minor),
    color = contour_col, linewidth = 0.06, alpha = 0.12) +
  tidyterra::geom_spatraster_contour(
    data = dem_s, breaks = seq(1000, 5000, cont_index),
    color = contour_col, linewidth = 0.14, alpha = 0.30) +
  ## graticule drawn as layers so it floats above the terrain
  ## graticule drawn as layers so it floats above the terrain
  {if (show_graticule)
     list(geom_vline(xintercept = lon_breaks, color = grat_col, linewidth = 0.16),
          geom_hline(yintercept = lat_breaks, color = grat_col, linewidth = 0.16))
   else NULL}

## water: lakes (polygons) under rivers (lines)
if (!is.null(lakes)) {
  ## no outline: at this scale the darker rim read as a drawn boundary rather
  ## than as a shoreline, and on the impounded reaches of the Jinsha it made the
  ## reservoirs look like polygons someone had digitised over the river
  p <- p + geom_sf(data = lakes, fill = water_col, color = NA, alpha = 0.9)
}
if (!is.null(rivers)) {
  p <- p +
    geom_sf(data = rivers, aes(linewidth = if ("w" %in% names(rivers)) w else 1),
            color = water_col, alpha = 0.9, lineend = "round") +
    scale_linewidth_continuous(range = c(0.12, 0.45), guide = "none")
}

## optional basin tint (off by default). Basin names are added by hand in
## post-production, so nothing is drawn for them here.
if (show_basin_tint) {
  p <- p + geom_sf(data = hulls, aes(fill = basin), color = NA, alpha = 0.09,
                   show.legend = FALSE)
}

## sites: one fixed size; fill = basin, shape = geomorphic position
p <- p +
  geom_sf(data = sites, aes(fill = basin, shape = geomorph),
          size = site_size, color = "white", stroke = site_stroke, alpha = 0.98) +
  scale_fill_manual(
    values = basin_cols, name = "Basin",
    guide = guide_legend(order = 1,
      override.aes = list(shape = 21, size = 2.6, colour = "white", stroke = 0.45))) +
  scale_shape_manual(
    values = geomorph_shapes, name = "Geomorphic position", drop = FALSE,
    guide = guide_legend(order = 2,
      override.aes = list(fill = "grey45", colour = "white", size = 2.6, stroke = 0.45)))

## The highlight is a TARGET RING, not a thicker outline on the symbol. Painting
## the marker's own ring yellow reads as a mistake — a symbol whose stroke does
## not match the others — and at 0.9 stroke it also swelled the dot enough to
## misplace it against its neighbours. Instead the marker keeps its white ring
## and its basin fill exactly like every other locality, and the emphasis is put
## OUTSIDE it, with a gap: a soft aura, then a hairline ring. The ring is fixed
## at shape 21 so it stays circular whatever geomorphic symbol the site carries.
if (highlight_ring_show && !is.na(highlight_code) &&
    any(sites$code == highlight_code)) {
  hl_site <- sites[sites$code == highlight_code, ]
  p <- p +
    geom_sf(data = hl_site, shape = 21, fill = NA, colour = highlight_ring,
            size = 5.6, stroke = 1.5, alpha = 0.28, show.legend = FALSE) +
    geom_sf(data = hl_site, shape = 21, fill = NA, colour = highlight_ring,
            size = 4.1, stroke = 0.5, alpha = 0.95, show.legend = FALSE)
}

if (show_site_labels) {
  p <- p + ggrepel::geom_text_repel(
    ## one repel layer, not two: splitting the highlighted code into its own
    ## layer would let the two sets repel independently and overlap. The colour
    ## is carried as data and passed straight through by scale_colour_identity().
    data = transform(sites,
                     .lab_col  = ifelse(code == highlight_code,
                                        highlight_label, label_col),
                     ## the highlighted code is set a quarter larger again
                     .lab_size = ifelse(code == highlight_code,
                                        label_size * 1.25, label_size)),
    aes(geometry = geometry, label = code, colour = .lab_col, size = .lab_size),
    stat = "sf_coordinates", fontface = "bold",
    bg.color = grDevices::adjustcolor("white", alpha.f = 0.8), bg.r = 0.13,
    ## point.padding lifted so the label clears the target ring rather than
    ## being repelled only from the dot at its centre
    seed = 42, max.overlaps = Inf, force = 7, force_pull = 0.45,
    box.padding = 0.28, point.padding = 0.30, min.segment.length = 0,
    segment.color = "grey45", segment.size = 0.22,
    segment.curvature = -0.12, segment.ncp = 3) +
    scale_colour_identity() + scale_size_identity()
}
if (show_river_labels && !is.null(rivers)) {
  river_labels <- data.frame(label = c("Sangyuan R.", "Liandong R.", "Caifeng R."),
                             lon = c(100.545, 100.495, 100.430),
                             lat = c(25.985,  25.875,  26.010))
  p <- p + geom_text(data = river_labels, aes(lon, lat, label = label),
                     inherit.aes = FALSE, color = water_col, fontface = "italic",
                     size = 2.6)
}

## ---- furniture: hairline scale bar + slim arrow ---------------------------
## a plain needle with an "N" above it — quieter than any of the ggspatial
## presets, which are drawn for topographic sheets rather than journal figures
north_needle <- grid::gTree(children = grid::gList(
  grid::linesGrob(
    x = grid::unit(c(0.5, 0.5), "npc"), y = grid::unit(c(0.0, 0.66), "npc"),
    arrow = grid::arrow(type = "closed", angle = 20, length = grid::unit(3.2, "mm")),
    gp = grid::gpar(col = label_col, fill = label_col, lwd = 2.0, lineend = "butt")),
  grid::textGrob("N", x = 0.5, y = 0.90,
                 gp = grid::gpar(col = label_col, fontsize = 10, fontface = "bold"))))

p <- p +
  ## width_hint 0.33 of a ~31 km frame -> annotation_scale picks a 10 km bar
  annotation_scale(
    location = "bl", style = "ticks", width_hint = 0.33,
    height = unit(0.26, "cm"), line_width = 1.5, tick_height = 0.8,
    text_cex = 0.8, text_col = label_col, line_col = label_col,
    text_face = "bold", text_family = "",
    pad_x = unit(0.45, "cm"), pad_y = unit(0.45, "cm")) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(1.2, "cm"), width = unit(0.7, "cm"),
    pad_x = unit(0.45, "cm"), pad_y = unit(0.45, "cm"),
    style = north_needle) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4], expand = FALSE) +
  scale_x_continuous(breaks = if (show_graticule) lon_breaks else NULL) +
  scale_y_continuous(breaks = if (show_graticule) lat_breaks else NULL)

## ---- titles + theme (house style of scripts/figures/*.R) ------------------
ttl <- list(title = "Quina sites of the Binchuan and Huangping basins",
            subtitle = "Shaded relief and drainage rendered from the SRTM DEM",
            caption = paste0(
              "Relief shading, ", cont_minor, "/", cont_index,
              " m contours and the channel network are derived from the SRTM DEM; ",
              "channels drawn drain\nmore than ",
              ifelse(is.na(river_km2), "the display threshold",
                     sprintf("%.0f km²", river_km2)),
              ". Rings mark the excavated, dated anchors LT (Longtan) and ",
              "THC (Tianhua Cave)."))
if (for_manuscript) ttl <- list(title = NULL, subtitle = NULL, caption = NULL)

p <- p +
  labs(x = NULL, y = NULL, title = ttl$title, subtitle = ttl$subtitle,
       caption = ttl$caption) +
  theme_minimal(base_size = 9) +
  theme(
    panel.border      = element_rect(color = "#202124", fill = NA,
                                     linewidth = border_lw),
    panel.grid        = element_blank(),
    panel.background  = element_rect(color = NA, fill = "white"),
    plot.background   = element_rect(color = NA, fill = "white"),
    axis.ticks        = if (show_graticule)
      element_line(color = "#202124", linewidth = 0.3) else element_blank(),
    axis.ticks.length = unit(if (show_graticule) 2 else 0, "pt"),
    axis.text         = if (show_graticule)
      element_text(color = "#303238", size = 6) else element_blank(),
    legend.position   = if (show_legend) "right" else "none",
    legend.title      = element_text(size = 8.5),
    legend.text       = element_text(size = 8),
    legend.key        = element_blank(),
    legend.key.size   = unit(10, "pt"),
    legend.spacing.y  = unit(6, "pt"),
    legend.margin     = margin(l = 2, r = 0),
    plot.title        = element_text(size = 10, face = "bold", color = "#202124"),
    plot.subtitle     = element_text(size = 8, color = "#5A5750",
                                     margin = margin(b = 4)),
    plot.caption      = element_text(size = 6.2, hjust = 0, color = "#5A5750",
                                     lineheight = 1.15, margin = margin(t = 4)),
    plot.margin       = margin(5, 5, 4, 4))

## ---- export (150 mm wide @ 600 dpi, as in scripts/figures/*.R) ------------
FIG_W_MM <- 150
FIG_H_MM <- 148   # no title/caption: just the panel + axis text
FIG_DPI  <- 600
map_stem <- paste0(if (identical(palette_mode, "landscape"))
  "map_quina_sites_landscape" else "map_quina_sites",
  if (use_regional) "_north" else "")

## A caller that only wants the shaded raster — terra_map_3D_hyps.R drapes it on
## the block — sets SKIP_EXPORT and skips two 600 dpi writes it would throw away.
if (exists("SKIP_EXPORT", inherits = FALSE) && isTRUE(SKIP_EXPORT)) {
  message("terra_map_2D.R: SKIP_EXPORT set, `shaded` built but nothing written")
} else {
ggsave(file.path(output_dir, paste0(map_stem, ".png")), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(output_dir, paste0(map_stem, ".pdf")), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
message("terra_map_2D.R done -> output/maps/", map_stem, ".(png|pdf)")
}
