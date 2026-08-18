## terra_map_2D_regional.R — the Figure 1 plan map (panel B of figures/study_area.png)
## re-drawn over a WIDER frame that reaches north to the Jinsha River (金沙江).
##
## Cartographically this is scripts/map/terra_map_2D.R: the same relief model, the
## same hypsometric wash, the same accumulation-thinned drainage, the same symbol
## grammar, theme, type sizes and export geometry. Only three things differ, and
## each is forced by the larger frame:
##   * its own cache. setup.R downloads the DEM for the site extent + 8 km only
##     (100.318-100.630 E, 25.746-26.120 N), which stops ~6 km short of the
##     Jinsha, so this script builds and caches its own regional layers next to
##     the existing ones (data/cache/*_regional.*). setup.R is NOT required.
##   * the hydrology is solved on a frame buffered well beyond the display window
##     (north to 27.1 N), and by BREACHING rather than filling depressions.
##     Two separate reasons:
##       - flow accumulation only counts cells inside the grid it is solved on, so
##         a network solved on the display frame alone would have the Jinsha
##         entering with near-zero upstream area and drawn as a hairline. The
##         buffer lets the trunk arrive already carrying its catchment.
##       - setup.R's wbt_fill_depressions() cannot be used on a frame this size.
##         The Jinsha leaves through the EAST edge (1105 m) and WhiteboxTools'
##         fill raises that outlet cell instead of treating it as one, so the
##         whole gorge ponds to a flat 1221 m and D8 then drains the entire
##         raster NORTHWARD - accumulation ends up decreasing downstream and the
##         trunk is drawn thinner the further it goes. wbt_breach_depressions()
##         (unconstrained; the least-cost variant needs a search distance longer
##         than the 70 km pond) carves the outlet instead and routes correctly.
##         Verified: raised cells drop from 75,422 to 0 on the same crop.
##   * graticule at 0.1 deg instead of 0.05 (the frame is 2.4x the area, so the
##     0.05 labels would collide). River names stay off, as in terra_map_2D.R.
##
## Frame:   display 100.27-100.77 E, 25.74-26.30 N  (~50 x 62 km). The west edge
##          stops at 100.27 on purpose: Erhai's shoreline ends at ~100.25, and a
##          sliver of it at the frame edge would read as an artefact. The top at
##          26.30 clears the whole east-flowing reach of the Jinsha, which runs
##          between 26.16 and 26.22 N across this window.
##          compute 100.10-101.00 E, 25.62-27.10 N  (hydrology only)
##
## Output:  output/figures/fig01_panels/panel_B_map_regional_jinsha.(png|pdf)
##          at 150 x 148 mm / 600 dpi, i.e. the native geometry of terra_map_2D.R,
##          so every point size on the sheet is literally the same as that script's.
##
## First run downloads a ~1.3 deg^2 SRTM DEM and solves the drainage with
## WhiteboxTools (several minutes, ~200 tiles). Everything is cached; re-runs are
## cheap. To rebuild a layer, delete its file from data/cache/ and run again.

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
output_dir <- file.path(proj_dir, "output", "figures", "fig01_panels")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- display toggles -----------------------------------------------------
for_manuscript    <- TRUE   # TRUE -> drop title/subtitle/caption (the .qmd carries them)
show_elev_legend  <- FALSE  # the elevation colourbar; off for the Fig. 1 panel
show_site_labels  <- TRUE
show_basin_tint   <- FALSE  # convex hulls read as marquee boxes
show_river_labels <- FALSE  # river names are added by hand in post-production

## ---- frame ---------------------------------------------------------------
## disp = what the reader sees. cmp = what the flow routing is solved on: the
## Jinsha enters this window from the north, so the buffer is pushed that way.
## (name order is st_bbox's, which is strict about it)
disp_ext <- c(xmin = 100.27, ymin = 25.74, xmax = 100.77, ymax = 26.30)
cmp_ext  <- c(xmin = 100.10, ymin = 25.62, xmax = 101.00, ymax = 27.10)
dem_zoom <- 11              # elevatr zoom, as in setup.R -> ~32 m cells here

## ---- cartographic parameters ---------------------------------------------
## Identical to terra_map_2D.R except where the comment says otherwise.
z_exag        <- 1.8              # hillshade vertical exaggeration
sun_angle     <- 35               # sun elevation (deg)
sun_dirs      <- c(300, 337, 15)  # multi-light azimuths, averaged
relief_hi     <- 0.42             # 0..1, pull toward white on lit faces
relief_lo     <- 0.38             # 0..1, pull toward black in shadow
warm_gain     <- 0.10             # warm tint added on lit slopes
cool_gain     <- 0.20             # cool tint added in shadow
hyps_strength <- 0.72             # 1 = full hypsometric tint, 0 = plain paper
## 60000 cells on the small frame; the wider frame packs 1.6x more channel length
## into the same printed width, so the threshold is raised to hold the on-paper
## drainage density of the original panel.
river_min_acc <- 100000           # min upstream cells for a channel to be drawn
stream_extract_thresh <- 6000     # WhiteboxTools vectorising threshold (<< the above)
lake_min_ha   <- 5                # drop OSM ponds smaller than this
cont_minor    <- 250              # contour intervals (m)
cont_index    <- 500
grat_step     <- 0.10             # graticule / axis-break spacing (degrees)

## ---- palette --------------------------------------------------------------
## verbatim from terra_map_2D.R — this is the whole point of the exercise
hyps_cols  <- c("#7E8E74", "#93A184", "#A9B195", "#BFBCA4",
                "#D1C9B3", "#DCD4C1", "#E6DECB")
paper_col  <- "#E9E4D8"           # neutral the tint is blended toward
blend_to <- function(cols, to, k) {
  a <- grDevices::col2rgb(cols) / 255; b <- as.numeric(grDevices::col2rgb(to)) / 255
  m <- a * k + b * (1 - k)
  grDevices::rgb(m[1, ], m[2, ], m[3, ])
}
basin_cols <- c(Binchuan = "#A0364B", Heqing = "#2F6489")
water_col  <- "#86A6BB"
river_lab_col <- "#4E7893"        # water_col darkened so a name reads on the sheet
contour_col<- "#6B6357"
label_col  <- "#332F29"
grat_col   <- grDevices::adjustcolor("white", alpha.f = 0.78)
geomorph_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)

## ==========================================================================
## CACHE — DEM, drainage, water. Self-contained: setup.R is not needed.
## ==========================================================================
dem_cmp_tif <- file.path(cache_dir, "dem_regional.tif")
breached <- file.path(cache_dir, "_dem_breached_regional.tif")
d8ptr   <- file.path(cache_dir, "_d8_pointer_regional.tif")
d8acc   <- file.path(cache_dir, "_d8_accum_regional.tif")
strast  <- file.path(cache_dir, "_streams_regional.tif")
strshp  <- file.path(cache_dir, "_streams_regional.shp")
rivers_gpkg <- file.path(cache_dir, "rivers_dem_regional.gpkg")
water_gpkg  <- file.path(cache_dir, "water_regional.gpkg")

## ---- 1. DEM over the compute frame ---------------------------------------
if (!file.exists(dem_cmp_tif)) {
  message("Downloading the regional DEM via elevatr (this is the slow step) ...")
  bb_sf <- st_as_sf(st_as_sfc(st_bbox(cmp_ext, crs = 4326)))
  ## elevatr refuses large requests unless the size check is waived; 1.3 deg^2 at
  ## z=11 is ~13 M cells, which is well within what the rest of this script handles
  dem_cmp <- tryCatch(
    elevatr::get_elev_raster(bb_sf, z = dem_zoom, clip = "bbox",
                             override_size_check = TRUE),
    error = function(e) elevatr::get_elev_raster(bb_sf, z = dem_zoom, clip = "bbox"))
  dem_cmp <- terra::rast(dem_cmp)
  names(dem_cmp) <- "elev"
  terra::writeRaster(dem_cmp, dem_cmp_tif, overwrite = TRUE)
} else {
  message("Using cached regional DEM.")
}
dem_cmp <- terra::rast(dem_cmp_tif)

## ---- 2. drainage, solved on the buffered frame ---------------------------
if (file.exists(rivers_gpkg) && file.exists(d8acc)) {
  message("Using cached regional channel network.")
} else {
  if (!requireNamespace("whitebox", quietly = TRUE)) install.packages("whitebox")
  library(whitebox)
  if (!whitebox::check_whitebox_binary()) whitebox::install_whitebox()
  message("Solving the drainage on the buffered frame (breach -> D8 -> accum -> vector) ...")
  ## breach, not fill — see the header note on the ponded Jinsha gorge
  wbt_breach_depressions(dem = dem_cmp_tif, output = breached)
  wbt_d8_pointer(dem = breached, output = d8ptr)
  wbt_d8_flow_accumulation(input = breached, output = d8acc, out_type = "cells")
  wbt_extract_streams(flow_accum = d8acc, output = strast,
                      threshold = stream_extract_thresh)
  wbt_raster_streams_to_vector(streams = strast, d8_pntr = d8ptr, output = strshp)
  rv <- sf::st_read(strshp, quiet = TRUE)
  if (is.na(sf::st_crs(rv))) sf::st_crs(rv) <- terra::crs(dem_cmp)
  sf::st_write(sf::st_transform(rv, 4326), rivers_gpkg, delete_dsn = TRUE, quiet = TRUE)
  message("Wrote rivers_dem_regional.gpkg (", nrow(rv), " line features).")
}

## ---- 3. lakes / reservoirs over the display frame (OSM, optional) --------
if (!file.exists(water_gpkg)) {
  ok <- tryCatch({
    wat <- osmdata::opq(bbox = disp_ext[c("xmin", "ymin", "xmax", "ymax")],
                        timeout = 300) |>
      osmdata::add_osm_feature("natural", "water") |>
      osmdata::osmdata_sf()
    poly <- wat$osm_polygons
    if (!is.null(poly) && nrow(poly) > 0) {
      sf::st_write(poly["osm_id"], water_gpkg, delete_dsn = TRUE, quiet = TRUE)
      message("Wrote water_regional.gpkg (", nrow(poly), " polygons).")
    }
    TRUE
  }, error = function(e) { message("OSM water fetch failed: ", conditionMessage(e)); FALSE })
} else {
  message("Using cached regional water polygons.")
}

## ==========================================================================
## LAYERS
## ==========================================================================
## ---- sites (Site_information.xlsx = source of truth) ----------------------
sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = names(geomorph_shapes))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

hulls <- sites |>
  group_by(basin) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  st_convex_hull() |>
  st_transform(32647) |> st_buffer(1100) |> st_transform(4326)

## ---- DEM for the sheet ---------------------------------------------------
## Cropped a hair wider than the display window: terra::terrain() returns NA on
## the outer cell ring, and the overhang keeps that ring outside coord_sf's limits.
pad <- 0.01
dem <- terra::crop(dem_cmp, terra::ext(disp_ext[["xmin"]] - pad, disp_ext[["xmax"]] + pad,
                                       disp_ext[["ymin"]] - pad, disp_ext[["ymax"]] + pad))

## ---- thin the drainage: keep channels by upstream contributing area -------
rivers <- st_read(rivers_gpkg, quiet = TRUE)
acc <- terra::rast(d8acc)
## clip to the display window first: extracting over the whole buffered network
## would cost minutes for lines that are never drawn
rivers <- suppressWarnings(
  st_crop(st_make_valid(rivers), st_bbox(disp_ext, crs = 4326)))
## cropping can leave POINT slivers where a channel only grazes the frame
rivers <- rivers[as.character(st_geometry_type(rivers)) %in%
                   c("LINESTRING", "MULTILINESTRING"), ]
rivers$acc <- terra::extract(acc, terra::vect(rivers), fun = max, na.rm = TRUE)[, 2]
lat0 <- mean(c(terra::ymin(acc), terra::ymax(acc)))
cell_km2 <- (terra::xres(acc) * 111.32 * cos(lat0 * pi / 180)) *
            (terra::yres(acc) * 111.32)
river_km2 <- river_min_acc * cell_km2
rivers <- rivers |>
  filter(!is.na(acc), acc >= river_min_acc) |>
  mutate(w = log10(acc))
rivers <- rivers |> st_transform(32647) |>
  st_simplify(dTolerance = 60, preserveTopology = TRUE) |> st_transform(4326)
message(sprintf("Rivers: %d channels kept (>= %.0f km2 upstream).",
                nrow(rivers), river_km2))

lakes <- if (file.exists(water_gpkg)) st_read(water_gpkg, quiet = TRUE) else NULL
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

dem_lims <- as.numeric(terra::minmax(dem))
ev <- terra::values(dem)[, 1]
u  <- pmin(1, pmax(0, (ev - dem_lims[1]) / diff(dem_lims)))
ok <- !is.na(u)
base_hex <- rep(NA_character_, length(u))
base_hex[ok] <- scales::colour_ramp(hyps_cols)(u[ok])

m   <- grDevices::col2rgb(ifelse(is.na(base_hex), "#000000", base_hex)) / 255
pap <- as.numeric(grDevices::col2rgb(paper_col)) / 255
m   <- m * hyps_strength + pap * (1 - hyps_strength)
hsl <- rgb2hsl(m[1, ], m[2, ], m[3, ])
hh  <- terra::values(hill)[, 1]; hh[is.na(hh)] <- 0.5
tt  <- 2 * hh - 1                       # -1 = full shadow ... +1 = full light
Ln  <- hsl$l + ifelse(tt > 0, relief_hi * tt * (1 - hsl$l), relief_lo * tt * hsl$l)
Ln  <- pmin(1, pmax(0, Ln))
o   <- hsl2rgb(hsl$h, hsl$s, Ln)
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

## `bbx` and the plot object `p` keep the names terra_map_2D.R uses, so an
## export wrapper can source this file and take them the same way.
bbx <- as.numeric(disp_ext[c("xmin", "xmax", "ymin", "ymax")])
lon_breaks <- seq(ceiling(bbx[1] / grat_step) * grat_step, bbx[2], by = grat_step)
lat_breaks <- seq(ceiling(bbx[3] / grat_step) * grat_step, bbx[4], by = grat_step)

## ---- build map -----------------------------------------------------------
p <- ggplot()

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
  tidyterra::geom_spatraster_rgb(data = shaded, maxcell = 4e6) +
  tidyterra::geom_spatraster_contour(
    data = dem_s, breaks = seq(1000, 5000, cont_minor),
    color = contour_col, linewidth = 0.06, alpha = 0.12) +
  tidyterra::geom_spatraster_contour(
    data = dem_s, breaks = seq(1000, 5000, cont_index),
    color = contour_col, linewidth = 0.14, alpha = 0.30) +
  geom_vline(xintercept = lon_breaks, color = grat_col, linewidth = 0.16) +
  geom_hline(yintercept = lat_breaks, color = grat_col, linewidth = 0.16)

if (!is.null(lakes)) {
  p <- p + geom_sf(data = lakes, fill = water_col,
                   color = grDevices::adjustcolor(water_col, red.f = 0.8,
                                                  green.f = 0.8, blue.f = 0.85),
                   linewidth = 0.15, alpha = 0.9)
}
## The Jinsha now spans three orders of magnitude of upstream area above the
## valley channels, so the top of the width range is opened up (0.45 -> 0.60);
## the bottom, which sets how the study-area streams read, is unchanged.
p <- p +
  geom_sf(data = rivers, aes(linewidth = w),
          color = water_col, alpha = 0.9, lineend = "round") +
  scale_linewidth_continuous(range = c(0.12, 0.60), guide = "none")

if (show_basin_tint) {
  p <- p + geom_sf(data = hulls, aes(fill = basin), color = NA, alpha = 0.09,
                   show.legend = FALSE)
}

p <- p +
  geom_sf(data = sites, aes(fill = basin, shape = geomorph),
          size = 2.2, color = "white", stroke = 0.45, alpha = 0.98) +
  scale_fill_manual(
    values = basin_cols, name = "Basin",
    labels = c(Binchuan = "Binchuan", Heqing = "Huangping"),
    guide = guide_legend(order = 1,
      override.aes = list(shape = 21, size = 2.6, colour = "white", stroke = 0.45))) +
  scale_shape_manual(
    values = geomorph_shapes, name = "Geomorphic position", drop = FALSE,
    guide = guide_legend(order = 2,
      override.aes = list(fill = "grey45", colour = "white", size = 2.6, stroke = 0.45)))

if (show_site_labels) {
  p <- p + ggrepel::geom_text_repel(
    data = sites, aes(geometry = geometry, label = code),
    stat = "sf_coordinates", size = 2.0, fontface = "bold", color = label_col,
    bg.color = grDevices::adjustcolor("white", alpha.f = 0.8), bg.r = 0.13,
    ## type size is unchanged, but the cluster now occupies a third of the panel
    ## instead of most of it, so at the source repulsion (force 7 / pull 0.45) the
    ## codes were being thrown up to 9 km from their symbols. Stronger pull and
    ## weaker push keep each code beside its own site; nothing else is retuned.
    seed = 42, max.overlaps = Inf, force = 3.5, force_pull = 1.6,
    box.padding = 0.22, point.padding = 0.14, min.segment.length = 0,
    segment.color = "grey45", segment.size = 0.22,
    segment.curvature = -0.12, segment.ncp = 3)
}

## ---- river names ---------------------------------------------------------
## The Jinsha label is anchored on the extracted trunk rather than typed in by
## hand, so it follows the channel if the frame or the threshold is changed.
if (show_river_labels && nrow(rivers) > 0) {
  ## the trunk is every segment within a factor of two of the largest upstream
  ## area — one segment alone is a few hundred metres of channel and would put
  ## the name wherever that fragment happens to sit
  trunk <- rivers[rivers$acc >= 0.5 * max(rivers$acc), ]
  xy    <- sf::st_coordinates(trunk)[, 1:2, drop = FALSE]
  anchor_lon <- bbx[1] + 0.74 * diff(bbx[1:2])
  at    <- xy[which.min(abs(xy[, 1] - anchor_lon)), ]
  river_labels <- data.frame(label = "Jinsha River",
                             lon   = at[["X"]],
                             lat   = at[["Y"]] + 0.032)   # clears the 26.2 graticule
  p <- p + geom_text(data = river_labels, aes(lon, lat, label = label),
                     inherit.aes = FALSE, color = river_lab_col,
                     fontface = "italic", size = 2.6)
}

## ---- furniture: hairline scale bar + slim arrow ---------------------------
north_needle <- grid::gTree(children = grid::gList(
  grid::linesGrob(
    x = grid::unit(c(0.5, 0.5), "npc"), y = grid::unit(c(0.0, 0.66), "npc"),
    arrow = grid::arrow(type = "closed", angle = 20, length = grid::unit(3.2, "mm")),
    gp = grid::gpar(col = label_col, fill = label_col, lwd = 2.0, lineend = "butt")),
  grid::textGrob("N", x = 0.5, y = 0.90,
                 gp = grid::gpar(col = label_col, fontsize = 10, fontface = "bold"))))

p <- p +
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
  scale_x_continuous(breaks = lon_breaks) +
  scale_y_continuous(breaks = lat_breaks)

## ---- titles + theme (house style of scripts/figures/*.R) ------------------
ttl <- list(title = "Quina sites of the Binchuan and Heqing basins",
            subtitle = "Regional frame: the study valleys and the Jinsha River to the north",
            caption = paste0(
              "Relief shading, ", cont_minor, "/", cont_index,
              " m contours and the channel network are derived from the SRTM DEM; ",
              "channels drawn drain\nmore than ",
              sprintf("%.0f km²", river_km2),
              ". Flow routing is solved by depression breaching on a frame ",
              "buffered north to 27.1°N, so the Jinsha arrives with its catchment."))
if (for_manuscript) ttl <- list(title = NULL, subtitle = NULL, caption = NULL)

p <- p +
  labs(x = NULL, y = NULL, title = ttl$title, subtitle = ttl$subtitle,
       caption = ttl$caption) +
  theme_minimal(base_size = 9) +
  theme(
    panel.border      = element_rect(color = "#202124", fill = NA, linewidth = 0.5),
    panel.grid        = element_blank(),
    panel.background  = element_rect(color = NA, fill = "white"),
    plot.background   = element_rect(color = NA, fill = "white"),
    axis.ticks        = element_line(color = "#202124", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.text         = element_text(color = "#303238", size = 6),
    legend.position   = "right",
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
FIG_H_MM <- 148
FIG_DPI  <- 600
base_name <- "panel_B_map_regional_jinsha"
ggsave(file.path(output_dir, paste0(base_name, ".png")), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(output_dir, paste0(base_name, ".pdf")), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
message("terra_map_2D_regional.R done -> ", file.path(output_dir, base_name), ".(png|pdf)")
