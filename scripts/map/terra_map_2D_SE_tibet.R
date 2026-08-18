## terra_map_2D_SE_tibet.R — regional relief map of the southeastern margin of
## the Tibetan Plateau. Terrain, drainage and lakes only: no sites, no symbols,
## no legend, no place names.
##
## Same cartography as scripts/map/terra_map_2D.R and terra_map_2D_regional.R —
## relief shading applied to the HSL L-channel with a warm-light / cool-shadow
## tint, the same hypsometric wash blended toward the same paper neutral, the
## same water colour, graticule, scale bar, north needle, panel border and theme.
## What the wider frame forces to change is noted where it happens; the four
## substantive ones are:
##   * contours are dropped. Over 6 km of relief and 600 km of width they
##     collapse into a grey wash and fight the shading.
##   * the palette is widened and the relief gains eased — see the palette block.
##   * trunk rivers come from Natural Earth rather than from the DEM. A D8
##     network on this frame would give every trunk a hairline start at the
##     boundary it enters through: Salween, Mekong, Yangtze and Yalong all
##     arrive from outside.
##   * lakes are detected in the DEM — see the lake block for why.
##
## Frame:   97-103 E, 24-29 N. 6 x 5 degrees, ~598 x 553 km.
##          Holds the Three Parallel Rivers gorges (Nu/Salween, Lancang/Mekong,
##          Jinsha/upper Yangtze) and the Yalong, the Hengduan ranges, the
##          Yunnan plateau and its lake district, and the plateau margin itself.
##          The Binchuan study area (100.5 E, 25.9 N) sits near the centre.
##
##          NOTE ON SHAPE: at these bounds the panel comes out 1.07:1, i.e.
##          effectively square, not the landscape rectangle the previous frame
##          gave. That is simply what 6 x 5 degrees at 26.5 N is. To get a
##          landscape sheet back without losing anything, widen the longitude:
##          96-104 E gives 1.43:1.
##
## Projection: EPSG:4326 under coord_sf, not a conic. Over this frame a conic
## would buy little in shape fidelity and cost the rectangular graticule.
##
## Output:  output/figures/map_SE_tibet_relief.(png|pdf), 210 x 196 mm @ 600 dpi.
##
## First run downloads a 30 deg^2 DEM (~17 M cells at z=9) and the Natural Earth
## rivers. Everything is cached under data/cache/; delete a file to rebuild it.

library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
sf::sf_use_s2(FALSE)

proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "figures")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- display toggles -----------------------------------------------------
for_manuscript  <- TRUE    # TRUE -> no title/subtitle
show_rivers     <- TRUE
show_lakes      <- TRUE
show_study_box  <- FALSE   # outline of the Fig. 1 frame; off, nothing is marked

## ---- frame ---------------------------------------------------------------
disp_ext <- c(xmin = 97, ymin = 24, xmax = 103, ymax = 29)
dem_zoom <- 9              # elevatr zoom -> ~130 m cells, ~1:1 with the export

## ---- cartographic parameters ---------------------------------------------
z_exag        <- 2.0       # 1.8 on the 30 m sheets, 2.2 at 265 m: coarser cells
                           # average the slopes away, so the lift tracks cell size
sun_angle     <- 35
sun_dirs      <- c(300, 337, 15)
relief_hi     <- 0.36      # eased from 0.42/0.38: at this width the hillshade was
relief_lo     <- 0.32      # swinging each pixel further in L than 3 km of
                           # elevation did, so regional height stopped reading
warm_gain     <- 0.10
cool_gain     <- 0.20
hyps_strength <- 0.95      # 0.72 on the local sheets, where the relief carried
                           # the map and the tint was only a wash. Here elevation
                           # is the subject, so the palette gets its full range
grat_step     <- 1         # degrees

## ---- palette --------------------------------------------------------------
## Same family as terra_map_2D.R -- sage valley floors through sand to stone --
## but widened at both ends. That palette spans L 55-88 because it only ever had
## to wash 2 km of relief and the shading was meant to dominate it. Here it has
## to carry more than 5 km, so the low stop goes to a deeper green and the range
## opens to L 36-85. The top stop is still held short of white, for the reason
## the source script gives: the headroom-aware L interpolation needs somewhere to
## go, or the high ground renders as a featureless blob.
hyps_cols  <- c("#4E6B54", "#6B8263", "#889777", "#A3A98B",
                "#BAB79F", "#CEC7B0", "#DED6C2", "#E9E1CF")
paper_col  <- "#E9E4D8"
water_col  <- "#86A6BB"
lake_col   <- "#7FA1B8"    # a touch deeper than the rivers, so lakes read as body
sea_col    <- "#9FBACB"    # inert on this inland frame; kept for wider extents
label_col  <- "#332F29"
grat_col   <- grDevices::adjustcolor("white", alpha.f = 0.55)

## ==========================================================================
## CACHE
## ==========================================================================
dem_tif     <- file.path(cache_dir, "dem_se_tibet_z9.tif")
rivers_gpkg <- file.path(cache_dir, "rivers_ne10_se.gpkg")
lakes_gpkg  <- file.path(cache_dir, "lakes_dem_se.gpkg")

if (!file.exists(dem_tif)) {
  message("Downloading the SE Tibet DEM via elevatr (the slow step) ...")
  bb_sf <- st_as_sf(st_as_sfc(st_bbox(disp_ext, crs = 4326)))
  d <- tryCatch(
    elevatr::get_elev_raster(bb_sf, z = dem_zoom, clip = "bbox",
                             override_size_check = TRUE),
    error = function(e) elevatr::get_elev_raster(bb_sf, z = dem_zoom, clip = "bbox"))
  d <- terra::rast(d); names(d) <- "elev"
  terra::writeRaster(d, dem_tif, overwrite = TRUE)
  rm(d); gc()
} else {
  message("Using cached SE Tibet DEM.")
}
dem <- terra::rast(dem_tif)
message(sprintf("DEM: %d rows x %d cols at %.5f deg (~%.0f m), range %s m",
                nrow(dem), ncol(dem), terra::xres(dem),
                terra::xres(dem) * 111320 * cos(26.5 * pi / 180),
                paste(round(as.numeric(terra::minmax(dem))), collapse = " to ")))

## ---- trunk rivers: Natural Earth 10 m -------------------------------------
if (show_rivers && !file.exists(rivers_gpkg)) {
  x <- tryCatch(
    rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines",
                               category = "physical", returnclass = "sf"),
    error = function(e) { message("  river download failed: ",
                                  conditionMessage(e)); NULL })
  if (!is.null(x)) {
    x <- suppressWarnings(st_crop(st_make_valid(st_transform(x, 4326)),
                                  st_bbox(disp_ext, crs = 4326)))
    st_write(x, rivers_gpkg, delete_dsn = TRUE, quiet = TRUE)
  }
}
rivers <- if (show_rivers && file.exists(rivers_gpkg))
            st_read(rivers_gpkg, quiet = TRUE) else NULL
if (!is.null(rivers)) message("Rivers: ", nrow(rivers), " features.")

## ---- lakes: flat surfaces in the DEM --------------------------------------
## Natural Earth's 10 m lakes layer holds nothing in this frame — not a bug, it
## simply does not carry Erhai, Dianchi, Fuxian or any of the Yunnan lakes — and
## Overpass rate-limits a query over 30 deg^2. The DEM already has them: a lake
## is the one landform whose surface is exactly level, so in integer-metre
## elevation data it is a patch of cells whose 3x3 neighbourhood has zero range.
## In terrain this steep nothing else qualifies at size. On the 265 m DEM the
## detection recovered Dianchi, Erhai, Fuxian, Chenghai, Qilu, Xingyun, Lugu and
## Qionghai in their right places, with no false positives above 1 km2, plus the
## impounded reaches behind the Jinsha and Lancang dams, which are water too.
## Dilated one cell to give back the rim the focal window eats, and simplified in
## metres so the shorelines are not stair-stepped.
lake_min_km2 <- 1          # ~1.1 km across; below this a lake is a few pixels

detect_lakes <- function(d, min_km2) {
  rng  <- terra::focal(d, w = 3, fun = "max", na.rm = TRUE) -
          terra::focal(d, w = 3, fun = "min", na.rm = TRUE)
  flat <- terra::ifel(rng <= 0.5 & d > 0, 1, NA)
  flat <- terra::focal(flat, w = 3, fun = "max", na.rm = TRUE)
  pt   <- terra::patches(flat, directions = 8, zeroAsNA = TRUE)
  cell_km2 <- (terra::xres(d) * 111.32 * cos(26.5 * pi / 180)) *
              (terra::yres(d) * 111.32)
  f    <- terra::freq(pt)
  keep <- f$value[f$count * cell_km2 >= min_km2]
  message(sprintf("Lakes: %d flat patches, %d of them >= %.1f km2.",
                  nrow(f), length(keep), min_km2))
  if (!length(keep)) return(NULL)
  pt <- terra::subst(pt, from = keep, to = keep, others = NA)
  sf::st_as_sf(terra::as.polygons(pt)) |>
    sf::st_transform(32647) |>
    sf::st_simplify(dTolerance = 200, preserveTopology = TRUE) |>
    sf::st_transform(4326)
}

if (show_lakes) {
  if (file.exists(lakes_gpkg)) {
    lakes <- st_read(lakes_gpkg, quiet = TRUE)
    message("Using cached DEM lakes: ", nrow(lakes), " features.")
  } else {
    lakes <- detect_lakes(dem, lake_min_km2)
    if (!is.null(lakes)) st_write(lakes, lakes_gpkg, delete_dsn = TRUE, quiet = TRUE)
  }
} else {
  lakes <- NULL
}

## ==========================================================================
## RELIEF SHADING
## ==========================================================================
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
                 function(d) terra::shade(slope, aspect, angle = sun_angle,
                                          direction = d))
hill   <- terra::app(terra::rast(hl), mean)
rm(dem_z, slope, aspect, hl); gc()
hr     <- as.numeric(stats::quantile(terra::values(hill, mat = FALSE),
                                     c(0.02, 0.98), na.rm = TRUE))
hill   <- (terra::clamp(hill, hr[1], hr[2]) - hr[1]) / (hr[2] - hr[1])

ev <- terra::values(dem)[, 1]
hh <- terra::values(hill)[, 1]; hh[is.na(hh)] <- 0.5
ramp_lo <- 0
## The top 0.5 per cent of cells are a scatter of 5.5-6.7 km summits. Anchoring
## the ramp on them spends a third of the palette on ground a few pixels wide and
## squeezes the plateau-to-lowland contrast — the actual subject — into the
## bottom half. Cap at the 99.5th percentile instead.
ramp_hi <- as.numeric(stats::quantile(ev, 0.995, na.rm = TRUE))
message(sprintf("hypsometric ramp: %.0f to %.0f m", ramp_lo, ramp_hi))

## colorRamp returns a numeric matrix; scales::colour_ramp returns hex strings,
## and millions of those would put gigabytes of unique strings into R's string
## cache. Composited in chunks for the same reason.
cr   <- grDevices::colorRamp(hyps_cols, space = "Lab")
pap  <- as.numeric(grDevices::col2rgb(paper_col)) / 255
warm <- as.numeric(grDevices::col2rgb("#FFF4E2")) / 255
cool <- as.numeric(grDevices::col2rgb("#4C5A6B")) / 255
seac <- as.numeric(grDevices::col2rgb(sea_col)) / 255
## Only paint sea if the frame actually reaches one. This DEM carries 47 isolated
## void cells below 0 m, and without the guard each would render as a blue speck
## on ground 2 km above sea level.
has_sea <- mean(ev <= 0, na.rm = TRUE) > 1e-3
message("sea in frame: ", has_sea)

n  <- length(ev)
RR <- GG <- BB <- rep(NA_real_, n)
chunk <- 2e6
for (i0 in seq(1, n, by = chunk)) {
  j   <- i0:min(i0 + chunk - 1, n)
  e   <- ev[j]; t_ <- 2 * hh[j] - 1
  ok  <- !is.na(e)
  u   <- pmin(1, pmax(0, (e - ramp_lo) / (ramp_hi - ramp_lo)))
  u[!ok] <- 0
  m   <- cr(u) / 255
  sea <- ok & e <= 0 & has_sea
  m[!sea, ] <- m[!sea, , drop = FALSE] * hyps_strength +
               rep(pap, each = sum(!sea)) * (1 - hyps_strength)
  m[sea, 1] <- seac[1]; m[sea, 2] <- seac[2]; m[sea, 3] <- seac[3]
  hsl <- rgb2hsl(m[, 1], m[, 2], m[, 3])
  Ln  <- hsl$l + ifelse(t_ > 0, relief_hi * t_ * (1 - hsl$l),
                                relief_lo * t_ * hsl$l)
  o   <- hsl2rgb(hsl$h, hsl$s, pmin(1, pmax(0, Ln)))
  wl  <- pmax(0,  t_) * warm_gain
  ws  <- pmax(0, -t_) * cool_gain
  kp  <- 1 - wl - ws
  r <- (o$r * kp + warm[1] * wl + cool[1] * ws) * 255
  g <- (o$g * kp + warm[2] * wl + cool[2] * ws) * 255
  b <- (o$b * kp + warm[3] * wl + cool[3] * ws) * 255
  r[!ok] <- NA; g[!ok] <- NA; b[!ok] <- NA
  RR[j] <- r; GG[j] <- g; BB[j] <- b
}
rm(ev, hh); gc()

shaded <- terra::rast(dem, nlyr = 3)
terra::values(shaded) <- cbind(RR, GG, BB)
shaded <- terra::clamp(shaded, 0, 255)
names(shaded) <- c("r", "g", "b")
rm(RR, GG, BB); gc()

## ==========================================================================
## PLOT
## ==========================================================================
bbx <- as.numeric(disp_ext[c("xmin", "xmax", "ymin", "ymax")])
lon_breaks <- seq(ceiling(bbx[1] / grat_step) * grat_step, bbx[2], by = grat_step)
lat_breaks <- seq(ceiling(bbx[3] / grat_step) * grat_step, bbx[4], by = grat_step)

p <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = shaded, maxcell = 3e7) +
  geom_vline(xintercept = lon_breaks, color = grat_col, linewidth = 0.16) +
  geom_hline(yintercept = lat_breaks, color = grat_col, linewidth = 0.16)

if (!is.null(lakes) && nrow(lakes) > 0) {
  p <- p + geom_sf(data = lakes, fill = lake_col,
                   color = grDevices::adjustcolor(lake_col, red.f = 0.78,
                                                  green.f = 0.78, blue.f = 0.85),
                   linewidth = 0.15)
}
if (!is.null(rivers) && nrow(rivers) > 0) {
  ## Natural Earth ranks its rivers 0 (Yangtze, Mekong) to 10 (minor); invert
  ## that so the trunks carry the weight, exactly as upstream contributing area
  ## does on the large-scale sheets
  sr <- suppressWarnings(as.numeric(rivers$scalerank))
  rivers$w <- 10 - ifelse(is.na(sr), 8, sr)
  p <- p +
    geom_sf(data = rivers, aes(linewidth = w), color = water_col,
            alpha = 0.95, lineend = "round") +
    scale_linewidth_continuous(range = c(0.18, 0.80), guide = "none")
}
if (show_study_box) {
  box <- st_as_sfc(st_bbox(c(xmin = 100.27, ymin = 25.74,
                             xmax = 100.77, ymax = 26.30), crs = 4326))
  p <- p + geom_sf(data = box, fill = NA, color = "#A0364B", linewidth = 0.5)
}

north_needle <- grid::gTree(children = grid::gList(
  grid::linesGrob(
    x = grid::unit(c(0.5, 0.5), "npc"), y = grid::unit(c(0.0, 0.66), "npc"),
    arrow = grid::arrow(type = "closed", angle = 20, length = grid::unit(3.2, "mm")),
    gp = grid::gpar(col = label_col, fill = label_col, lwd = 2.0, lineend = "butt")),
  grid::textGrob("N", x = 0.5, y = 0.90,
                 gp = grid::gpar(col = label_col, fontsize = 10, fontface = "bold"))))

p <- p +
  annotation_scale(
    location = "bl", style = "ticks", width_hint = 0.25,
    height = unit(0.26, "cm"), line_width = 1.5, tick_height = 0.8,
    text_cex = 0.8, text_col = label_col, line_col = label_col,
    text_face = "bold", text_family = "",
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(1.2, "cm"), width = unit(0.7, "cm"),
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
    style = north_needle) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4], expand = FALSE) +
  scale_x_continuous(breaks = lon_breaks) +
  scale_y_continuous(breaks = lat_breaks)

ttl <- list(title = "Southeastern margin of the Tibetan Plateau",
            subtitle = paste("Shaded relief and lakes from the SRTM DEM;",
                             "trunk rivers from Natural Earth"))
if (for_manuscript) ttl <- list(title = NULL, subtitle = NULL)

p <- p +
  labs(x = NULL, y = NULL, title = ttl$title, subtitle = ttl$subtitle) +
  theme_minimal(base_size = 9) +
  theme(
    panel.border      = element_rect(color = "#202124", fill = NA, linewidth = 0.5),
    panel.grid        = element_blank(),
    panel.background  = element_rect(color = NA, fill = "white"),
    plot.background   = element_rect(color = NA, fill = "white"),
    axis.ticks        = element_line(color = "#202124", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.text         = element_text(color = "#303238", size = 7),
    legend.position   = "none",
    plot.title        = element_text(size = 10, face = "bold", color = "#202124"),
    plot.subtitle     = element_text(size = 8, color = "#5A5750",
                                     margin = margin(b = 4)),
    plot.margin       = margin(5, 6, 4, 4))

FIG_W_MM <- 210
FIG_H_MM <- 196
FIG_DPI  <- 600
ggsave(file.path(output_dir, "map_SE_tibet_relief.png"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(output_dir, "map_SE_tibet_relief.pdf"), p,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
message("terra_map_2D_SE_tibet.R done -> output/figures/map_SE_tibet_relief.(png|pdf)")
