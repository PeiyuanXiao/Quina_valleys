# locator_globe_figure.R
# Locator inset for Figure 1A: an orthographic globe centred on the study area,
# in the Wikipedia idiom -- but WITHOUT national boundaries. Standalone: it needs
# neither setup.R nor data/cache/, only rnaturalearth's coastlines, and it writes
# straight into the panel folder for the manual layout.
#
# WHY NO BOUNDARIES
# Natural Earth's outlines do not match the official Chinese depiction (South
# Tibet falls outside its China polygon; Taiwan, Hong Kong and Macao are
# separate `admin` entries; there is no South China Sea line), and the official
# standard maps are issued as finished artwork with a review number rather than
# as GIS layers, so they cannot simply be substituted here. Drawing no political
# boundaries at all sidesteps the question: the inset only has to say WHERE the
# study area is, which a coastline and a marker do on their own. The country
# polygons are therefore dissolved into a single landmass and only the coastline
# is drawn.
#
# The orthographic projection (+proj=ortho) renders the Earth as a disc showing
# one hemisphere. The awkward part is the horizon: polygons that cross it
# produce garbage if transformed naively, so the world is first clipped on the
# SPHERE (s2) with a spherical cap centred on the view point, and only the
# surviving geometry is projected.
#
# Input:  rnaturalearth (coastlines only)
# Output: output/figures/fig01_panels/locator_globe.png (+ .pdf)

required <- c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "here")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Please install: ", paste(missing, collapse = ", "))

library(sf)
library(ggplot2)
library(here)

proj_dir <- here()
out_dir  <- file.path(proj_dir, "output", "figures", "fig01_panels")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

FIG_MM  <- 38          # square inset, mm
FIG_DPI <- 600

## ---- view point + styling ---------------------------------------------------
lon0 <- 100.47; lat0 <- 25.94        # view centre (the study area)
show_graticule <- TRUE
## No study-area marker is drawn: it is added by hand in the layout.

## Land and ocean have to differ in LIGHTNESS, not just hue. The first version
## paired #DCD6C8 land with #C3D6E2 ocean -- almost the same lightness, so they
## separated only by hue and merged wherever the sphere lighting brightened the
## centre. Each preset below keeps a clear lightness gap.
palette_name <- "classic"            # "classic" | "slate" | "deep"
palettes <- list(
  # light warm land on mid blue ocean; ocean sits in the plan map's water family
  classic = list(ocean = "#96B6C9", land = "#E4DECF", coast = "#A29884",
                 halo  = "#8FB3C9"),
  # neutral: no hue competing with the red site symbols elsewhere in the figure
  slate   = list(ocean = "#8C9EAB", land = "#EDEAE3", coast = "#948C7E",
                 halo  = "#8C9EAB"),
  # strongest separation, for very small placements
  deep    = list(ocean = "#6E97AF", land = "#F0E9D9", coast = "#8F8672",
                 halo  = "#7FA6BC")
)
pal <- palettes[[palette_name]]

## Shading. Both effects are built from STACKED TRANSLUCENT CIRCLES rather than
## a raster: they stay vector in the PDF, and they need no differencing because
## overlapping alpha accumulates on its own.
##   sphere_light  centre brightening -> the disc reads as a lit ball, not a
##                 flat circle (limb darkening by relative contrast)
##   halo          an atmosphere glow drawn UNDER the globe, so only the part
##                 outside the limb shows
sphere_light <- 0.24    # 0 = flat disc; too high and it washes out the contrast
halo_alpha   <- 0.30    # 0 = no atmosphere
n_halo       <- 44      # opaque rings: as many as you like
n_light      <- 18      # alpha layers: keep low, see the note below
circle_seg   <- 120     # nQuadSegs: keeps the limb from looking faceted

ocean_col  <- pal$ocean
land_col   <- pal$land
coast_col  <- pal$coast
halo_col   <- pal$halo
limb_col   <- "#5E5849"
grat_col   <- "#FFFFFF"
light_col  <- "#FFFFFF"

R <- 6371000
ortho <- sprintf(paste0("+proj=ortho +lat_0=%s +lon_0=%s +x_0=0 +y_0=0 ",
                        "+a=%s +b=%s +units=m +no_defs"), lat0, lon0, R, R)

## ---- visible hemisphere, cut on the sphere ---------------------------------
sf_use_s2(TRUE)
centre <- st_sfc(st_point(c(lon0, lat0)), crs = 4326)
## a spherical cap just short of 90 deg: at exactly 90 the limb is degenerate
cap <- st_buffer(centre, dist = R * (89.5 * pi / 180))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_make_valid()
world_vis <- suppressWarnings(st_intersection(world, cap))
## dissolve the countries: this is what removes every political boundary and
## leaves a single landmass whose only edge is the coast
land <- suppressWarnings(st_union(st_geometry(world_vis))) |> st_make_valid()

grat <- NULL
if (show_graticule) {
  ## built at 1-degree steps, so no st_segmentize() (and no lwgeom) needed
  mer <- lapply(seq(-180, 150, by = 30), function(L)
    st_linestring(cbind(L, seq(-89, 89, by = 1))))
  par <- lapply(seq(-60, 60, by = 30), function(B)
    st_linestring(cbind(seq(-180, 180, by = 1), B)))
  grat <- suppressWarnings(st_intersection(st_sfc(c(mer, par), crs = 4326), cap))
}

sf_use_s2(FALSE)

## ---- project ---------------------------------------------------------------
to_ortho <- function(x) if (is.null(x) || length(x) == 0) NULL else
  suppressWarnings(st_transform(x, ortho))
land_o <- to_ortho(land)
grat_o <- to_ortho(grat)

## the globe itself: a disc of Earth radius in projected metres
origin <- st_sfc(st_point(c(0, 0)), crs = ortho)
globe  <- st_buffer(origin, R, nQuadSegs = circle_seg)

## graded stack of concentric circles; alpha accumulates toward whichever end
## the radii converge on
## WATCH THE ALPHA. Stacking many circles at alpha = total/n fails once the
## per-layer alpha drops below the 8-bit quantisation step: the per-channel
## blend differences round inconsistently (blue up, red/green down for a blue
## fill) and the bias ACCUMULATES, turning a blue-grey glow saturated violet.
## So: the halo, whose background is the known white page, is built from OPAQUE
## rings of pre-blended colour -- no alpha at all. The sphere light sits over
## varying content so it does need alpha, but with few enough layers that each
## one is comfortably above the quantisation floor.
blend_white <- function(col, a) {
  c0 <- grDevices::col2rgb(col)[, 1] / 255
  grDevices::rgb(c0[1] * a + (1 - a), c0[2] * a + (1 - a), c0[3] * a + (1 - a))
}

# atmosphere: opaque rings, largest first, so each is overpainted by the next
halo_r <- seq(R * 1.075, R, length.out = n_halo)
halo_a <- seq(0, halo_alpha, length.out = n_halo)
halo_layers <- lapply(seq_along(halo_r), function(i)
  geom_sf(data = st_buffer(origin, halo_r[i], nQuadSegs = circle_seg),
          fill = blend_white(halo_col, halo_a[i]), colour = NA))

# sphere: radii shrink to the centre, drawn over the globe -> centre brightens
light_a <- 1 - (1 - sphere_light)^(1 / n_light)
light_layers <- lapply(seq(R, R * 0.18, length.out = n_light), function(r)
  geom_sf(data = st_buffer(origin, r, nQuadSegs = circle_seg),
          fill = light_col, colour = NA, alpha = light_a))

cat("landmass dissolved from", nrow(world_vis),
    "country polygons | no political boundaries drawn\n")

## ---- draw ------------------------------------------------------------------
p <- ggplot()
if (halo_alpha > 0) for (l in halo_layers) p <- p + l      # under the globe
p <- p + geom_sf(data = globe, fill = ocean_col, colour = NA)

if (show_graticule && !is.null(grat_o))
  p <- p + geom_sf(data = grat_o, colour = grat_col, linewidth = 0.15,
                   alpha = 0.7)

p <- p +
  geom_sf(data = land_o, fill = land_col, colour = coast_col, linewidth = 0.08)

# sphere shading over ocean AND land, but under the marker
if (sphere_light > 0) for (l in light_layers) p <- p + l

p <- p +
  # limb last, so it sits over every edge
  geom_sf(data = globe, fill = NA, colour = limb_col, linewidth = 0.25) +
  coord_sf(crs = ortho, expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(out_dir, "locator_globe.png"), p,
       width = FIG_MM, height = FIG_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(out_dir, "locator_globe.pdf"), p,
       width = FIG_MM, height = FIG_MM, units = "mm", device = cairo_pdf)
message("locator_globe_figure.R done -> ", out_dir, "/locator_globe.(png|pdf)")
