## 05_geology_map_scan.R — geological map of the Binchuan & Heqing basins built on
## the GEOREFERENCED 1:200,000 published geological sheet (the authoritative source),
## replacing the schematic bedrock of 04_geology_map.R.
##
## SOURCE -----------------------------------------------------------------------
##  Sheet: 1:200,000 large-format regional geological map, 100°00'–101°00'E ×
##         25°20'–26°40'N. Compiled & drawn 1973 by the First Regional Geological
##         Survey Team, Yunnan Geological Bureau (云南省地质局第一区域地质测量队);
##         printed by 国营五四三厂. Scan: data/geology_scan.jpg (6303×6156 px, 330 dpi).
##
## GEOREFERENCING ---------------------------------------------------------------
##  The sheet carries a lat/lon graticule. Its four inner neat-line corners were
##  located in pixel space and tied to their graticule values, giving an axis-
##  aligned affine transform to EPSG:4326 (the graticule is parallel to the pixel
##  axes, so a 4-corner affine is exact to within scan/datum error; the print is
##  geometrically clean — 38.8 vs 38.6 px/km on the two axes). The underlying
##  datum is most likely Beijing 1954; treating the graticule as WGS84 introduces
##  a ~0.1 km systematic shift, acceptable for a bedrock-context figure (sites are
##  plotted from their own GPS coordinates on top).
##
## WHY A SCAN BASE (honesty note) ----------------------------------------------
##  The sheet is coloured by stratigraphic AGE (standard scheme) while a useful
##  raw-material reading needs LITHOLOGY, and several different-age (different-
##  lithology) units share near-identical faded colours, so automatic colour
##  vectorisation cannot recover clean lithological polygons — the printed unit
##  CODES are what disambiguate them. We therefore display the authoritative map
##  itself, georeferenced, rather than a lossy re-colouring. KEY READING: the
##  pale-yellow valley floors are Quaternary basin fill (where the artefacts were
##  collected); the green hill units around the basins are PERMIAN Emeishan basalt
##  (Pβ) — i.e. the nearest mapped volcanic bedrock is Palaeozoic basalt, NOT the
##  Neogene trachyte the old schematic assumed.
## -----------------------------------------------------------------------------

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
sf::sf_use_s2(FALSE)

proj_dir   <- "H:/Quina_valleys"
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- switches --------------------------------------------------------------
show_site_labels <- TRUE
show_geo_key     <- TRUE     # indicative colour key for the dominant units
hillshade_alpha  <- 0        # 0 = none; the sheet already encodes relief. Try 0.12.

## ---- georeference the scan -------------------------------------------------
## inner neat-line corners (image pixels, row 0 = top) -> graticule lon/lat
left_x <- 1176; right_x <- 5059; top_y <- 19; bottom_y <- 5728
lon_W <- 100.0; lon_E <- 101.0; lat_N <- 26 + 40/60; lat_S <- 25 + 20/60

scan <- terra::rast(file.path(proj_dir, "data", "geology_scan.jpg"))
nc <- terra::ncol(scan); nr <- terra::nrow(scan)
lon_at <- function(x) lon_W + (x - left_x)/(right_x - left_x)*(lon_E - lon_W)
lat_at <- function(y) lat_N + (y - top_y)/(bottom_y - top_y)*(lat_S - lat_N)
terra::ext(scan) <- c(lon_at(0), lon_at(nc), lat_at(nr), lat_at(0))
terra::crs(scan) <- "EPSG:4326"
names(scan) <- c("red", "green", "blue")

## ---- study extent = DEM bbox (mirror 01_map.R / 04_geology_map.R) ----------
read_if <- function(f, reader) if (file.exists(f)) reader(f) else NULL
dem <- read_if(file.path(cache_dir, "dem.tif"), terra::rast)
stopifnot(!is.null(dem))
bb  <- terra::ext(dem)
bbx <- as.numeric(as.vector(bb))         # c(xmin, xmax, ymin, ymax)
scan_c <- terra::crop(scan, bb)

## optional subtle hillshade (same recipe as 04) to add a little relief texture
hill <- NULL
if (hillshade_alpha > 0) {
  z_exag <- 1.8; dem_z <- dem * z_exag
  slope_r  <- terra::terrain(dem_z, "slope",  unit = "radians")
  aspect_r <- terra::terrain(dem_z, "aspect", unit = "radians")
  hl <- lapply(c(300, 337, 15),
               function(d) terra::shade(slope_r, aspect_r, angle = 35, direction = d))
  hill <- terra::app(terra::rast(hl), mean)
  hr <- as.numeric(stats::quantile(terra::values(hill, mat = FALSE),
                                   c(0.02, 0.98), na.rm = TRUE))
  hill <- (terra::clamp(hill, hr[1], hr[2]) - hr[1]) / (hr[2] - hr[1])
  names(hill) <- "hillshade"
}

## ---- sites + hulls (mirror 04_geology_map.R exactly) -----------------------
basin_cols <- c(Binchuan = "#8E2F39", Heqing = "#23506E")
label_col  <- "#241F1A"

sites <- readxl::read_excel(file.path(proj_dir, "data", "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>
  mutate(basin = factor(sub(" basin$", "", trimws(basin)),
                        levels = c("Binchuan", "Heqing"))) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
anchor <- subset(sites, code %in% c("LT", "THC"))

hulls <- sites |>
  group_by(basin) |>
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  st_convex_hull() |>
  st_transform(32647) |> st_buffer(900) |> st_transform(4326)

## ---- indicative geology colour key (dominant units in the study area) ------
## Colours sampled from the scan (k-means cluster means); INDICATIVE only —
## the printed unit codes on the sheet are authoritative. Order = young -> old.
geo_key_levels <- c(
  "Quaternary alluvium & terraces",
  "Permian Emeishan basalt (Pβ)",
  "Triassic",
  "Cretaceous–Paleogene red beds",
  "Carboniferous–Permian carbonate",
  "Precambrian metamorphic basement")
geo_key_cols <- c(
  "Quaternary alluvium & terraces"        = "#D8C66E",
  "Permian Emeishan basalt (Pβ)"     = "#8C9A55",
  "Triassic"                              = "#AC8473",
  "Cretaceous–Paleogene red beds"    = "#CFAB8F",
  "Carboniferous–Permian carbonate"  = "#A6AE96",
  "Precambrian metamorphic basement"      = "#7A5E3F")
key_df <- st_sf(
  unit = factor(geo_key_levels, levels = geo_key_levels),
  geometry = st_sfc(lapply(seq_along(geo_key_levels),
                           function(i) st_point(c(lon_W - 1, lat_S - 1))),  # off-canvas
                    crs = 4326))

## ===========================================================================
## BUILD MAP
## ===========================================================================
p <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = scan_c, maxcell = 6e6)

## subtle hillshade on top, if requested
if (!is.null(hill)) {
  p <- p +
    tidyterra::geom_spatraster(data = hill, alpha = hillshade_alpha,
                               show.legend = FALSE) +
    scale_fill_gradient(low = "black", high = "white", na.value = NA) +
    ggnewscale::new_scale_fill()
}

## indicative geology key (off-canvas squares -> legend only)
if (show_geo_key) {
  p <- p +
    geom_sf(data = key_df, aes(fill = unit), shape = 22, size = 0,
            color = NA, stroke = 0) +
    scale_fill_manual(values = geo_key_cols, breaks = geo_key_levels,
                      name = "Geology (indicative;\ncodes on map are authoritative)",
                      drop = FALSE,
                      guide = guide_legend(order = 1,
                              override.aes = list(size = 4.2, alpha = 1, color = "grey35"))) +
    ggnewscale::new_scale_fill()
}

## basin hulls (faint dashed, like 04)
p <- p +
  geom_sf(data = hulls, aes(color = basin), fill = NA, linewidth = 0.6,
          linetype = "22", show.legend = FALSE)

## sites
p <- p +
  geom_sf(data = sites, aes(fill = basin), size = 2.7,
          shape = 21, color = "white", stroke = 0.6, alpha = 0.98) +
  geom_sf(data = anchor, shape = 8, size = 5.0, color = "white", stroke = 1.6) +
  geom_sf(data = anchor, shape = 8, size = 3.9, color = label_col, stroke = 0.9) +
  scale_fill_manual(values = basin_cols, name = "Basin",
                    guide = guide_legend(order = 2,
                            override.aes = list(shape = 21, size = 3))) +
  scale_color_manual(values = basin_cols, guide = "none")

if (show_site_labels)
  p <- p + ggrepel::geom_text_repel(
    data = sites, aes(geometry = geometry, label = code),
    stat = "sf_coordinates", size = 2.4, fontface = "bold", color = label_col,
    bg.color = "white", bg.r = 0.18, max.overlaps = 30,
    min.segment.length = 0, segment.color = "grey30", segment.size = 0.25)

p <- p +
  annotation_scale(location = "bl", width_hint = 0.25,
                   bar_cols = c("grey20", "white"), text_col = "grey15") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm")) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4], expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Geology of the Binchuan and Heqing basins",
       subtitle = "Excerpt of the 1:200,000 geological sheet (Yunnan Geological Bureau, 1973), georeferenced to the study area",
       caption = paste0(
         "Base map: 1:200,000 regional geological sheet (100°–101°E × 25°20'–26°40'N), ",
         "compiled 1973 by the First Regional Geological Survey Team,\nYunnan Geological Bureau; ",
         "georeferenced (EPSG:4326) from the four graticule corners. Stratigraphic colours follow the ",
         "standard scheme — printed unit codes are authoritative.\n",
         "Pale-yellow valley floors = Quaternary basin fill (artefact-bearing terraces); green hill units around the basins = ",
         "Permian Emeishan basalt (Pβ). Stars = LT (Longtan), THC (Tianhua Cave).")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        panel.grid = element_line(color = grey(0.85), linewidth = 0.12),
        legend.title = element_text(size = 8.5),
        legend.text  = element_text(size = 8),
        plot.subtitle = element_text(size = 9, color = "grey25"),
        plot.caption  = element_text(size = 7, hjust = 0))

## ---- export ----------------------------------------------------------------
ggsave(file.path(output_dir, "map_geology_scan.pdf"), p, width = 9.2, height = 8.6)
ggsave(file.path(output_dir, "map_geology_scan.png"), p, width = 9.2, height = 8.6, dpi = 300)
message("05_geology_map_scan.R done -> output/map_geology_scan.(pdf|png)")
