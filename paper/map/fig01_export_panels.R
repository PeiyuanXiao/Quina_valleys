# fig01_export_panels.R
# Exports the Figure 1 panels as separate files sized for MANUAL layout in
# Illustrator, matching the draft composition. This is the only route to
# Figure 1: figures/study_area.png, the file the manuscript includes, is
# assembled by hand from what this script and locator_globe_figure.R write.
#
# The draft composition:
#
#   +----------------------+---------------------+
#   |  B  3-D block        |  A  plan map        |
#   |                      |                     |
#   +----------------------+---------+-----------+
#   |  C  traverse profile           |  legend   |
#   +--------------------------------+-----------+
#
# The point of the script is that each panel is rendered AT ITS FINAL PLACED
# SIZE, so the point sizes below are what appears on the page and the panels
# match: a map drawn at 150 mm and scaled to 81 mm in Illustrator ends up with
# type at ~3.8 pt beside the profile's ~5.7 pt. Place these files at 100% and
# never rescale them.
#
# Sizes assume a 180 mm (double-column) figure. If your final width differs,
# scale TARGET_* proportionally and re-run rather than resizing in Illustrator.
#
# Output (output/figures/fig01_panels/), PNG + PDF for each:
#   panel_A_map                104 x 100 mm — the map at 81 mm plus the shared
#                              legend, which rides along with this panel rather
#                              than being exported on its own
#   panel_A_map_with_route     same, with the panel C traverse drawn on the map
#   panel_C_profile            146 x  54 mm, no legend
#
# Panel B does not come through here: it is a path-traced raster that
# terra_map_3D_hyps.R writes straight to output/maps/. The locator globe is
# likewise written into this folder by locator_globe_figure.R.

required <- c("ggplot2", "sf", "here")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Please install: ", paste(missing, collapse = ", "))

library(ggplot2)
library(here)

proj_dir <- here()
out_dir  <- file.path(proj_dir, "output", "figures", "fig01_panels")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

FIG_DPI <- 600
## Panel A carries the shared legend, which is why it is wider than the map.
MAP_W <- 104; MAP_H <- 100     # mm, panel A (map + legend) as placed
## mm, panel C as placed: the layout fixes the width, the height follows from
## PRF_ASPECT.
PRF_ASPECT <- 3.0
PRF_W <- 146; PRF_H <- PRF_W / PRF_ASPECT

save_both <- function(p, name, w, h) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h,
         units = "mm", dpi = FIG_DPI, device = ragg::agg_png)
  ggsave(file.path(out_dir, paste0(name, ".pdf")), p, width = w, height = h,
         units = "mm", device = cairo_pdf)
  message(sprintf("  %-26s %3.0f x %3.0f mm", name, w, h))
}

# ---- rebuild the two ggplot panels -----------------------------------------
message("building panels ...")
script_dir <- file.path(proj_dir, "paper", "map")
## An argument switches the plan map to the landscape ramp and suffixes every
## file this script writes, so the originals are never overwritten.
args <- commandArgs(trailingOnly = TRUE)
pal  <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "sheet"
sfx  <- if (identical(pal, "sheet")) "" else paste0("_", pal)

## arg 2 "north": pull the frame's north edge to 26.25 so the Jinsha is in it.
## The east-west span widens with it so the PANEL ASPECT IS UNCHANGED — that is
## what keeps this file placeable in the same 104 x 100 mm slot as before. The
## numbers come from solving (1/cos(mid_lat)) * dlat/dlon for the aspect the
## current frame already has (1.33123), about the same centre longitude.
north <- length(args) >= 2 && identical(args[2], "north")
if (north) sfx <- paste0(sfx, "_north")

## arg 3: a locality code to pick out - yellow ring, red label
hl <- if (length(args) >= 3 && nzchar(args[3])) args[3] else NA_character_

## arg 4 "bare": strip graticule, axis text, ticks and legend. The panel then has
## almost no fixed furniture left, so the export is re-sized to the panel
## itself rather than to the 104 x 100 mm slot, which would letterbox it with
## the width the legend occupies.
bare <- length(args) >= 4 && identical(args[4], "bare")
if (bare) sfx <- paste0(sfx, "_bare")

env_a <- new.env()
assign("PALETTE_MODE", pal, envir = env_a)
## The west edge is pulled in by hand, so this frame does not preserve the
## panel aspect the way the "north" frame does: 0.3597 deg wide against 0.5043
## tall, a 1.560 panel. The bare export below solves its height from that.
if (north) assign("MAP_EXT", c(xmin = 100.325000, xmax = 100.684685,
                               ymin = 25.745700,  ymax = 26.250000), envir = env_a)
if (!is.na(hl)) assign("HIGHLIGHT_CODE", hl, envir = env_a)
if (bare) {
  assign("SHOW_GRID",   FALSE, envir = env_a)
  assign("SHOW_LEGEND", FALSE, envir = env_a)
  ## with no legend or axis text the panel takes the whole file, so the symbols
  ## and codes are set larger than the values tuned for the 81 mm placement
  assign("SITE_SIZE",   3.4,   envir = env_a)
  assign("SITE_STROKE", 0.60,  envir = env_a)
  assign("LABEL_SIZE",  3.0,   envir = env_a)
  assign("BORDER_WIDTH", 0.8,  envir = env_a)
}
sys.source(file.path(script_dir, "terra_map_2D.R"), envir = env_a)
env_c <- new.env()
sys.source(file.path(script_dir, "traverse_profile_figure.R"), envir = env_c)

# ---- Panel A: plan map at 81 mm ---------------------------------------------
# At 81 mm the panel is ~69 mm across, so the 0.05-degree longitude labels
# collide and the 2.0-size site codes crowd; both are dialled back here. The
# axis/legend point sizes are NOT changed -- they are already correct once the
# file is placed at 100%.
p_map <- env_a$p
if (!bare) p_map <- p_map +
  ## a tenth of a degree, over whatever frame is in force
  scale_x_continuous(breaks = seq(ceiling(env_a$bbx[1] * 10) / 10,
                                  env_a$bbx[2], by = 0.1)) +
  theme(legend.position = "right")

# ggrepel is a layer, so its size has to be replaced rather than themed
## In bare mode the size is carried as a mapped aesthetic (scale_size_identity),
## and writing aes_params$size here would override that mapping and flatten the
## highlighted code back to the common size. Only the small-placement path needs
## this shrink at all.
idx <- which(vapply(p_map$layers,
                    function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
for (i in idx) {
  if (!bare) p_map$layers[[i]]$aes_params$size <- 1.8
  p_map$layers[[i]]$geom_params$box.padding <- unit(0.20, "lines")
}

# The scale bar and north arrow are sized in absolute cm, so at 81 mm instead of
# 150 mm they take up twice the relative area and swamp the panel. Drop the
# ggspatial layers and re-add them at roughly half their linear dimensions, with
# the text brought to the same ~6.5 pt as everything else on the page.
is_furniture <- vapply(p_map$layers, function(l)
  inherits(l$geom, "GeomScaleBar") || inherits(l$geom, "GeomNorthArrow"),
  logical(1))
p_map$layers <- p_map$layers[!is_furniture]

north_small <- grid::gTree(children = grid::gList(
  grid::linesGrob(
    x = grid::unit(c(0.5, 0.5), "npc"), y = grid::unit(c(0, 0.66), "npc"),
    arrow = grid::arrow(type = "closed", angle = 20, length = grid::unit(1.8, "mm")),
    gp = grid::gpar(col = "#332F29", fill = "#332F29", lwd = 1.1, lineend = "butt")),
  grid::textGrob("N", x = 0.5, y = 0.90,
                 gp = grid::gpar(col = "#332F29", fontsize = 6.5, fontface = "bold"))))

p_map <- p_map +
  ggspatial::annotation_scale(
    location = "bl", style = "ticks", width_hint = 0.33,
    height = unit(0.14, "cm"), line_width = 0.8, tick_height = 0.8,
    text_cex = 0.52, text_col = "#332F29", line_col = "#332F29",
    text_face = "bold", text_family = "",
    pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.66, "cm"), width = unit(0.38, "cm"),
    pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
    style = north_small)

if (bare) {
  ## everything that is not the panel is now just the plot margins, so the panel
  ## aspect fixes the file: solve the height from the width that is left over.
  g  <- ggplot2::ggplotGrob(p_map)
  li <- g$layout[g$layout$name == "panel", ][1, ]
  fw <- sum(grid::convertWidth(g$widths,   "mm", valueOnly = TRUE)[-li$l])
  fh <- sum(grid::convertHeight(g$heights, "mm", valueOnly = TRUE)[-li$t])
  ar <- (1 / cos(mean(env_a$bbx[3:4]) * pi / 180)) *
        diff(env_a$bbx[3:4]) / diff(env_a$bbx[1:2])     # panel height / width
  MAP_H <- (MAP_W - fw) * ar + fh
  message(sprintf("bare export: %.0f x %.1f mm (panel aspect %.4f)", MAP_W, MAP_H, ar))
}
save_both(p_map, paste0("panel_A_map", sfx), MAP_W, MAP_H)

# ---- same map, with the panel C traverse drawn on it ------------------------
# Panel C is a profile along a route; without the route on the map the reader
# cannot tell where that profile runs.
route <- env_c$route_sites
## as an sf data frame, not a bare sfc: a spliced-in layer (below) never goes
## through ggplot_add(), and layer_data() chokes on a raw sfc
route_line <- sf::st_sf(id = 1L,
                        geometry = sf::st_sfc(
                          sf::st_linestring(sf::st_coordinates(route)),
                          crs = 32647)) |> sf::st_transform(4326)

route_layer <- geom_sf(data = route_line, colour = "white", linewidth = 0.5,
                       linetype = "22", alpha = 0.95, inherit.aes = FALSE)

## Appending the route with `+` puts it on top of everything, and the dashes
## then slice through every site symbol. It has to go UNDER the markers -- but a
## layer spliced straight into $layers never passes through ggplot_add() and
## fails later in layer_data(). So: lift the point layers out, add the route
## normally with `+`, then put those already-initialised layers back on top.
is_point_layer <- vapply(p_map$layers, function(l) {
  d <- l$data
  inherits(d, "sf") &&
    any(as.character(sf::st_geometry_type(d)) %in% c("POINT", "MULTIPOINT"))
}, logical(1))
site_layers <- p_map$layers[is_point_layer]

p_base <- p_map
p_base$layers <- p_map$layers[!is_point_layer]

p_map_route <- p_base + route_layer +
  coord_sf(xlim = env_a$bbx[1:2], ylim = env_a$bbx[3:4], expand = FALSE)
p_map_route$layers <- c(p_map_route$layers, site_layers)

save_both(p_map_route, paste0("panel_A_map_with_route", sfx), MAP_W, MAP_H)

# ---- Panel C: traverse profile ----------------------------------------------
# The height drops from 78 to 54 mm, so the site labels need less headroom or
# they run into the transect brackets.
p_prof <- env_c$p + theme(legend.position = "none")
idx <- which(vapply(p_prof$layers,
                    function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
for (i in idx) p_prof$layers[[i]]$aes_params$size <- 1.8

save_both(p_prof, paste0("panel_C_profile", sfx), PRF_W, PRF_H)

## No separate legend file: the shared legend now rides along with panel A.
message("fig01_export_panels.R done -> ", out_dir)
