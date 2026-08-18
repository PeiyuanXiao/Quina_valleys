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
# THE POINT OF THIS SCRIPT is that each panel is rendered AT ITS FINAL PLACED
# SIZE. In the draft the map was made at 150 mm and scaled down to ~81 mm in
# Illustrator, so its type ended up at roughly 3.8 pt while the profile -- placed
# near its native width -- kept ~5.7 pt. Rendering at the placed size instead
# means the point sizes below are exactly what appears on the page, and the two
# panels match. Place these files at 100% and never rescale them.
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
# terra_map_3D.R writes straight to output/maps/terrain_3d.png. The locator
# globe is likewise written into this folder by locator_globe_figure.R.

required <- c("ggplot2", "sf", "here")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Please install: ", paste(missing, collapse = ", "))

library(ggplot2)
library(here)

proj_dir <- here()
out_dir  <- file.path(proj_dir, "output", "figures", "fig01_panels")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

FIG_DPI <- 600
## Panel A now carries the shared legend, so it is ~23 mm wider than before; the
## map itself still lands at about the same size as in the draft layout.
MAP_W <- 104; MAP_H <- 100     # mm, panel A (map + legend) as placed
PRF_W <- 146; PRF_H <- 54      # mm, panel C as placed

save_both <- function(p, name, w, h) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h,
         units = "mm", dpi = FIG_DPI, device = ragg::agg_png)
  ggsave(file.path(out_dir, paste0(name, ".pdf")), p, width = w, height = h,
         units = "mm", device = cairo_pdf)
  message(sprintf("  %-26s %3.0f x %3.0f mm", name, w, h))
}

# ---- rebuild the two ggplot panels -----------------------------------------
message("building panels ...")
## these two live in scripts/map/ — the folder was called scripts/figure1/ before
## the restructure, and this script was left pointing at the old path
script_dir <- file.path(proj_dir, "scripts", "map")
env_a <- new.env()
sys.source(file.path(script_dir, "terra_map_2D.R"), envir = env_a)
env_c <- new.env()
sys.source(file.path(script_dir, "traverse_profile_figure.R"), envir = env_c)

# ==============================================================================
# Panel A -- plan map at 81 mm
# ==============================================================================
# At 81 mm the panel is ~69 mm across, so the 0.05-degree longitude labels
# collide and the 2.0-size site codes crowd; both are dialled back here. The
# axis/legend point sizes are NOT changed -- they are already correct once the
# file is placed at 100%.
p_map <- env_a$p +
  scale_x_continuous(breaks = seq(100.4, 100.6, 0.1)) +
  theme(legend.position = "right")

# ggrepel is a layer, so its size has to be replaced rather than themed
idx <- which(vapply(p_map$layers,
                    function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
for (i in idx) {
  p_map$layers[[i]]$aes_params$size <- 1.8
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

save_both(p_map, "panel_A_map", MAP_W, MAP_H)

# ---- same map, with the panel C traverse drawn on it ------------------------
# Panel C is a profile along a route; without the route on the map the reader
# cannot tell where that profile runs. This is the single most useful addition
# to the draft layout.
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

save_both(p_map_route, "panel_A_map_with_route", MAP_W, MAP_H)

# ==============================================================================
# Panel C -- traverse profile at 146 x 54 mm
# ==============================================================================
# The height drops from 78 to 54 mm, so the site labels need less headroom or
# they run into the transect brackets.
p_prof <- env_c$p + theme(legend.position = "none")
idx <- which(vapply(p_prof$layers,
                    function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
for (i in idx) p_prof$layers[[i]]$aes_params$size <- 1.8

save_both(p_prof, "panel_C_profile", PRF_W, PRF_H)

## No separate legend file: the shared legend now rides along with panel A.
message("fig01_export_panels.R done -> ", out_dir)
