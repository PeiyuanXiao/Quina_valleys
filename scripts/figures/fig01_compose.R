# fig01_compose.R
# Assembles Figure 1 from the three panels:
#   A  plan site map            (scripts/maps/01_map.R)
#   B  3-D block model          (scripts/maps/08_terrain_3d.R, rendered PNG)
#   C  traverse long profile    (scripts/figures/traverse_profile_figure.R)
#
# A and C are rebuilt as ggplot objects by sourcing their scripts into private
# environments and taking `p` (each script also re-saves its own standalone
# output, which is harmless). B is a path-traced raster, so it comes in as an
# image. Legends are collected once into a guide area under B: A and C use the
# identical basin/geomorph scales, so patchwork merges them.
#
# Output: output/figures/fig01_composite.png (+ .pdf)

required <- c("patchwork", "magick", "ggplot2", "here")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Please install: ", paste(missing, collapse = ", "))

library(ggplot2)
library(patchwork)
library(here)

proj_dir <- here()
fig_dir  <- file.path(proj_dir, "output", "figures")
map_png  <- file.path(proj_dir, "output", "maps", "terrain_3d.png")
stopifnot(file.exists(map_png))

FIG_W_MM <- 180
FIG_H_MM <- 165
FIG_DPI  <- 600

# ---- rebuild panels A and C -------------------------------------------------
message("building panel A (plan map) ...")
env_a <- new.env()
sys.source(file.path(proj_dir, "scripts", "maps", "01_map.R"), envir = env_a)
## the map is ~half its standalone width here, so its 0.05-degree longitude
## breaks collide; thin them to 0.1 for the composite
p_map <- env_a$p + scale_x_continuous(breaks = seq(100.4, 100.6, 0.1))

message("building panel C (traverse profile) ...")
env_c <- new.env()
sys.source(file.path(proj_dir, "scripts", "figures", "traverse_profile_figure.R"),
           envir = env_c)
## C's basin/geomorph guides are dropped rather than collected: the map draws
## its fill scale after ggnewscale::new_scale_fill(), so internally it is
## `fill_new` and patchwork will not merge it with C's plain `fill` -- collecting
## both yields two identical Basin legends side by side. A's copy is the one
## kept, so the shared legend still describes both panels.
p_prof <- env_c$p + guides(fill = "none", shape = "none")

# ---- panel B: the path-traced block model as an image -----------------------
p_3d <- magick::image_read(map_png) |>
  magick::image_trim() |>
  magick::image_ggplot(interpolate = TRUE) +
  theme(plot.margin = margin(0, 0, 0, 0))

# ---- assemble ---------------------------------------------------------------
## No guide_area(): parking the legend in the right column made that column
## taller than the map and left a dead band across the middle. Collected to the
## right of the whole figure instead, it forms a strip beside both rows.
top <- p_map + p_3d + plot_layout(widths = c(1.15, 1))
fig <- top / p_prof +
  plot_layout(heights = c(1.55, 1), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "right",
        plot.tag = element_text(size = 11, face = "bold", colour = "#202124"))

ggsave(file.path(fig_dir, "fig01_composite.png"), fig,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", dpi = FIG_DPI,
       device = ragg::agg_png)
ggsave(file.path(fig_dir, "fig01_composite.pdf"), fig,
       width = FIG_W_MM, height = FIG_H_MM, units = "mm", device = cairo_pdf)
message("fig01_compose.R done -> output/figures/fig01_composite.(png|pdf)")
