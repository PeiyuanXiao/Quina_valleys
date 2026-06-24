## 03_elevation_profile.R — site elevation profile (line chart) along the valleys.
## Standalone: reads only Site_information.xlsx (no network, no cache needed).
## Sites are ordered by latitude within each river transect (a proxy for
## along-valley position); Binchuan splits into its two rivers.

library(dplyr)
library(readxl)
library(ggplot2)

proj_dir   <- "H:/Quina_valleys"
output_dir <- file.path(proj_dir, "outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## Site_information.xlsx = source of truth (29 sites); drop PJDD/ZKZ -> clean 27.
sites <- readxl::read_excel(file.path(proj_dir, "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = c("T2", "T3", "T4", "hilltop")),
    ## river transect: Binchuan = Sangyuan (north ~26.0N) vs Liandong (south ~25.83-25.91N)
    transect = case_when(
      basin == "Heqing"                  ~ "Caifeng (Heqing)",
      basin == "Binchuan" & lat >= 25.93 ~ "Sangyuan (Binchuan, N)",
      TRUE                               ~ "Liandong (Binchuan, S)"
    )
  ) |>
  arrange(basin, transect, lat)

transect_cols <- c(
  "Sangyuan (Binchuan, N)" = "#D55E00",
  "Liandong (Binchuan, S)" = "#E69F00",
  "Caifeng (Heqing)"       = "#0072B2"
)
geo_shapes <- c(T2 = 21, T3 = 22, T4 = 24, hilltop = 23)
anchor     <- subset(sites, code %in% c("LT", "THC"))

p <- ggplot(sites, aes(x = lat, y = elev_m, group = transect, color = transect)) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(aes(shape = geomorph, fill = transect),
             size = 2.6, color = "grey20", stroke = 0.3) +
  ## excavated/dated anchors LT & THC
  geom_point(data = anchor, shape = 8, size = 4, color = "black") +
  ggrepel::geom_text_repel(aes(label = code), size = 2.6, max.overlaps = 30,
                           min.segment.length = 0, color = "grey15",
                           show.legend = FALSE) +
  facet_wrap(~ basin, scales = "free_x") +
  scale_color_manual(values = transect_cols, name = "River transect") +
  scale_fill_manual(values = transect_cols, guide = "none") +
  scale_shape_manual(values = geo_shapes, name = "Geomorphic position",
                     drop = FALSE) +
  labs(x = "Latitude (°N)  —  proxy for along-valley position",
       y = "Elevation (m a.s.l.)",
       title = "Elevation profile of Quina sites",
       caption = paste("Sites ordered by latitude within each river transect.",
                       "Stars = excavated/dated anchors (LT Longtan, THC Tianhua Cave).")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 7, hjust = 0))

ggsave(file.path(output_dir, "elevation_profile.png"), p, width = 9, height = 5,
       dpi = 300)
ggsave(file.path(output_dir, "elevation_profile.pdf"), p, width = 9, height = 5)
message("03_elevation_profile.R done -> outputs/elevation_profile.(png|pdf)")
