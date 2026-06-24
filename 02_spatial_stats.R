## 02_spatial_stats.R — C. Point-pattern description + distance-to-river test.
## Honesty guardrails are implemented throughout; see outputs/spatial_notes.md.
## Requires 00_setup.R (for cached rivers). Sites are read from Site_information.xlsx.

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(spatstat.geom)
library(spatstat.explore)
sf::sf_use_s2(FALSE)
set.seed(123)

proj_dir   <- "H:/Quina_valleys"
output_dir <- file.path(proj_dir, "outputs")
cache_dir  <- file.path(proj_dir, "data_cache")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- sites in UTM 47N (metres); source of truth = Site_information.xlsx ----
## drop PJDD/ZKZ to keep the analysed clean 27.
sites <- readxl::read_excel(file.path(proj_dir, "Site_information.xlsx"))
names(sites) <- trimws(names(sites))
sites <- sites |>
  rename(code = Code) |>
  filter(!code %in% c("PJDD", "ZKZ")) |>
  mutate(
    basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    geomorph = factor(geomorph, levels = c("T2", "T3", "T4", "hilltop"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sites_utm <- st_transform(sites, 32647)

rivers <- if (file.exists(file.path(cache_dir, "rivers.gpkg")))
  st_read(file.path(cache_dir, "rivers.gpkg"), quiet = TRUE) else NULL

bc <- sites_utm[sites_utm$basin == "Binchuan", ]
hq <- sites_utm[sites_utm$basin == "Heqing", ]

## ---- helpers -------------------------------------------------------------
## ppp on the convex hull dilated by 1 km (D2), staying entirely within spatstat
make_ppp <- function(pts_utm, buffer = 1000) {
  xy <- st_coordinates(pts_utm)[, 1:2, drop = FALSE]
  W  <- spatstat.geom::dilation(spatstat.geom::convexhull.xy(xy), buffer)
  spatstat.geom::ppp(xy[, 1], xy[, 2], window = W)
}
## sf study window (convex hull + 1 km) for Monte-Carlo random points (C.5)
basin_window_sf <- function(pts_utm, buffer = 1000) {
  st_buffer(st_convex_hull(st_union(pts_utm)), buffer)
}

## ---- C.2 nearest-neighbour distances (descriptive, core) -----------------
nnd_summary <- function(pts_utm, label) {
  xy <- st_coordinates(pts_utm)[, 1:2, drop = FALSE]
  if (nrow(xy) < 2)
    return(data.frame(group = label, n = nrow(xy), nnd_mean = NA, nnd_median = NA,
                      nnd_min = NA, nnd_max = NA))
  P <- spatstat.geom::ppp(xy[, 1], xy[, 2],
                          window = spatstat.geom::convexhull.xy(xy))
  d <- spatstat.geom::nndist(P)
  data.frame(group = label, n = spatstat.geom::npoints(P),
             nnd_mean = mean(d), nnd_median = median(d),
             nnd_min = min(d), nnd_max = max(d))
}

nnd_tbl <- bind_rows(
  nnd_summary(bc,        "Binchuan"),
  nnd_summary(hq,        "Heqing"),
  nnd_summary(sites_utm, "All")
)

nnd_points <- function(pts_utm, label) {
  xy <- st_coordinates(pts_utm)[, 1:2, drop = FALSE]
  P  <- spatstat.geom::ppp(xy[, 1], xy[, 2],
                           window = spatstat.geom::convexhull.xy(xy))
  data.frame(group = label, nnd = as.numeric(spatstat.geom::nndist(P)))
}
nnd_df <- bind_rows(nnd_points(bc, "Binchuan"), nnd_points(hq, "Heqing"))
p_nnd <- ggplot(nnd_df, aes(nnd, fill = group)) +
  geom_histogram(bins = 12, color = "white") +
  facet_wrap(~ group, scales = "free") +
  scale_fill_manual(values = c(Binchuan = "#D55E00", Heqing = "#0072B2")) +
  labs(x = "Nearest-neighbour distance (m)", y = "Count",
       title = "Nearest-neighbour distances by basin",
       caption = "Heqing n = 5: descriptive only.") +
  theme_bw() + theme(legend.position = "none")
ggsave(file.path(output_dir, "nnd_histogram.png"), p_nnd, width = 8, height = 4,
       dpi = 300)

## ---- C.3 Clark-Evans (Binchuan only; exploratory + caveat) ---------------
ce_R <- NA_real_; ce_p <- NA_real_
ce <- tryCatch(
  spatstat.explore::clarkevans.test(make_ppp(bc), correction = "Donnelly",
                                    alternative = "two.sided", nsim = 999),
  error = function(e) { message("Clark-Evans failed: ", conditionMessage(e)); NULL })
if (!is.null(ce)) { ce_R <- unname(ce$statistic); ce_p <- ce$p.value }

## ---- C.4 Ripley's L + CSR envelope (Binchuan only; exploratory) ----------
env <- tryCatch(
  spatstat.explore::envelope(make_ppp(bc), spatstat.explore::Lest,
                             nsim = 199, correction = "border", verbose = FALSE),
  error = function(e) { message("Envelope failed: ", conditionMessage(e)); NULL })
if (!is.null(env)) {
  png(file.path(output_dir, "ripley_L_binchuan.png"),
      width = 1600, height = 1200, res = 200)
  plot(env, main = "Ripley's L — Binchuan (EXPLORATORY: CSR is not a valid null here)")
  dev.off()
}

## ---- C.5 distance to river: observed vs random (core, defensible) --------
dist_stats <- NULL
if (!is.null(rivers)) {
  rivers_utm <- st_union(st_transform(rivers, 32647))
  sites_utm$dist_river <- as.numeric(st_distance(sites_utm, rivers_utm))

  ds_list <- list(); dd_list <- list()
  for (b in c("Binchuan", "Heqing")) {
    pts    <- sites_utm[sites_utm$basin == b, ]
    win    <- basin_window_sf(pts)
    rand   <- st_sample(win, size = 2000, type = "random")
    rand_d <- as.numeric(st_distance(rand, rivers_utm))
    obs_d  <- pts$dist_river
    wt     <- suppressWarnings(wilcox.test(obs_d, rand_d, alternative = "less"))
    ds_list[[b]] <- data.frame(
      group = b, n = length(obs_d),
      dist_obs_median_m  = median(obs_d),
      dist_rand_median_m = median(rand_d),
      dist_wilcox_p_less = wt$p.value)
    dd_list[[b]] <- bind_rows(
      data.frame(group = b, type = "Observed sites",    dist = obs_d),
      data.frame(group = b, type = "Random in window",  dist = rand_d))
  }
  dist_stats <- bind_rows(ds_list)
  dist_df    <- bind_rows(dd_list)

  p_distr <- ggplot(dist_df, aes(dist, fill = type)) +
    geom_density(alpha = 0.5, color = NA) +
    facet_wrap(~ group, scales = "free") +
    scale_fill_manual(values = c("Observed sites" = "#D55E00",
                                 "Random in window" = "grey60")) +
    labs(x = "Distance to nearest river (m)", y = "Density", fill = NULL,
         title = "Site distance-to-river vs random expectation",
         caption = paste("One-sided Wilcoxon (sites < random).",
                         "Heqing n = 5: interpret as descriptive.")) +
    theme_bw()
  ggsave(file.path(output_dir, "dist_to_river.png"), p_distr, width = 8, height = 4,
         dpi = 300)
} else {
  message("No rivers cached -> C.5 (distance to river) skipped. Provide rivers in 00_setup.R.")
}

## ---- assemble spatial_stats.csv ------------------------------------------
spatial_stats <- nnd_tbl |>
  mutate(
    clark_evans_R = ifelse(group == "Binchuan", round(ce_R, 3), NA_real_),
    clark_evans_p = ifelse(group == "Binchuan", signif(ce_p, 3), NA_real_),
    clark_evans_note = case_when(
      group == "Binchuan" ~ "exploratory: 2-D CSR invalid (linear along-river)",
      group == "Heqing"   ~ "n=5, not tested",
      TRUE                ~ "pooled (basins not comparable)"
    )
  )
if (!is.null(dist_stats)) {
  spatial_stats <- left_join(spatial_stats, dist_stats, by = c("group", "n"))
}
write.csv(spatial_stats, file.path(output_dir, "spatial_stats.csv"), row.names = FALSE)

## ---- geomorph x basin contingency (descriptive only; guardrail #3) -------
geo_tab <- as.data.frame.matrix(table(sites$basin, sites$geomorph))
geo_tab <- cbind(basin = rownames(geo_tab), geo_tab)
write.csv(geo_tab, file.path(output_dir, "geomorph_basin_table.csv"),
          row.names = FALSE)

## ---- honest interpretation notes -----------------------------------------
notes <- c(
  "# Spatial analysis — honest interpretation notes",
  "",
  "## Sample sizes and what is testable",
  "- **Heqing** (n = 5): no point-pattern test is statistically meaningful; reported as **description only** ('n=5, not tested').",
  "- **Binchuan** (n = 22): sites lie in a **near-linear arrangement along the valley/rivers**.",
  "",
  "## Why 2-D CSR is the wrong null model (read before citing C.3/C.4)",
  "- Clark-Evans (C.3) and Ripley's L (C.4) test against 2-D Complete Spatial Randomness.",
  "- A linear, along-river distribution reads as 'significantly clustered' under CSR **purely because of the linear geometry**, not because of meaningful clustering.",
  "- These two analyses are therefore **EXPLORATORY ONLY** and must not be cited as evidence of clustering.",
  "",
  "## The defensible core spatial test: distance to river (C.5)",
  "- Tests whether observed sites sit closer to rivers than random points in the same study window (one-sided Wilcoxon).",
  "- This addresses a meaningful hypothesis (water-/raw-material-oriented placement) and replaces the uninformative 2-D CSR test.",
  "- Reported per basin; compare against the thesis §3 median site-river distance (604 m).",
  "",
  "## Other guardrails enforced",
  "- **Lithic counts are NOT compared across basins.** Heqing includes excavated sites (LT, THC); higher counts reflect collection method (excavation vs surface survey), not occupation intensity. On the map, n_lithics is a visual size only; no cross-basin count test is run.",
  "- **Geomorph x basin is descriptive only** (geomorph_basin_table.csv). Structural zeros (hilltop/T4 only Binchuan; T2 only Heqing) make any chi-square/Fisher test trivially significant but uninformative — no test performed.",
  "- **Height-above-river (h_river_m) is descriptive only.** If terraces were defined by height, 'T4 higher than T3' is circular; h_river is used only to separate near-river terraces from uplifted hilltops.",
  "",
  "## Data note",
  "- Sites are read directly from Site_information.xlsx (29 sites); PJDD and ZKZ are excluded to give the analysed clean 27.",
  "- geomorph in Site_information.xlsx had been corrupted (mojibake) and has been repaired in-file to the canonical T2/T3/T4/hilltop tokens; the basin x geomorph table is internally consistent with guardrail #3 (hilltop/T4 only Binchuan; T2 only Heqing)."
)
writeLines(notes, file.path(output_dir, "spatial_notes.md"))

cat("\n== NND summary ==\n"); print(nnd_tbl)
cat("\nClark-Evans (Binchuan): R =", round(ce_R, 3), " p =", signif(ce_p, 3), "\n")
if (!is.null(dist_stats)) { cat("\n== Distance to river ==\n"); print(dist_stats) }
message("\n02_spatial_stats.R done -> outputs/{spatial_stats.csv, nnd_histogram.png, ",
        "ripley_L_binchuan.png, dist_to_river.png, geomorph_basin_table.csv, spatial_notes.md}")
