# QV_raw_material_permanova.R
# Variation in raw-material composition explained by basin vs river_ID.
#
# PERMANOVA (adonis2) + PERMDISP on two datasets, both Bray-Curtis:
#   A. Used tools (Quina scrapers, artifact level).
#   C. Available cobbles (cobble level, n = 469) -- spatial heterogeneity.
# river_ID nests in basin, so one-way river R2 >= basin R2 by construction; the
# nested model D ~ basin/river_ID separates between-basin vs river-within-basin.
# Dataset A is ~96% Trachyte, so a tiny R2 is the finding (uniform selection).
# Sandstone = "Quartz sandstone" + "Coarse sandstone" (matches sibling scripts).
#
# Pipeline:
#   1. Dataset A -- used tools: one-way + nested PERMANOVA + composition figures.
#   2. Available cobbles: load + composition figures (descriptive).
#   3. Dataset C -- available cobbles (cobble level): D ~ Loc heterogeneity + viz.
#   4. Combined summary of variance explained.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#   - data/Site_information.xlsx
#   - data/Raw_mat_basin.xlsx
#
# Output:
#   - output/raw_material_analysis/raw_material_permanova/*.png

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0)
  stop("Please install R packages before running: ",
       paste(missing_packages, collapse = ", "))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(here)
library(grid)

set.seed(2226)

# ==============================================================================
# Global parameters
# ==============================================================================

proj_dir  <- here()
sc_path   <- file.path(proj_dir, "data", "Quina_scraper_surface.xlsx")
site_path <- file.path(proj_dir, "data", "Site_information.xlsx")
basin_path<- file.path(proj_dir, "data", "Raw_mat_basin.xlsx")
out_dir   <- file.path(proj_dir, "output", "raw_material_analysis", "raw_material_permanova")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

perm       <- 9999

# ---- shared levels / palettes (project idiom) ------------------------------
material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone", "Andesite")
material_colors <- c(Trachyte = "#D55E00", Sandstone = "#E69F00",
                     Quartz = "#56B4E9", Mudstone = "#0072B2", Andesite = "#009E73")
basin_levels <- c("Binchuan", "Heqing")
basin_colors <- c(Binchuan = "#C9603F", Heqing = "#3F7CAC")
river_levels <- c("Sangyuan", "Liandong", "Caifeng")
river_colors <- c(Sangyuan = "#D55E00", Liandong = "#E69F00", Caifeng = "#0072B2")

base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649", margin = margin(b = 8)),
    axis.title = element_text(size = 12), axis.text = element_text(color = "#303238"),
    legend.title = element_text(face = "bold"), legend.key = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white"))

# ==============================================================================
# Helpers (tests, ordination, compositional)
# ==============================================================================

fmt_p <- function(p) ifelse(is.na(p), "NA",
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3))))

harmonise_lithology <- function(x) {
  x <- trimws(as.character(x))
  recode(x, "Quartz sandstone" = "Sandstone", "Coarse sandstone" = "Sandstone")
}
strip_basin <- function(x) sub(" basin$", "", trimws(as.character(x)))

# one-way PERMANOVA + PERMDISP for a single grouping factor
run_oneway <- function(D, meta, fac, prefix, label) {
  form <- as.formula(paste("D ~", fac))
  ad <- adonis2(form, data = meta, permutations = perm)
  disp_p <- NA_real_
  tryCatch({
    b  <- betadisper(D, meta[[fac]])
    pt <- permutest(b, permutations = perm)
    disp_p <- pt$tab$`Pr(>F)`[1]
  }, error = function(e) message("  PERMDISP skipped for ", fac, ": ", conditionMessage(e)))
  cat(sprintf("\n[%s]  %s ~ %-9s :  R2 = %.3f | F = %.2f | p %s | PERMDISP p %s\n",
              prefix, label, fac, ad$R2[1], ad$F[1], fmt_p(ad$`Pr(>F)`[1]), fmt_p(disp_p)))
  print(ad)
  data.frame(dataset = prefix, model = paste0("~ ", fac), term = fac,
             R2 = ad$R2[1], F = ad$F[1], p = ad$`Pr(>F)`[1], permdisp_p = disp_p,
             stringsAsFactors = FALSE)
}

# nested model D ~ basin/river_ID : separates between-basin vs river-within-basin
run_nested <- function(D, meta, prefix, label) {
  ad <- adonis2(D ~ basin / river_ID, data = meta, permutations = perm, by = "terms")
  cat(sprintf("\n[%s]  %s ~ basin/river_ID  (sequential variance partition):\n", prefix, label))
  print(ad)
  rn <- rownames(ad)
  pick <- function(k) { i <- which(rn == k); if (length(i)) i else NA_integer_ }
  ib <- pick("basin"); ir <- pick("basin:river_ID")
  data.frame(
    dataset = prefix, model = "~ basin/river_ID",
    term = c("basin", "river_within_basin"),
    R2 = c(ad$R2[ib], ad$R2[ir]), F = c(ad$F[ib], ad$F[ir]),
    p = c(ad$`Pr(>F)`[ib], ad$`Pr(>F)`[ir]), permdisp_p = NA_real_,
    stringsAsFactors = FALSE)
}

# PCoA (metric MDS) of a dissimilarity, Cailliez-corrected so eigenvalues >= 0
pcoa_scores <- function(D) {
  cm  <- cmdscale(D, k = 2, eig = TRUE, add = TRUE)
  eig <- cm$eig; pos <- eig[eig > 0]
  list(pts = as.data.frame(cm$points) |> setNames(c("Axis1", "Axis2")),
       pct = round(100 * pos[1:2] / sum(pos), 1))
}

# stacked composition bar (percent within group)
composition_bar <- function(df, group_levels, group_lab, title, subtitle, file, width = 5.6) {
  comp <- df |>
    count(Group, Material, name = "n") |>
    complete(Group, Material, fill = list(n = 0)) |>
    group_by(Group) |> mutate(percent = 100 * n / sum(n)) |> ungroup()
  totals <- df |> count(Group, name = "N")
  labs_x <- setNames(sprintf("%s\n(n = %d)", totals$Group, totals$N), as.character(totals$Group))
  p <- ggplot(comp, aes(Group, percent, fill = Material)) +
    geom_col(width = 0.7, color = "white", linewidth = 0.3) +
    geom_text(data = subset(comp, percent >= 6), aes(label = sprintf("%.0f%%", percent)),
              position = position_stack(vjust = 0.5), size = 3, colour = "white", fontface = "bold") +
    scale_fill_manual(values = material_colors, drop = FALSE) +
    scale_x_discrete(limits = group_levels, labels = labs_x) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle, x = group_lab, y = "Percentage", fill = "Raw material") +
    base_theme + theme(panel.grid.major.x = element_blank())
  ggsave(file, p, width = width, height = 5.2, dpi = 300)
  invisible(comp)
}

# PCoA ordination plot (points coloured by basin, shaped by river)
pcoa_plot <- function(D, meta, title, subtitle, file, label_col = NULL) {
  sc <- pcoa_scores(D)
  dd <- cbind(sc$pts, meta)
  p <- ggplot(dd, aes(Axis1, Axis2, color = basin, shape = river_ID)) +
    geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", color = "grey55") +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", color = "grey55") +
    geom_point(size = 3.2, alpha = 0.85) +
    scale_color_manual(values = basin_colors, drop = FALSE) +
    labs(title = title, subtitle = subtitle,
         x = sprintf("PCoA axis 1 (%.1f%%)", sc$pct[1]),
         y = sprintf("PCoA axis 2 (%.1f%%)", sc$pct[2]),
         color = "Basin", shape = "River") +
    base_theme
  if (!is.null(label_col))
    p <- p + geom_text(aes(label = .data[[label_col]]), vjust = -1.1, size = 3, show.legend = FALSE)
  ggsave(file, p, width = 6.8, height = 5.4, dpi = 300)
  invisible(sc)
}

summary_rows <- list()
push <- function(x) summary_rows[[length(summary_rows) + 1]] <<- x

# ==============================================================================
# 1. Dataset A -- used tools (artifact-level Quina scrapers)
# ==============================================================================

cat("\n################## DATASET A: USED TOOLS (Quina scrapers) ##################\n")

sites <- read_excel(site_path)
names(sites) <- trimws(names(sites))
site_key <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            basin   = factor(strip_basin(basin), levels = basin_levels),
            river_ID = factor(trimws(river_ID), levels = river_levels))

sc <- read_excel(sc_path, sheet = "Quina scraper") |>
  transmute(Site_ID  = trimws(as.character(Site_ID)),
            Material = harmonise_lithology(Raw_material)) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  left_join(site_key, by = "Site_ID")

unmatched <- sc |> filter(is.na(basin) | is.na(river_ID))
if (nrow(unmatched) > 0) {
  cat("Site_IDs with no basin/river match:\n"); print(sort(unique(unmatched$Site_ID)))
  stop("Resolve the Site_ID <-> Code join before proceeding.")
}
sc <- sc |> mutate(Material = factor(Material, levels = material_levels))

cat("\nArtifact-level N =", nrow(sc), " across", n_distinct(sc$Site_ID), "sites\n")
cat("Raw material x basin (counts):\n");    print(table(sc$Material, sc$basin))
cat("\nRaw material x river_ID (counts):\n"); print(table(sc$Material, sc$river_ID))

# Bray-Curtis on the artifact x material indicator matrix (one 1 per artifact)
A_ind  <- table(seq_len(nrow(sc)), droplevels(sc$Material))
A_D    <- vegdist(as.matrix(unclass(A_ind)), method = "bray")
A_meta <- data.frame(basin = droplevels(sc$basin), river_ID = droplevels(sc$river_ID))

push(run_oneway(A_D, A_meta, "basin",    "A_tools", "used-material"))
push(run_oneway(A_D, A_meta, "river_ID", "A_tools", "used-material"))
push(run_nested(A_D, A_meta, "A_tools", "used-material"))

# composition figures (the near-constant Trachyte signal is the message here)
composition_bar(transmute(sc, Group = basin, Material),
                basin_levels, "Basin",
                "Used raw material by basin (Quina scrapers)",
                "Artifact-level composition; ~96% Trachyte overall",
                file.path(out_dir, "A_tools_composition_by_basin.png"))
composition_bar(transmute(sc, Group = river_ID, Material),
                river_levels, "River",
                "Used raw material by river (Quina scrapers)",
                "Artifact-level composition; ~96% Trachyte overall",
                file.path(out_dir, "A_tools_composition_by_river.png"), width = 6.4)

# site-level PCoA (exploratory; most sites are pure Trachyte -> they overlap)
site_comp <- sc |> count(Site_ID, basin, river_ID, Material, name = "n") |>
  complete(nesting(Site_ID, basin, river_ID), Material, fill = list(n = 0))
site_mat <- site_comp |>
  pivot_wider(names_from = Material, values_from = n, values_fill = 0)
site_meta <- site_mat |> transmute(Site_ID, basin = factor(basin, basin_levels),
                                   river_ID = factor(river_ID, river_levels))
site_M <- site_mat |> select(-Site_ID, -basin, -river_ID) |> as.matrix()
site_D <- vegdist(site_M, method = "bray")
pcoa_plot(site_D, site_meta,
          "Dataset A: site-level raw-material PCoA (Bray-Curtis)",
          "Exploratory; near-total Trachyte dominance -> pure-Trachyte sites coincide",
          file.path(out_dir, "A_tools_pcoa_site.png"), label_col = "Site_ID")

# ==============================================================================
# 2. Available cobbles -- load + composition figures (descriptive)
# ==============================================================================

cat("\n################## AVAILABLE COBBLES: COMPOSITION ##################\n")

cobbles <- read_excel(basin_path, sheet = "Sheet1")
names(cobbles) <- trimws(names(cobbles))
cobbles <- cobbles |>
  transmute(Loc = as.character(Loc),
            basin = factor(strip_basin(basin), levels = basin_levels),
            river_ID = factor(trimws(river_ID), levels = river_levels),
            Material = factor(harmonise_lithology(Lithology), levels = material_levels)) |>
  filter(!is.na(Material))

cat("\nCobble N =", nrow(cobbles), " across", n_distinct(cobbles$Loc), "localities\n")
cat("Lithology x Loc (counts):\n"); print(table(cobbles$Material, cobbles$Loc))

loc_long <- cobbles |> transmute(Group = factor(Loc), Material)
composition_bar(loc_long, levels(loc_long$Group), "Locality",
                "Available raw material by locality (basin survey)",
                "Cobble lithology composition per sampling point (Loc 1-5)",
                file.path(out_dir, "B_avail_composition_by_locality.png"), width = 7.2)
composition_bar(transmute(cobbles, Group = basin, Material), basin_levels, "Basin",
                "Available raw material by basin (basin survey)",
                "Cobble lithology pooled to basin",
                file.path(out_dir, "B_avail_composition_by_basin.png"))
composition_bar(transmute(cobbles, Group = river_ID, Material), river_levels, "River",
                "Available raw material by river (basin survey)",
                "Cobble lithology pooled to river",
                file.path(out_dir, "B_avail_composition_by_river.png"), width = 6.4)

# ==============================================================================
# 3. Dataset C -- availability spatial heterogeneity (cobble level, n = 469)
# ==============================================================================
# Bray-Curtis (CLR undefined for single-category rows). Only D ~ Loc is a valid,
# powered test (Loc = sampling unit); the basin/river one-way tests are
# pseudoreplicated -- read their R2 as effect size and take the spatial-scale
# decomposition from the hierarchical model below.

cat("\n################## DATASET C: AVAILABILITY, COBBLE LEVEL (spatial heterogeneity) ##################\n")

C_meta <- cobbles |> transmute(Loc = factor(Loc),
                              basin = droplevels(basin), river_ID = droplevels(river_ID),
                              Material = droplevels(Material))
C_ind <- table(seq_len(nrow(C_meta)), C_meta$Material)   # cobble x material indicator
C_D   <- vegdist(as.matrix(unclass(C_ind)), method = "bray")

cat("\nCobble N =", nrow(C_meta), " | Loc x lithology:\n")
print(table(C_meta$Loc, C_meta$Material))

# (1) PRIMARY -- spatial heterogeneity among the 5 localities (valid & powered)
C_loc <- run_oneway(C_D, C_meta, "Loc", "C_cobble", "avail-cobble"); push(C_loc)
# (2),(3) basin / river at cobble level -- R2 = effect size; p PSEUDOREPLICATED
cat("\n  NOTE: the two tests below are pseudoreplicated (basin/river are Loc-level\n",
    "  properties); read R2 as effect size, not p. The hierarchical model below\n",
    "  separates between-valley from within-valley variation.\n", sep = "")
push(run_oneway(C_D, C_meta, "basin",    "C_cobble", "avail-cobble"))
push(run_oneway(C_D, C_meta, "river_ID", "C_cobble", "avail-cobble"))

# hierarchical spatial-scale decomposition (basin / river-within-basin / Loc-within-river)
Chier <- adonis2(C_D ~ basin / river_ID / Loc, data = C_meta, permutations = perm, by = "terms")
cat("\n[C_cobble] hierarchical  D ~ basin/river_ID/Loc  (spatial-scale decomposition;\n",
    "  higher-level p anticonservative, only Loc-level contrasts have cobble replication):\n", sep = "")
print(Chier)

# contingency cross-check: single categorical var -> classic Loc x lithology test
gt <- chisq.test(table(C_meta$Loc, C_meta$Material), simulate.p.value = TRUE, B = 4999)
cat(sprintf("\nCross-check: chi-square Loc x lithology (Monte-Carlo, B=4999): X2 = %.1f, p = %.4f\n",
            unname(gt$statistic), gt$p.value))

# ---- viz C.1: spatial variance partition (hierarchical R2 by scale) ---------
star <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***",
  ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))
part <- data.frame(
  scale = factor(c("Between basins", "Between rivers (within basin)",
                   "Between localities (within river)"),
                 levels = c("Between localities (within river)",
                            "Between rivers (within basin)", "Between basins")),
  R2 = c(Chier$R2[1], Chier$R2[2], Chier$R2[3]),
  p  = c(Chier$`Pr(>F)`[1], Chier$`Pr(>F)`[2], Chier$`Pr(>F)`[3]))
part$lab <- sprintf("%.1f%%  %s", 100 * part$R2, star(part$p))
scale_cols <- c("Between basins" = "#8C6BB1",
                "Between rivers (within basin)" = "#3F7CAC",
                "Between localities (within river)" = "#7FB069")
p_part <- ggplot(part, aes(100 * R2, scale, fill = scale)) +
  geom_col(width = 0.66, color = "grey25", linewidth = 0.3) +
  geom_text(aes(label = lab), hjust = -0.1, size = 4, fontface = "bold", color = "#202124") +
  scale_fill_manual(values = scale_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = function(x) paste0(x, "%")) +
  labs(title = "Raw-material availability: spatial variance partition",
       subtitle = sprintf("469 cobbles  |  between-locality R2 = %.1f%% (D ~ Loc, p = %.3f)  |  residual %.1f%%",
                          100 * C_loc$R2, C_loc$p, 100 * Chier$R2[4]),
       x = "Variance explained (R2, PERMANOVA)", y = NULL,
       caption = paste("** p<=0.01  *** p<0.001  ns not significant  (999 permutations -> p floor = 0.001).",
         "Basin & river p are anticonservative (cobbles nested in locality);",
         "locality-within-river uses valid cobble replication -> ns = same-river localities are compositionally alike.",
         sep = "\n")) +
  base_theme +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#454649"),
        plot.caption = element_text(hjust = 0, size = 8, color = "#454649"))
ggsave(file.path(out_dir, "C_cobble_variance_partition.png"), p_part, width = 9.2, height = 4.5, dpi = 300)

# ---- viz C.2: PERMDISP -- compositional evenness per locality (mean +/- SE) ----
# Distance-to-centroid on categorical (Bray-Curtis) data is discrete, so raw
# boxplots are degenerate; the informative summary is the group MEAN dispersion
# (= the PERMDISP statistic), which reads as compositional evenness per locality.
bd_loc  <- betadisper(C_D, C_meta$Loc)
disp_df <- data.frame(Loc = factor(C_meta$Loc), dist = bd_loc$distances)
disp_sum <- disp_df |> group_by(Loc) |>
  summarise(mean = mean(dist), se = sd(dist) / sqrt(n()), .groups = "drop")
loc_cols <- c("1" = "#C9603F", "2" = "#E69F00", "3" = "#7FB069", "4" = "#3F7CAC", "5" = "#8C6BB1")
p_disp <- ggplot(disp_sum, aes(Loc, mean, color = Loc)) +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.9, size = 0.85) +
  scale_color_manual(values = loc_cols) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Availability: compositional evenness per locality (PERMDISP)",
       subtitle = sprintf("Mean betadisper distance to centroid +/- SE; localities differ, p = %.3f", C_loc$permdisp_p),
       x = "Sampling locality (Loc)",
       y = "Mean distance to centroid\n(higher = more even lithology mix)") +
  base_theme + theme(legend.position = "none", plot.title = element_text(size = 13.5))
ggsave(file.path(out_dir, "C_cobble_permdisp_dispersion.png"), p_disp, width = 7.8, height = 4.6, dpi = 300)

# ---- viz C.3: cobble-level PCoA scatter (Bray-Curtis) + locality centroids -----
# Single-category rows -> distances are 0/1 -> the 469 cobbles collapse onto 4
# lithology vertices (shown jittered). The 5 locality CENTROIDS (mean positions)
# are the structure that D ~ Loc tests: their spread = between-locality signal.
pc  <- cmdscale(C_D, k = 2, eig = TRUE, add = TRUE)
evp <- pc$eig[pc$eig > 0]; pct <- round(100 * evp[1:2] / sum(evp), 1)
scl <- data.frame(A1 = pc$points[, 1], A2 = pc$points[, 2],
                  Loc = C_meta$Loc, basin = C_meta$basin,
                  river_ID = C_meta$river_ID, Material = C_meta$Material)
set.seed(2226)
scl$A1j <- scl$A1 + rnorm(nrow(scl), 0, 0.045 * diff(range(scl$A1)))
scl$A2j <- scl$A2 + rnorm(nrow(scl), 0, 0.045 * diff(range(scl$A2)))
cen <- scl |> group_by(Loc, basin, river_ID) |>
  summarise(cA1 = mean(A1), cA2 = mean(A2), .groups = "drop")
spk <- left_join(scl, cen, by = c("Loc", "basin", "river_ID"))
p_ord <- ggplot(scl, aes(A1j, A2j)) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", color = "grey75") +
  geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", color = "grey75") +
  geom_segment(data = spk, aes(A1j, A2j, xend = cA1, yend = cA2),
               color = "grey70", linewidth = 0.2, alpha = 0.07, inherit.aes = FALSE) +
  geom_point(aes(color = Material), size = 1.5, alpha = 0.4, shape = 16) +
  geom_point(data = cen, aes(cA1, cA2, fill = basin, shape = river_ID),
             size = 5, color = "grey15", stroke = 0.6, inherit.aes = FALSE) +
  geom_text(data = cen, aes(cA1, cA2, label = Loc), inherit.aes = FALSE,
            fontface = "bold", size = 3.2, color = "grey15", vjust = -1.5) +
  scale_color_manual(values = material_colors, drop = FALSE) +
  scale_fill_manual(values = basin_colors, drop = FALSE) +
  scale_shape_manual(values = c(Sangyuan = 21, Liandong = 24, Caifeng = 22), drop = FALSE) +
  labs(title = "Dataset C: cobble-level PCoA (Bray-Curtis, 469 cobbles)",
       subtitle = "Cobbles (small, jittered) collapse to 4 lithology vertices; large points = locality centroids (fill = basin, shape = river)",
       x = sprintf("PCoA axis 1 (%.1f%%)", pct[1]),
       y = sprintf("PCoA axis 2 (%.1f%%)", pct[2]),
       color = "Raw material", fill = "Basin", shape = "River") +
  base_theme +
  theme(plot.subtitle = element_text(size = 9.5)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)),
         fill = guide_legend(override.aes = list(shape = 21)))
ggsave(file.path(out_dir, "C_cobble_pcoa_scatter.png"), p_ord, width = 8.4, height = 6.0, dpi = 300)

# ==============================================================================
# 4. Combined summary (variance explained by each grouping variable)
# ==============================================================================

summary_tbl <- bind_rows(summary_rows)
cat("\n################## SUMMARY: variance in raw-material composition explained ##################\n")
print(summary_tbl, row.names = FALSE, digits = 3)

writeLines(c(
  "GUARDRAILS -- raw-material composition PERMANOVA (basin vs river_ID)",
  "* river_ID is NESTED in basin (Sangyuan,Liandong c Binchuan; Caifeng c Heqing):",
  "  one-way river_ID R2 >= basin R2 almost by construction. Use the nested model",
  "  (D ~ basin/river_ID) to split between-basin vs river-within-basin variation.",
  "* Dataset A (used tools) is ~96% Trachyte -> tiny R2 is the finding (uniform",
  "  selection). Artifact-level test is pseudoreplicated; R2 = effect size, p exploratory.",
  "* Dataset C (cobble level, n=469) answers 'is availability spatially heterogeneous?'",
  "  ONLY via D ~ Loc (valid: Loc is the sampling unit, cobbles are within-Loc",
  "  replicates). C's one-way basin/river tests are pseudoreplicated (cobbles nested",
  "  in Loc): read their R2 as effect size, not their p. The hierarchical model",
  "  (D ~ basin/river_ID/Loc) is what separates between-valley from within-valley",
  "  variation; only its Loc-level term has cobble replication.",
  "  Cobble rows are single-category -> Bray-Curtis, NOT Aitchison/CLR."),
  file.path(out_dir, "_GUARDRAILS.txt"))

cat("\n########## DONE. Outputs under", out_dir, "##########\n")
