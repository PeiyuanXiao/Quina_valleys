# QV_landscape_triage.R
# Site-level landscape triage of surface-collected (SC) Quina scrapers.
#
# Unit = the site: each site enters once as the MEDIAN of its scrapers on the 6
# technical variables. Exploratory (few sites, unbalanced, basin/landscape
# confounded) -> rank by effect size (R2 / rho / median spread), not p<0.05. SC
# only, one retouch tool-class: this is the landscape distribution of reduction
# intensity, not a provisioning system. Full guardrails -> _GUARDRAILS.txt.
#
# Question: are the 6 technical-consistency metrics structured by landscape?
#
# Pipeline:
#   1. Basin (Binchuan vs Heqing)      -- categorical: PERMANOVA + PERMDISP + PCA.
#   2. Height above river (m)          -- gradient + coarse-bin robustness.
#      (= the continuous form of terrace level; unlike absolute elevation it is
#       measured from the local channel, so basin base level cancels out.)
#   3. Distance to nearest river       -- gradient: Spearman (wtd) + site db-RDA.
#   4. Site size (per-site SC count)   -- gradient + coarse-bin robustness.
#   + cross-analysis: marginal models + collinearity (all site-level).
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#   - data/Site_information.xlsx
#
# Output:
#   - output/technological_consistency/ (analysis1_basin/ ... cross_analysis/)

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan",
                       "rstatix", "ggpubr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install the following R packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(rstatix)

set.seed(123)

# ---- constants -------------------------------------------------------------
variables <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
               "N_Scar", "Ave_RG", "Edge_Angle")
kw_vars    <- c("Ave_GIUR", "N_Scar", "Ave_RG")                 # KW + Dunn
welch_vars <- c("Retouch_length_index", "Thickness", "Edge_Angle")  # Welch ANOVA + t

proj_dir   <- here::here()
sc_path    <- file.path(proj_dir, "data", "Quina_scraper_surface.xlsx")
site_path  <- file.path(proj_dir, "data", "Site_information.xlsx")
out_root   <- file.path(proj_dir, "output", "technological_consistency")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

guardrails <- c(
  "INTERPRETATION GUARDRAILS (SITE-LEVEL landscape triage, SC Quina scrapers)",
  "* Unit = the site (each site enters once, as the MEDIAN of its scrapers).",
  "  No artifact-level pseudoreplication -- but few sites + unbalanced groups",
  "  -> read p as exploratory; rank by effect size (R^2 / rho / median spread).",
  "* Many site medians rest on 1-2 scrapers and are NOISY; the weighted Spearman",
  "  (weight = n scrapers per site) down-weights singleton sites -- read both.",
  "* Significant PERMANOVA + small R^2 = overlapping groups; read with PERMDISP",
  "  (if dispersion differs, the difference is spread, not centroid location).",
  "* Height above river is the CONTINUOUS form of terrace level (measured from the",
  "  local channel, so basin base level cancels out; Spearman vs basin ~ 0). But it",
  "  is strongly right-skewed: the 3 hilltop sites (150-187 m) sit far above the",
  "  terrace sites (25-73 m) and carry high leverage in the linear PERMANOVA term",
  "  -> read the rank-based Spearman and the banded check alongside it.",
  "* Marginal models rank RELATIVE structure only; terms are NOT independent effects.",
  "* SC only, one retouch tool-class: this is the landscape distribution of",
  "  reduction intensity, NOT evidence of a provisioning system."
)

# ---- shared visual style (matches QV_analysis.R) ---------------------------
ordination_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649",
                                 margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )
corr_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "#454649", hjust = 0.5),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# categorical palettes (muted, low-saturation, in the QV idiom)
basin_colors    <- c(Binchuan = "#C9603F", Heqing = "#3F7CAC")
hbin_colors     <- c(`<=40` = "#7FB069", `40-60` = "#E6C25C", `>60` = "#E07C90")
sizebin_colors  <- c(`1` = "#C9D6DF", `2-3` = "#6BA8CE", `4+` = "#2C5F7C")

# coarse height-above-river bands (m); sites span 25-187 m, tertile-like 9/11/6
h_breaks <- c(-Inf, 40, 60, Inf)
h_labels <- c("<=40", "40-60", ">60")

fmt_p <- function(p) ifelse(is.na(p), "NA",
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3))))

# ---- PCA helpers (identical drawing logic to QV_analysis.R) ----------------
make_centroids <- function(scores, x_var, y_var) {
  scores |>
    group_by(Group) |>
    summarise(x_centroid = mean(.data[[x_var]], na.rm = TRUE),
              y_centroid = mean(.data[[y_var]], na.rm = TRUE), .groups = "drop")
}
make_spokes <- function(scores, centroids) {
  dplyr::left_join(scores, centroids, by = "Group")
}
make_convex_hulls <- function(scores, x_var, y_var) {
  scores |>
    group_by(Group) |>
    filter(n() >= 3) |>
    group_modify(function(g, key) {
      h <- chull(g[[x_var]], g[[y_var]])
      bind_rows(g[h, , drop = FALSE], g[h[1], , drop = FALSE])
    }) |>
    ungroup()
}

# prcomp PCA on a z-scored matrix + ordination plot; writes scores/loadings/png.
run_pca_plot <- function(mat, groups, colors, outdir, prefix, title, subtitle) {
  pca <- prcomp(mat, center = TRUE, scale. = FALSE)
  vexp <- pca$sdev^2 / sum(pca$sdev^2) * 100
  scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                       Group = factor(groups, levels = names(colors)))
  loadings <- data.frame(Variable = rownames(pca$rotation),
                         PC1 = pca$rotation[, 1], PC2 = pca$rotation[, 2],
                         row.names = NULL)

  cent  <- make_centroids(scores, "PC1", "PC2")
  spoke <- make_spokes(scores, cent)
  hull  <- make_convex_hulls(scores, "PC1", "PC2")

  p <- ggplot(scores, aes(PC1, PC2, color = Group)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4, linetype = "dashed") +
    geom_polygon(data = hull, aes(PC1, PC2, fill = Group, group = Group),
                 alpha = 0.12, color = NA, inherit.aes = FALSE) +
    geom_path(data = hull, aes(PC1, PC2, color = Group, group = Group),
              linewidth = 0.45, alpha = 0.65, inherit.aes = FALSE) +
    geom_segment(data = spoke,
                 aes(PC1, PC2, xend = x_centroid, yend = y_centroid, color = Group),
                 linewidth = 0.25, alpha = 0.35, inherit.aes = FALSE) +
    geom_point(size = 2.4, alpha = 0.85, shape = 16) +
    geom_point(data = cent, aes(x_centroid, y_centroid, color = Group),
               shape = 21, fill = "white", size = 4, stroke = 1.1, inherit.aes = FALSE) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    labs(title = title, subtitle = subtitle,
         x = paste0("PC1 (", round(vexp[1], 1), "%)"),
         y = paste0("PC2 (", round(vexp[2], 1), "%)")) +
    coord_equal() + ordination_theme +
    guides(fill = "none",
           color = guide_legend(override.aes = list(size = 3.2, alpha = 1, shape = 16)))
  ggsave(file.path(outdir, paste0(prefix, "_pca_ordination.png")), p,
         width = 7.6, height = 5.8, dpi = 300)

  # loadings bar (same look as QV_analysis.R)
  ll <- loadings |>
    pivot_longer(c(PC1, PC2), names_to = "PC", values_to = "Loading") |>
    mutate(PC = factor(PC, levels = c("PC1", "PC2")),
           Variable = factor(Variable, levels = rev(variables)),
           Sign = ifelse(Loading >= 0, "Positive", "Negative"))
  pl <- ggplot(ll, aes(Loading, Variable, fill = Sign)) +
    geom_col(width = 0.7, color = "#303238", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
    facet_wrap(~ PC) +
    scale_fill_manual(values = c(Positive = "#6BA8CE", Negative = "#E07C90")) +
    scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(subtitle = "PCA variable loadings (site-level)", x = "Loading", y = NULL, fill = NULL) +
    ordination_theme + theme(panel.grid.major.y = element_blank(), legend.position = "top")
  ggsave(file.path(outdir, paste0(prefix, "_pca_loadings.png")), pl,
         width = 7.6, height = 4.6, dpi = 300)

  list(scores = scores, loadings = loadings, var_explained = vexp)
}

# ---- per-variable categorical engine (KW+Dunn / Welch ANOVA+t) -------------
# dd must contain a factor column `Grp` plus the 6 variable columns (SITE rows).
# Robust to small / zero-variance groups: a failed test is skipped, not fatal.
per_variable_tests <- function(dd, colors, outdir, prefix) {
  long <- dd |>
    select(Grp, all_of(variables)) |>
    pivot_longer(all_of(variables), names_to = "Variable", values_to = "Value") |>
    filter(is.finite(Value)) |>
    mutate(Variable = factor(Variable, levels = variables))

  kw_long <- long |> filter(Variable %in% kw_vars)    |> mutate(Variable = droplevels(Variable))
  we_long <- long |> filter(Variable %in% welch_vars) |> mutate(Variable = droplevels(Variable))

  safe <- function(expr) tryCatch(expr, error = function(e) {
    message("  per-variable test skipped: ", conditionMessage(e)); NULL })

  kw_omn <- safe(kw_long |> group_by(Variable) |> kruskal_test(Value ~ Grp) |> ungroup())
  kw_ph  <- safe(kw_long |> group_by(Variable) |>
    dunn_test(Value ~ Grp, p.adjust.method = "bonferroni") |> ungroup())
  we_omn <- safe(we_long |> group_by(Variable) |> welch_anova_test(Value ~ Grp) |> ungroup())
  we_ph  <- safe(we_long |> group_by(Variable) |>
    pairwise_t_test(Value ~ Grp, pool.sd = FALSE, p.adjust.method = "bonferroni") |> ungroup())

  # group medians (direction) + median spread (effect magnitude)
  meds <- long |> group_by(Variable, Grp) |>
    summarise(median = median(Value, na.rm = TRUE), .groups = "drop")
  eff <- meds |> group_by(Variable) |>
    summarise(median_spread = max(median) - min(median), .groups = "drop")
  omn_p <- bind_rows(
    if (!is.null(kw_omn)) kw_omn |> transmute(Variable = as.character(Variable), omnibus_p = p),
    if (!is.null(we_omn)) we_omn |> transmute(Variable = as.character(Variable), omnibus_p = p))
  eff <- eff |> mutate(Variable = as.character(Variable))
  if (nrow(omn_p) > 0) eff <- eff |> left_join(omn_p, by = "Variable") else eff$omnibus_p <- NA_real_

  # boxplot with Bonferroni post-hoc brackets (free_y; per-facet positions)
  brackets <- bind_rows(
    if (!is.null(kw_ph)) kw_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif),
    if (!is.null(we_ph)) we_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif))

  bx <- ggplot(long, aes(Grp, Value)) +
    geom_jitter(aes(color = Grp), width = 0.20, height = 0, size = 1.9, alpha = 0.7, shape = 16) +
    geom_boxplot(color = "black", fill = NA, width = 0.62, linewidth = 0.6, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "black") +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    scale_color_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(subtitle = "Site medians per group; pairwise post-hoc Bonferroni (ns / * / ** / *** / ****)",
         x = NULL, y = NULL) +
    ordination_theme +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")

  if (nrow(brackets) > 0) {
    brackets <- brackets |> mutate(Variable = factor(as.character(Variable), levels = variables))
    ranges <- long |> group_by(Variable) |>
      summarise(ymax = max(Value, na.rm = TRUE),
                yrange = diff(range(Value, na.rm = TRUE)), .groups = "drop")
    brackets <- brackets |> group_by(Variable) |> mutate(step = row_number()) |> ungroup() |>
      left_join(ranges, by = "Variable") |>
      mutate(y.position = ymax + yrange * (0.06 + 0.10 * step))
    bx <- bx + ggpubr::stat_pvalue_manual(brackets, label = "p.adj.signif",
                                          y.position = "y.position", tip.length = 0.012,
                                          bracket.size = 0.4, label.size = 3, color = "#202124")
  }
  ggsave(file.path(outdir, paste0(prefix, "_variable_boxplots.png")), bx,
         width = 8.4, height = 6.8, dpi = 300)

  eff
}

# ---- categorical analysis driver (PERMANOVA + PERMDISP + PCA + per-var) -----
# `dat` is the SITE-LEVEL frame; each row is one site.
run_categorical <- function(dat, group_col, group_levels, colors, outdir, prefix,
                            title, perm = 999) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  writeLines(guardrails, file.path(outdir, "_GUARDRAILS.txt"))

  dd <- dat |>
    filter(!is.na(.data[[group_col]])) |>
    mutate(Grp = factor(as.character(.data[[group_col]]), levels = group_levels)) |>
    filter(!is.na(Grp)) |>
    filter(if_all(all_of(variables), ~ !is.na(.x)))

  cat("\n==== ", title, " ====\n", sep = "")
  cat("N SITES per group:\n"); print(table(dd$Grp))
  cat("Total sites =", nrow(dd), "\n")

  mat <- scale(as.matrix(dd[, variables]))
  d   <- dist(mat, method = "euclidean")

  ad <- adonis2(d ~ Grp, data = dd, permutations = perm)
  cat("\nPERMANOVA (adonis2, site-level):\n"); print(ad)

  bd <- betadisper(d, dd$Grp)
  pt <- permutest(bd, permutations = perm, pairwise = TRUE)
  cat("\nPERMDISP (betadisper + permutest, site-level):\n"); print(pt$tab)

  sub <- sprintf("Sites as units (n = %d). PERMANOVA R2 = %.3f, p %s  |  PERMDISP p %s",
                 nrow(dd), ad$R2[1], fmt_p(ad$`Pr(>F)`[1]), fmt_p(pt$tab$`Pr(>F)`[1]))
  pca <- run_pca_plot(mat, as.character(dd$Grp), colors, outdir, prefix, title, sub)
  eff <- per_variable_tests(dd, colors, outdir, prefix)

  list(R2 = ad$R2[1], F = ad$F[1], p = ad$`Pr(>F)`[1],
       disp_p = pt$tab$`Pr(>F)`[1], eff = eff, data = dd, mat = mat, dist = d,
       pca = pca)
}

# weighted Spearman = weighted Pearson on ranks (base R; no extra packages)
weighted_spearman <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w)
  x <- rank(x[ok]); y <- rank(y[ok]); w <- w[ok]
  mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w)
  num <- sum(w * (x - mx) * (y - my))
  den <- sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
  if (den == 0) NA_real_ else num / den
}

# ---- continuous-gradient analysis driver (SITE-LEVEL only) -----------------
# `site_df` is one row per site; gradient is a site attribute.
run_gradient <- function(site_df, grad_col, grad_label, outdir, prefix, perm = 999) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  writeLines(guardrails, file.path(outdir, "_GUARDRAILS.txt"))

  dd <- site_df |>
    filter(!is.na(.data[[grad_col]])) |>
    filter(if_all(all_of(variables), ~ !is.na(.x))) |>
    mutate(grad = as.numeric(.data[[grad_col]]))

  cat("\n==== Technical metrics ~ ", grad_label, " (site-level gradient) ====\n", sep = "")
  cat("N sites =", nrow(dd), " | scrapers behind medians: min =", min(dd$n_art),
      ", median =", median(dd$n_art), ", max =", max(dd$n_art), "\n")
  cat("Sites resting on <3 scrapers (noisy medians):", sum(dd$n_art < 3), "of", nrow(dd), "\n")

  mat <- scale(as.matrix(dd[, variables]))
  d   <- dist(mat, method = "euclidean")

  # ---- multivariate (SITE-LEVEL; one row per site, NOT pseudoreplicated) ----
  ad <- adonis2(d ~ grad, data = data.frame(grad = dd$grad), permutations = perm)
  cat(sprintf("\nadonis2(dist ~ %s): R2 = %.3f, p %s  [site-level]\n",
              grad_label, ad$R2[1], fmt_p(ad$`Pr(>F)`[1])))

  # ---- per-variable SITE-LEVEL Spearman (unweighted + weighted by n_art) ----
  per_var <- do.call(rbind, lapply(variables, function(v) {
    ct <- suppressWarnings(cor.test(dd[[v]], dd$grad, method = "spearman", exact = FALSE))
    data.frame(Variable = v, n_sites = nrow(dd),
               rho_site = unname(ct$estimate), p_site = ct$p.value,
               rho_site_wtd = weighted_spearman(dd[[v]], dd$grad, dd$n_art))
  }))
  cat("\nPer-variable Spearman (site-level; rho_site_wtd weights by n scrapers per site):\n")
  print(per_var, row.names = FALSE)

  # ---- faceted scatter: one point per site (size = n scrapers) + lm trend ----
  long <- dd |> select(Site_ID, Basin, n_art, grad, all_of(variables)) |>
    pivot_longer(all_of(variables), names_to = "Variable", values_to = "Value") |>
    mutate(Variable = factor(Variable, levels = variables))
  labs_df <- per_var |>
    mutate(Variable = factor(Variable, levels = variables),
           label = sprintf("rho = %.2f (p %s)\nrho_wtd = %.2f",
                           rho_site, fmt_p(p_site), rho_site_wtd))
  sc_p <- ggplot(long, aes(grad, Value)) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#6BA8CE", fill = "#9BC7DF", linewidth = 0.8) +
    geom_point(aes(size = n_art, color = Basin), alpha = 0.8, shape = 16) +
    geom_text(data = labs_df, aes(x = -Inf, y = Inf, label = label),
              hjust = -0.06, vjust = 1.15, size = 3.0, color = "#202124",
              lineheight = 0.95, inherit.aes = FALSE) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    scale_color_manual(values = basin_colors) +
    scale_size_continuous(range = c(1.6, 6), name = "n scrapers") +
    labs(title = paste0("Technical metrics vs ", grad_label, " (site-level)"),
         subtitle = "one point per site; point size = n scrapers behind the median; line = lm",
         x = grad_label, y = NULL, color = "Basin") +
    corr_theme
  ggsave(file.path(outdir, paste0(prefix, "_scatter.png")), sc_p,
         width = 9.6, height = 6.2, dpi = 300)

  list(mv_R2 = ad$R2[1], mv_p = ad$`Pr(>F)`[1], per_var = per_var, data = dd)
}

# ==============================================================================
# Load + schema check + join + site-level aggregation
# ==============================================================================
cat("\n########## DATA / SCHEMA / JOIN / SITE AGGREGATION ##########\n")

sc_raw <- read_excel(sc_path, sheet = "Quina scraper")
cat("\nArtifact file columns (Quina scraper):\n"); print(names(sc_raw))
stopifnot(all(variables %in% names(sc_raw)), "Site_ID" %in% names(sc_raw))

sc <- sc_raw |>
  mutate(Site_ID = trimws(as.character(Site_ID)),
         across(all_of(variables), as.numeric))

site_raw <- read_excel(site_path)
names(site_raw) <- trimws(names(site_raw))           # headers carry trailing spaces
cat("\nSite file columns (Site_information.xlsx, trimmed):\n"); print(names(site_raw))
req_site <- c("Code", "basin", "h_river_m", "d_river_m")
miss_site <- setdiff(req_site, names(site_raw))
if (length(miss_site) > 0)
  stop("Missing required site-level fields in Site_information.xlsx: ",
       paste(miss_site, collapse = ", "))

site_land <- site_raw |>
  transmute(
    Site_ID  = trimws(as.character(Code)),
    Basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    Distance_to_water = as.numeric(d_river_m),
    Height_above_river = as.numeric(h_river_m),
    n_Quina_scraper_col = as.numeric(n_Quina_scraper)
  )

# Site_size = per-site SC artifact count (preferred), cross-checked vs the column
sc_count <- sc |> count(Site_ID, name = "Site_size")
size_check <- site_land |> left_join(sc_count, by = "Site_ID") |>
  mutate(Site_size = dplyr::coalesce(Site_size, 0L))
mismatch <- size_check |> filter(Site_size != n_Quina_scraper_col)
cat("\nSite_size cross-check (SC count vs n_Quina_scraper column): ",
    nrow(mismatch), " mismatched site(s)\n", sep = "")
if (nrow(mismatch) > 0) print(mismatch |> select(Site_ID, Site_size, n_Quina_scraper_col))

# artifacts -> landscape join, complete-case on the 6 variables
sc_cc <- sc |>
  left_join(site_land, by = "Site_ID") |>
  left_join(sc_count,  by = "Site_ID") |>
  filter(if_all(all_of(variables), ~ !is.na(.x)))
unmatched <- sc_cc |> filter(is.na(Basin))
cat("Complete-case artifacts with no matching site row:", nrow(unmatched), "\n")
if (nrow(unmatched) > 0) {
  cat(" -> STOPPING: unmatched Site_IDs: ",
      paste(sort(unique(unmatched$Site_ID)), collapse = ", "), "\n")
  stop("Unmatched artifacts; resolve the join key before proceeding.")
}

# ---- AGGREGATE TO SITE LEVEL: each site -> median vector (the analysis frame) ----
site_df <- sc_cc |>
  group_by(Site_ID) |>
  summarise(n_art = n(),
            across(all_of(variables), ~ median(.x, na.rm = TRUE)),
            Basin    = dplyr::first(Basin),
            Distance_to_water = dplyr::first(Distance_to_water),
            Height_above_river = dplyr::first(Height_above_river),
            Site_size = dplyr::first(Site_size),
            .groups = "drop") |>
  mutate(H_bin = cut(Height_above_river, breaks = h_breaks, labels = h_labels))

cat("\nSITE-LEVEL analysis frame: n sites =", nrow(site_df),
    "(aggregated from", nrow(sc_cc), "complete-case scrapers)\n")
cat("Scrapers per site (site-median reliability):\n"); print(summary(site_df$n_art))
cat("Sites resting on <3 scrapers (median ~= a single piece):",
    sum(site_df$n_art < 3), "of", nrow(site_df), "\n")
cat("\nN sites by Basin:\n");    print(table(site_df$Basin))
cat("\nHeight above river (m):\n"); print(summary(site_df$Height_above_river))
cat("\nN sites by height band:\n"); print(table(site_df$H_bin))
cat("\nBasin x height band contingency (site-level; empty cells => confounded):\n")
print(table(site_df$Basin, site_df$H_bin))

# accumulator for the cross-analysis triage summary
summary_rows <- list()
push_cat <- function(tag, res) {
  e <- res$eff
  summary_rows[[length(summary_rows) + 1]] <<- data.frame(
    analysis = tag, variable = e$Variable,
    mv_R2 = res$R2, mv_p = res$p, permdisp_p = res$disp_p,
    effect_primary = e$median_spread, effect_primary_type = "median_spread",
    p_primary = e$omnibus_p,
    effect_secondary = NA_real_, effect_secondary_type = NA_character_,
    p_secondary = NA_real_, stringsAsFactors = FALSE)
}
push_grad <- function(tag, res) {
  pv <- res$per_var
  summary_rows[[length(summary_rows) + 1]] <<- data.frame(
    analysis = tag, variable = pv$Variable,
    mv_R2 = res$mv_R2, mv_p = res$mv_p, permdisp_p = NA_real_,
    effect_primary = pv$rho_site, effect_primary_type = "rho_site",
    p_primary = pv$p_site,
    effect_secondary = pv$rho_site_wtd, effect_secondary_type = "rho_site_wtd",
    p_secondary = NA_real_, stringsAsFactors = FALSE)
}

# ==============================================================================
# 1. Basin (Binchuan vs Heqing; site-level)
# ==============================================================================
a1_dir <- file.path(out_root, "analysis1_basin")
a1 <- run_categorical(site_df, "Basin", c("Binchuan", "Heqing"), basin_colors,
                      a1_dir, "a1_basin", "Analysis 1: technical metrics ~ Basin (site-level)")
push_cat("1_basin", a1)

# ---- 1a. composition diagnostic: is "Heqing" really "Tianhua/Longtan area"? ----
prox     <- c("THC", "LT")                                # Tianhua Cave, Longtan
heq      <- site_df |> filter(Basin == "Heqing")
n_prox   <- sum(heq$Site_ID %in% prox)
cat(sprintf("\nComposition diagnostic: Heqing has %d sites; %d are Tianhua/Longtan-proximal (THC/LT).\n",
            nrow(heq), n_prox))
if (nrow(heq) > 0 && n_prox / nrow(heq) >= 0.4) {
  cat("WARNING: 'Heqing basin' is dominated by Tianhua/Longtan-proximal sites\n",
      "  => here 'Heqing basin' ~= 'Tianhua/Longtan area'; Basin and site are CONFOUNDED.\n")
}
# sensitivity: drop THC/LT sites from Heqing, re-run the basin contrast (site-level)
sens_data <- site_df |> filter(!(Basin == "Heqing" & Site_ID %in% prox))
a1_sens <- run_categorical(sens_data, "Basin", c("Binchuan", "Heqing"), basin_colors,
                           a1_dir, "a1_basin_sensitivity",
                           "Analysis 1 sensitivity: Basin (Heqing excl. THC/LT, site-level)")
sens_tbl <- data.frame(
  Model = c("Basin (all sites)", "Basin (Heqing excl. THC/LT)"),
  R2 = c(a1$R2, a1_sens$R2), p_value = c(a1$p, a1_sens$p),
  PERMDISP_p = c(a1$disp_p, a1_sens$disp_p))
cat("\nBasin sensitivity (with vs without THC/LT in Heqing):\n"); print(sens_tbl, row.names = FALSE)

# ==============================================================================
# 2. Height above river (continuous gradient, site-level) + height-band robustness
#    Continuous form of terrace level; basin base level cancels out.
# ==============================================================================
a2_dir <- file.path(out_root, "analysis2_height")
a2 <- run_gradient(site_df, "Height_above_river", "Height above river (m)",
                   a2_dir, "a2_height")
push_grad("2_height_continuous", a2)

# bin robustness: does a continuous height trend survive coarse banding of SITES?
cat("\nHeight-above-river bands (site-level N):\n"); print(table(site_df$H_bin))
a2b <- run_categorical(site_df, "H_bin", h_labels, hbin_colors,
                       a2_dir, "a2_heightbin",
                       "Analysis 2 robustness: technical metrics ~ height band (site-level)")
push_cat("2_height_bands", a2b)

# ---- 2a. collinearity with Basin: marginal (Type-III-like) PERMANOVA ----
mat2 <- scale(as.matrix(site_df[, variables]))
d2   <- dist(mat2, method = "euclidean")
margin_BH <- tryCatch(
  adonis2(d2 ~ Basin + Height_above_river, data = site_df, by = "margin", permutations = 999),
  error = function(e) { message("marginal Basin+Height failed (collinear design): ",
                                conditionMessage(e)); NULL })
if (!is.null(margin_BH)) {
  cat("\nMarginal PERMANOVA dist ~ Basin + Height above river (site-level, by='margin'):\n")
  print(margin_BH)
  cat("Reading: Height's marginal R2/p = its contribution AFTER Basin is controlled.\n")
}
cat("\nHeight above river by basin (site-level; should be comparable -- unlike",
    "\nabsolute elevation, height is measured from the local channel):\n")
print(site_df |> group_by(Basin) |>
        summarise(n = n(), min = min(Height_above_river),
                  median = median(Height_above_river),
                  max = max(Height_above_river), .groups = "drop") |> as.data.frame())

# ==============================================================================
# 3. Distance to water (continuous gradient, site-level)
# ==============================================================================
a3_dir <- file.path(out_root, "analysis3_distance")
a3 <- run_gradient(site_df, "Distance_to_water", "Distance to river (m)",
                   a3_dir, "a3_distance")
push_grad("3_distance", a3)

# ==============================================================================
# 4. Site size (continuous gradient, site-level) + bin robustness
# ==============================================================================
a4_dir <- file.path(out_root, "analysis4_size")
a4 <- run_gradient(site_df, "Site_size", "Site size (SC artifact count)",
                   a4_dir, "a4_size")
push_grad("4_size_continuous", a4)
writeLines(c(guardrails, "",
  "ANALYSIS 4 SPECIFIC CAVEAT:",
  "Site_size has a tiny dynamic range (many sites = 1-2 pieces) and the RESPONSE",
  "(retouch intensity) may SHARE COLLECTION BIAS with the PREDICTOR (size): more",
  "intensively collected sites yield both more pieces AND more retouched pieces.",
  "A correlation here can be a collection artefact, possibly even sign-reversed."),
  file.path(a4_dir, "a4_size_CAVEAT.txt"))

# bin robustness: does a continuous trend survive coarse binning of SITES?
site_bin <- site_df |>
  mutate(Size_bin = cut(Site_size, breaks = c(-Inf, 1, 3, Inf),
                        labels = c("1", "2-3", "4+")))
cat("\nSite_size bins (site-level N):\n"); print(table(site_bin$Size_bin))
a4b <- run_categorical(site_bin, "Size_bin", c("1", "2-3", "4+"), sizebin_colors,
                       a4_dir, "a4_sizebin",
                       "Analysis 4 robustness: technical metrics ~ site-size bin (site-level)")
push_cat("4_size_bins", a4b)

# ==============================================================================
# Cross-analysis triage summary
# ==============================================================================
summary_tbl <- bind_rows(summary_rows)
cat("\n########## CROSS-ANALYSIS TRIAGE SUMMARY (site-level; rank by effect size) ##########\n")
print(summary_tbl, row.names = FALSE)

# per-analysis multivariate headline (which grouping/gradient structures most?)
mv_head <- bind_rows(
  data.frame(analysis = "1_basin",           mv_R2 = a1$R2,    mv_p = a1$p,    permdisp_p = a1$disp_p),
  data.frame(analysis = "2_height_continuous", mv_R2 = a2$mv_R2, mv_p = a2$mv_p, permdisp_p = NA_real_),
  data.frame(analysis = "2_height_bands",      mv_R2 = a2b$R2,   mv_p = a2b$p,   permdisp_p = a2b$disp_p),
  data.frame(analysis = "3_distance",        mv_R2 = a3$mv_R2, mv_p = a3$mv_p, permdisp_p = NA_real_),
  data.frame(analysis = "4_size_continuous", mv_R2 = a4$mv_R2, mv_p = a4$mv_p, permdisp_p = NA_real_),
  data.frame(analysis = "4_size_bins",       mv_R2 = a4b$R2,   mv_p = a4b$p,   permdisp_p = a4b$disp_p)
) |> arrange(desc(mv_R2))
cat("\nMultivariate R2 ranking (site-level; largest = most structured; all exploratory):\n")
print(mv_head, row.names = FALSE)

# ---- combined marginal model 'who structures technique most?' (site-level) ----
cross_dir <- file.path(out_root, "cross_analysis")
dir.create(cross_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(c(guardrails, "",
  "COMBINED MARGINAL MODEL: Basin, Height above river, Distance and Size are",
  "INTERCORRELATED (Site_size tracks Basin; height and distance both index",
  "position within the terrace staircase). Height above river is by construction",
  "free of basin base level, but the model is still exploratory: marginal R2 only",
  "ranks RELATIVE structure; do not read the terms as independent effects."),
  file.path(cross_dir, "_COLLINEARITY_WARNING.txt"))
combo <- tryCatch(
  adonis2(d2 ~ Basin + Height_above_river + Distance_to_water + Site_size,
          data = site_df, by = "margin", permutations = 999),
  error = function(e) { message("combined marginal model failed (rank-deficient): ",
                                conditionMessage(e)); NULL })
if (!is.null(combo)) {
  cat("\nCombined marginal PERMANOVA (site-level; exploratory; strong collinearity):\n"); print(combo)
}

# collinearity panel: site-level predictor correlations + basin association
num_pred <- site_df |>
  transmute(Distance_to_water, Site_size, Height_above_river, Basin_num = as.integer(Basin))
pred_cor <- cor(num_pred, use = "pairwise.complete.obs", method = "spearman")
cat("\nSite-level predictor Spearman matrix (collinearity check):\n"); print(round(pred_cor, 3))
cat("Note: Height_above_river vs Basin_num should be ~0 -- that is the point of using\n",
    "height above the local channel rather than absolute elevation.\n", sep = "")
# Cramer's V for Basin x height band (site-level)
bl <- table(site_df$Basin, site_df$H_bin)
chi <- suppressWarnings(chisq.test(bl))
cramers_v <- sqrt(as.numeric(chi$statistic) / (sum(bl) * (min(dim(bl)) - 1)))
cat(sprintf("Basin x height-band association (site-level): Cramer's V = %.2f\n", cramers_v))

cat("\n########## DONE. Site-level outputs under ", out_root,
    " (analysis1_basin/ ... analysis4_size/, cross_analysis/). ##########\n", sep = "")
