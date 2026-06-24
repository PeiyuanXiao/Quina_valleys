## QV_landscape_triage.R
## ============================================================================
## EXPLORATORY landscape triage of SURFACE-COLLECTED (SC) Quina scrapers.
## Question: are the 6 technical-consistency metrics structured by landscape?
##   Analysis 1) Basin (Binchuan vs Heqing)            -- categorical
##   Analysis 2) Landform / geomorphic position        -- categorical
##   Analysis 3) Distance to nearest river (d_river_m) -- continuous gradient
##   Analysis 4) Site size (per-site SC count)         -- continuous gradient
##
## Framework reused verbatim from QV_analysis.R (SC-Longtan comparison):
##   z-score -> Euclidean -> adonis2 (PERMANOVA) + betadisper (PERMDISP);
##   prcomp PCA with convex hull / centroid / spoke; per-variable test routing
##     {Ave_GIUR, N_Scar, Ave_RG}  = Kruskal-Wallis + Dunn (Bonferroni)
##     {Retouch_length_index, Thickness, Edge_Angle} = Welch ANOVA + Welch t (Bonf).
##   Same minimal-grey ggplot theme and palette idiom.
##
## DATA (checked, not assumed -- see schema section below):
##   Artifact-level technical data : Quina_scraper_surface.xlsx, sheet "Quina scraper"
##   Site-level landscape data     : Site_information.xlsx   (NOT the 27-clean csv)
##   Join key                      : artifact Site_ID  <->  site Code
##
## ----------------------------------------------------------------------------
## INTERPRETATION GUARDRAILS  (read before citing ANY number this script prints)
##  * EXPLORATORY, not confirmatory. Small / unbalanced N + site-level
##    pseudoreplication => treat every p-value as exploratory / descriptive.
##    Rank evidence by EFFECT SIZE (R^2 / rho / group median spread), NOT p<0.05.
##  * A significant PERMANOVA with a SMALL R^2 = heavily OVERLAPPING groups.
##    Always read it next to PERMDISP: if within-group dispersion differs, the
##    "difference" is mostly spread, not a shift in centroid location.
##  * Surface palimpsest + unbalanced groups => PERMDISP is reported every time.
##  * Artifacts at one site share that site's SINGLE landscape value (distance,
##    size). Distance/size per-variable tests are therefore run at TWO levels:
##      - site-level  (cleaner inference, n = number of sites)
##      - artifact-level (pseudoreplicated, exploratory only).
##    The strictly correct model is a site random-effect mixed model (lme4);
##    the two-level contrast is the package-free stand-in.
##  * SC only, ONE retouch tool-class (not a whole assemblage): any pattern here
##    is the LANDSCAPE DISTRIBUTION OF REDUCTION INTENSITY, NOT evidence of a
##    provisioning / curation system.
## ============================================================================

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

## ---- constants -------------------------------------------------------------
variables <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
               "N_Scar", "Ave_RG", "Edge_Angle")
kw_vars    <- c("Ave_GIUR", "N_Scar", "Ave_RG")                 # KW + Dunn
welch_vars <- c("Retouch_length_index", "Thickness", "Edge_Angle")  # Welch ANOVA + t

proj_dir   <- "H:/Quina_valleys"
sc_path    <- file.path(proj_dir, "Quina_scraper_surface.xlsx")
site_path  <- file.path(proj_dir, "Site_information.xlsx")
lt_path    <- file.path(proj_dir, "Longtan_lithic_tools.xlsx")
out_root   <- file.path(proj_dir, "outputs")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

guardrails <- c(
  "INTERPRETATION GUARDRAILS (exploratory landscape triage, SC Quina scrapers)",
  "* Exploratory, not confirmatory. Small/unbalanced N + site pseudoreplication",
  "  -> read p as exploratory; rank by effect size (R^2 / rho / median spread).",
  "* Significant PERMANOVA + small R^2 = overlapping groups; read with PERMDISP",
  "  (if dispersion differs, the difference is spread, not centroid location).",
  "* Distance/size: artifacts share their site's single value -> site-level result",
  "  is the cleaner inference; artifact-level is pseudoreplicated/exploratory.",
  "* SC only, one retouch tool-class: this is the landscape distribution of",
  "  reduction intensity, NOT evidence of a provisioning system."
)

## ---- shared visual style (matches QV_analysis.R) ---------------------------
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

## categorical palettes (muted, low-saturation, in the QV idiom)
basin_colors    <- c(Binchuan = "#C9603F", Heqing = "#3F7CAC")
landform_colors <- c(T2 = "#6BA8CE", T3 = "#7FB069",
                     T4 = "#E6C25C", hilltop = "#E07C90")
sizebin_colors  <- c(`1` = "#C9D6DF", `2-3` = "#6BA8CE", `4+` = "#2C5F7C")
## overlay: both basins reddish (= SC) vs Longtan yellow/blue (matches QV_analysis)
overlay_colors  <- c(Binchuan = "#E07C90", Heqing = "#B23A52",
                     LT_Quina = "#E6C25C", LT_Ordinary = "#6BA8CE")

fmt_p <- function(p) ifelse(is.na(p), "NA",
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3))))

## ---- PCA helpers (identical drawing logic to QV_analysis.R) ----------------
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

## prcomp PCA on a z-scored matrix + ordination plot; writes scores/loadings/png.
run_pca_plot <- function(mat, groups, colors, outdir, prefix, title, subtitle) {
  pca <- prcomp(mat, center = TRUE, scale. = FALSE)
  vexp <- pca$sdev^2 / sum(pca$sdev^2) * 100
  scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                       Group = factor(groups, levels = names(colors)))
  loadings <- data.frame(Variable = rownames(pca$rotation),
                         PC1 = pca$rotation[, 1], PC2 = pca$rotation[, 2],
                         row.names = NULL)
  write.csv(scores,   file.path(outdir, paste0(prefix, "_pca_scores.csv")),   row.names = FALSE)
  write.csv(loadings, file.path(outdir, paste0(prefix, "_pca_loadings.csv")), row.names = FALSE)

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
    geom_point(size = 1.85, alpha = 0.8, shape = 16) +
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

  ## loadings bar (same look as QV_analysis.R)
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
    labs(subtitle = "PCA variable loadings", x = "Loading", y = NULL, fill = NULL) +
    ordination_theme + theme(panel.grid.major.y = element_blank(), legend.position = "top")
  ggsave(file.path(outdir, paste0(prefix, "_pca_loadings.png")), pl,
         width = 7.6, height = 4.6, dpi = 300)

  list(scores = scores, loadings = loadings, var_explained = vexp)
}

## ---- per-variable categorical engine (KW+Dunn / Welch ANOVA+t) -------------
## dd must contain a factor column `Grp` plus the 6 variable columns.
per_variable_tests <- function(dd, colors, outdir, prefix) {
  long <- dd |>
    select(Grp, all_of(variables)) |>
    pivot_longer(all_of(variables), names_to = "Variable", values_to = "Value") |>
    mutate(Variable = factor(Variable, levels = variables))

  kw_long <- long |> filter(Variable %in% kw_vars)    |> mutate(Variable = droplevels(Variable))
  we_long <- long |> filter(Variable %in% welch_vars) |> mutate(Variable = droplevels(Variable))

  kw_omn <- kw_long |> group_by(Variable) |> kruskal_test(Value ~ Grp) |> ungroup()
  kw_ph  <- kw_long |> group_by(Variable) |>
    dunn_test(Value ~ Grp, p.adjust.method = "bonferroni") |> ungroup()
  we_omn <- we_long |> group_by(Variable) |> welch_anova_test(Value ~ Grp) |> ungroup()
  we_ph  <- we_long |> group_by(Variable) |>
    pairwise_t_test(Value ~ Grp, pool.sd = FALSE, p.adjust.method = "bonferroni") |> ungroup()

  write.csv(kw_omn, file.path(outdir, paste0(prefix, "_kruskal_omnibus.csv")),        row.names = FALSE)
  write.csv(kw_ph,  file.path(outdir, paste0(prefix, "_dunn_posthoc_bonf.csv")),       row.names = FALSE)
  write.csv(we_omn, file.path(outdir, paste0(prefix, "_welch_anova_omnibus.csv")),     row.names = FALSE)
  write.csv(we_ph,  file.path(outdir, paste0(prefix, "_welch_pairwise_t_bonf.csv")),   row.names = FALSE)

  ## group medians (direction) + median spread (effect magnitude)
  meds <- long |> group_by(Variable, Grp) |>
    summarise(median = median(Value, na.rm = TRUE), .groups = "drop")
  write.csv(meds, file.path(outdir, paste0(prefix, "_group_medians.csv")), row.names = FALSE)
  eff <- meds |> group_by(Variable) |>
    summarise(median_spread = max(median) - min(median), .groups = "drop")
  omn_p <- bind_rows(
    kw_omn |> transmute(Variable = as.character(Variable), omnibus_p = p),
    we_omn |> transmute(Variable = as.character(Variable), omnibus_p = p))
  eff <- eff |> mutate(Variable = as.character(Variable)) |>
    left_join(omn_p, by = "Variable")

  ## boxplot with Bonferroni post-hoc brackets (free_y; per-facet positions)
  brackets <- bind_rows(
    kw_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif),
    we_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif)) |>
    mutate(Variable = factor(as.character(Variable), levels = variables))
  ranges <- long |> group_by(Variable) |>
    summarise(ymax = max(Value, na.rm = TRUE),
              yrange = diff(range(Value, na.rm = TRUE)), .groups = "drop")
  brackets <- brackets |> group_by(Variable) |> mutate(step = row_number()) |> ungroup() |>
    left_join(ranges, by = "Variable") |>
    mutate(y.position = ymax + yrange * (0.06 + 0.10 * step))

  bx <- ggplot(long, aes(Grp, Value)) +
    geom_jitter(aes(color = Grp), width = 0.31, height = 0, size = 1.4, alpha = 0.6, shape = 16) +
    geom_boxplot(color = "black", fill = NA, width = 0.62, linewidth = 0.6, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "black") +
    ggpubr::stat_pvalue_manual(brackets, label = "p.adj.signif", y.position = "y.position",
                               tip.length = 0.012, bracket.size = 0.4, label.size = 3,
                               color = "#202124") +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    scale_color_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(subtitle = "Pairwise post-hoc, Bonferroni-adjusted (ns / * / ** / *** / ****)",
         x = NULL, y = NULL) +
    ordination_theme +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")
  ggsave(file.path(outdir, paste0(prefix, "_variable_boxplots.png")), bx,
         width = 8.4, height = 6.8, dpi = 300)

  eff
}

## ---- categorical analysis driver (PERMANOVA + PERMDISP + PCA + per-var) -----
run_categorical <- function(dat, group_col, group_levels, colors, outdir, prefix,
                            title, perm = 999) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  writeLines(guardrails, file.path(outdir, "_GUARDRAILS.txt"))

  dd <- dat |>
    filter(!is.na(.data[[group_col]])) |>
    mutate(Grp = factor(as.character(.data[[group_col]]), levels = group_levels)) |>
    filter(!is.na(Grp))
  dd <- dd |> filter(if_all(all_of(variables), ~ !is.na(.x)))

  cat("\n==== ", title, " ====\n", sep = "")
  cat("N per group (complete-case on 6 variables):\n"); print(table(dd$Grp))
  cat("Total N =", nrow(dd), "\n")

  mat <- scale(as.matrix(dd[, variables]))
  d   <- dist(mat, method = "euclidean")

  ad <- adonis2(d ~ Grp, data = dd, permutations = perm)
  write.csv(as.data.frame(ad), file.path(outdir, paste0(prefix, "_permanova.csv")))
  cat("\nPERMANOVA (adonis2):\n"); print(ad)

  bd <- betadisper(d, dd$Grp)
  pt <- permutest(bd, permutations = perm, pairwise = TRUE)
  write.csv(as.data.frame(pt$tab), file.path(outdir, paste0(prefix, "_permdisp.csv")))
  cat("\nPERMDISP (betadisper + permutest):\n"); print(pt$tab)

  sub <- sprintf("PERMANOVA R2 = %.3f, p %s  |  PERMDISP p %s",
                 ad$R2[1], fmt_p(ad$`Pr(>F)`[1]), fmt_p(pt$tab$`Pr(>F)`[1]))
  pca <- run_pca_plot(mat, as.character(dd$Grp), colors, outdir, prefix, title, sub)
  eff <- per_variable_tests(dd, colors, outdir, prefix)

  list(R2 = ad$R2[1], F = ad$F[1], p = ad$`Pr(>F)`[1],
       disp_p = pt$tab$`Pr(>F)`[1], eff = eff, data = dd, mat = mat, dist = d,
       pca = pca)
}

## ---- pairwise PERMANOVA on one z-scored matrix (same scope) -----------------
pairwise_adonis <- function(mat, groups, perm = 999) {
  groups <- factor(groups)
  combos <- combn(levels(groups), 2, simplify = FALSE)
  do.call(rbind, lapply(combos, function(pr) {
    keep <- groups %in% pr
    g <- droplevels(groups[keep])
    m <- adonis2(dist(mat[keep, , drop = FALSE]) ~ g,
                 data = data.frame(g = g), permutations = perm)
    data.frame(Comparison = paste(pr, collapse = " vs "),
               R2 = m$R2[1], F = m$F[1], p_value = m$`Pr(>F)`[1])
  }))
}

## weighted Spearman = weighted Pearson on ranks (base R; no extra packages)
weighted_spearman <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w)
  x <- rank(x[ok]); y <- rank(y[ok]); w <- w[ok]
  mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w)
  num <- sum(w * (x - mx) * (y - my))
  den <- sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
  if (den == 0) NA_real_ else num / den
}

## ---- continuous-gradient analysis driver (two-level: site & artifact) ------
run_gradient <- function(dat, grad_col, grad_label, outdir, prefix, perm = 999) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  writeLines(guardrails, file.path(outdir, "_GUARDRAILS.txt"))

  dd <- dat |>
    filter(!is.na(.data[[grad_col]])) |>
    filter(if_all(all_of(variables), ~ !is.na(.x))) |>
    mutate(grad = as.numeric(.data[[grad_col]]))

  cat("\n==== Technical metrics ~ ", grad_label, " (gradient) ====\n", sep = "")
  cat("Artifact-level N =", nrow(dd), " | sites =", dplyr::n_distinct(dd$Site_ID), "\n")

  mat <- scale(as.matrix(dd[, variables]))
  d   <- dist(mat, method = "euclidean")

  ## ---- multivariate (ARTIFACT-LEVEL; pseudoreplicated, exploratory) ----
  df_grad <- data.frame(grad = dd$grad)
  ad <- adonis2(d ~ grad, data = df_grad, permutations = perm)
  write.csv(as.data.frame(ad), file.path(outdir, paste0(prefix, "_adonis2_gradient.csv")))
  cap_R2 <- NA_real_; cap_p <- NA_real_
  cap_ok <- tryCatch({
    cap   <- vegan::capscale(d ~ grad, data = df_grad)
    cap_a <- anova(cap, permutations = perm)        # anova.cca
    write.csv(as.data.frame(cap_a), file.path(outdir, paste0(prefix, "_capscale_anova.csv")))
    cap_R2 <- cap$CCA$tot.chi / cap$tot.chi
    cap_p  <- cap_a$`Pr(>F)`[1]
    TRUE
  }, error = function(e) { message("  capscale failed: ", conditionMessage(e)); FALSE })
  writeLines(c(guardrails, "",
               "NOTE: the multivariate db-RDA/PERMANOVA above is ARTIFACT-LEVEL and",
               "pseudoreplicated (artifacts inherit their site's single gradient value).",
               "Treat R^2/p as exploratory; the site-level Spearman is the cleaner read."),
             file.path(outdir, paste0(prefix, "_MULTIVARIATE_NOTE.txt")))
  cat(sprintf("\nadonis2(dist ~ %s): R2 = %.3f, p %s  [artifact-level, pseudoreplicated]\n",
              grad_label, ad$R2[1], fmt_p(ad$`Pr(>F)`[1])))

  ## ---- per-variable, ARTIFACT-LEVEL Spearman (exploratory) ----
  art <- do.call(rbind, lapply(variables, function(v) {
    ct <- suppressWarnings(cor.test(dd[[v]], dd$grad, method = "spearman", exact = FALSE))
    data.frame(Variable = v, n = nrow(dd),
               rho_artifact = unname(ct$estimate), p_artifact = ct$p.value)
  }))

  ## ---- per-variable, SITE-LEVEL Spearman (cleaner inference) ----
  site_sum <- dd |>
    group_by(Site_ID) |>
    summarise(n = n(), grad = dplyr::first(grad),
              across(all_of(variables), ~ median(.x, na.rm = TRUE)), .groups = "drop")
  write.csv(site_sum, file.path(outdir, paste0(prefix, "_site_summary_medians.csv")), row.names = FALSE)
  cat("Site-level summary: n_sites =", nrow(site_sum),
      "| sites with n<3 artifacts:", sum(site_sum$n < 3), "(aggregation unstable)\n")

  sit <- do.call(rbind, lapply(variables, function(v) {
    ct <- suppressWarnings(cor.test(site_sum[[v]], site_sum$grad, method = "spearman", exact = FALSE))
    data.frame(Variable = v, n_sites = nrow(site_sum),
               rho_site = unname(ct$estimate), p_site = ct$p.value,
               rho_site_wtd = weighted_spearman(site_sum[[v]], site_sum$grad, site_sum$n))
  }))

  per_var <- art |> left_join(sit, by = "Variable")
  write.csv(per_var, file.path(outdir, paste0(prefix, "_per_variable_spearman.csv")), row.names = FALSE)
  cat("\nPer-variable Spearman (site-level = cleaner; artifact-level = exploratory):\n")
  print(per_var, row.names = FALSE)

  ## ---- faceted scatter (artifact points + lm trend + site medians overlaid) ----
  art_long <- dd |> select(Site_ID, grad, all_of(variables)) |>
    pivot_longer(all_of(variables), names_to = "Variable", values_to = "Value") |>
    mutate(Variable = factor(Variable, levels = variables))
  site_long <- site_sum |> select(grad, all_of(variables)) |>
    pivot_longer(all_of(variables), names_to = "Variable", values_to = "Median") |>
    mutate(Variable = factor(Variable, levels = variables))
  labs_df <- per_var |>
    mutate(Variable = factor(Variable, levels = variables),
           label = sprintf("rho_site = %.2f (p %s)\nrho_art = %.2f (p %s)",
                           rho_site, fmt_p(p_site), rho_artifact, fmt_p(p_artifact)))
  sc_p <- ggplot(art_long, aes(grad, Value)) +
    geom_point(color = "#303238", alpha = 0.45, size = 1.5, shape = 16) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#6BA8CE", fill = "#9BC7DF", linewidth = 0.8) +
    geom_point(data = site_long, aes(grad, Median), color = "#C9603F",
               size = 2.4, shape = 18, inherit.aes = FALSE) +
    geom_text(data = labs_df, aes(x = -Inf, y = Inf, label = label),
              hjust = -0.06, vjust = 1.15, size = 3.0, color = "#202124",
              lineheight = 0.95, inherit.aes = FALSE) +
    facet_wrap(~ Variable, scales = "free_y", ncol = 3,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    labs(title = paste0("Technical metrics vs ", grad_label),
         subtitle = "grey = artifacts (pseudoreplicated); orange diamond = site median; line = lm",
         x = grad_label, y = NULL) +
    corr_theme
  ggsave(file.path(outdir, paste0(prefix, "_scatter.png")), sc_p,
         width = 9.6, height = 6.2, dpi = 300)

  list(mv_R2 = ad$R2[1], mv_p = ad$`Pr(>F)`[1], cap_R2 = cap_R2, cap_p = cap_p,
       per_var = per_var, site_sum = site_sum, data = dd)
}

## ============================================================================
## LOAD + SCHEMA CHECK + JOIN  (printed BEFORE any statistics are run)
## ============================================================================
cat("\n########## DATA / SCHEMA / JOIN ##########\n")

sc_raw <- read_excel(sc_path, sheet = "Quina scraper")
cat("\nArtifact file columns (Quina scraper):\n"); print(names(sc_raw))
stopifnot(all(variables %in% names(sc_raw)), "Site_ID" %in% names(sc_raw))

sc <- sc_raw |>
  mutate(Site_ID = trimws(as.character(Site_ID)),
         across(all_of(variables), as.numeric))

site_raw <- read_excel(site_path)
names(site_raw) <- trimws(names(site_raw))           # headers carry trailing spaces
cat("\nSite file columns (Site_information.xlsx, trimmed):\n"); print(names(site_raw))
req_site <- c("Code", "basin", "geomorph", "d_river_m")
miss_site <- setdiff(req_site, names(site_raw))
if (length(miss_site) > 0)
  stop("Missing required site-level fields in Site_information.xlsx: ",
       paste(miss_site, collapse = ", "))

site_land <- site_raw |>
  transmute(
    Site_ID  = trimws(as.character(Code)),
    Basin    = factor(sub(" basin$", "", trimws(basin)), levels = c("Binchuan", "Heqing")),
    Landform = factor(trimws(geomorph), levels = c("T2", "T3", "T4", "hilltop")),
    Distance_to_water = as.numeric(d_river_m),
    elev_m   = as.numeric(elev_m),
    n_Quina_scraper_col = as.numeric(n_Quina_scraper)
  )

## Site_size = per-site SC artifact count (preferred), cross-checked vs the column
sc_count <- sc |> count(Site_ID, name = "Site_size")
size_check <- site_land |> left_join(sc_count, by = "Site_ID") |>
  mutate(Site_size = dplyr::coalesce(Site_size, 0L))
mismatch <- size_check |> filter(Site_size != n_Quina_scraper_col)
cat("\nSite_size cross-check (SC count vs n_Quina_scraper column): ",
    nrow(mismatch), " mismatched site(s)\n", sep = "")
if (nrow(mismatch) > 0) print(mismatch |> select(Site_ID, Site_size, n_Quina_scraper_col))

## left join artifacts -> site landscape (+ Site_size)
sc_join <- sc |>
  left_join(site_land, by = "Site_ID") |>
  left_join(sc_count,  by = "Site_ID")
unmatched <- sc_join |> filter(is.na(Basin))
cat("Artifacts with no matching site row:", nrow(unmatched), "\n")
if (nrow(unmatched) > 0) {
  cat(" -> STOPPING: unmatched Site_IDs: ",
      paste(sort(unique(unmatched$Site_ID)), collapse = ", "), "\n")
  stop("Unmatched artifacts; resolve the join key before proceeding.")
}

## complete-case (6 variables) -- the shared SC analysis frame
sc_cc <- sc_join |> filter(if_all(all_of(variables), ~ !is.na(.x)))
cat("\nSC complete-case N (all 6 variables):", nrow(sc_cc), "of", nrow(sc_join), "\n")
cat("\nN by Basin:\n");    print(table(sc_cc$Basin))
cat("\nN by Landform:\n"); print(table(sc_cc$Landform))
cat("\nBasin x Landform contingency (artifact-level; small cells unreliable):\n")
print(table(sc_cc$Basin, sc_cc$Landform))
cat("\nBasin x Landform contingency (site-level):\n")
print(table(site_land$Basin, site_land$Landform))

## accumulator for the cross-analysis triage summary
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
    effect_secondary = pv$rho_artifact, effect_secondary_type = "rho_artifact",
    p_secondary = pv$p_artifact, stringsAsFactors = FALSE)
}

## ============================================================================
## ANALYSIS 1 -- BASIN  (Binchuan vs Heqing)
## ============================================================================
a1_dir <- file.path(out_root, "analysis1_basin")
a1 <- run_categorical(sc_cc, "Basin", c("Binchuan", "Heqing"), basin_colors,
                      a1_dir, "a1_basin", "Analysis 1: technical metrics ~ Basin")
push_cat("1_basin", a1)

## ---- 1a. consistency vs SC-Longtan (is Basin R^2 << SC-Longtan R^2 ?) ----
read_grp <- function(path, sheet, g) {
  read_excel(path, sheet = sheet) |>
    mutate(Group = g, across(all_of(variables), as.numeric)) |>
    select(Group, all_of(variables)) |>
    filter(if_all(all_of(variables), ~ !is.na(.x)))
}
lt_q <- read_grp(lt_path, "Quina scraper",    "LT_Quina")
lt_o <- read_grp(lt_path, "Ordinary scraper", "LT_Ordinary")

## reproduce the 3-group SC-Longtan pairwise on its own z-scope (matches QV_analysis)
three <- bind_rows(
  sc_cc |> transmute(Group = "SC_Quina", across(all_of(variables))),
  lt_q, lt_o) |>
  mutate(Group = factor(Group, levels = c("SC_Quina", "LT_Quina", "LT_Ordinary")))
three_mat <- scale(as.matrix(three[, variables]))
three_pw  <- pairwise_adonis(three_mat, three$Group)

consistency <- bind_rows(
  data.frame(Comparison = "Binchuan vs Heqing (SC-only scope)",
             R2 = a1$R2, F = a1$F, p_value = a1$p),
  three_pw |> filter(grepl("SC_Quina", Comparison)) |>
    mutate(Comparison = paste0(Comparison, " (3-group scope)"))
)
## cross-check against the stored QV_analysis post-hoc table if present
stored_pp <- file.path(out_root, "permanova_posthoc_pairwise.csv")
if (file.exists(stored_pp)) {
  sp <- read.csv(stored_pp)
  cat("\nStored QV_analysis pairwise R2 (cross-check):\n")
  print(sp[, intersect(c("Comparison", "R2", "p_value", "p_adjusted"), names(sp))])
}
write.csv(consistency, file.path(a1_dir, "a1_consistency_vs_longtan.csv"), row.names = FALSE)
cat("\nConsistency comparison (Basin R2 vs SC-Longtan R2):\n"); print(consistency, row.names = FALSE)
cat("Reading: if Binchuan-Heqing R2 is far SMALLER than SC_Quina-LT_* R2,\n",
    "the two basins are technically close => supports regional consistency.\n")

## ---- 1b. overlay PCA on 4 groups (basins split + the two Longtan groups) ----
overlay <- bind_rows(
  sc_cc |> transmute(Group = as.character(Basin), across(all_of(variables))),
  lt_q, lt_o) |>
  mutate(Group = factor(Group, levels = c("Binchuan", "Heqing", "LT_Quina", "LT_Ordinary"))) |>
  filter(if_all(all_of(variables), ~ !is.na(.x)))
ov_mat <- scale(as.matrix(overlay[, variables]))
cat("\nOverlay (4-group) N:\n"); print(table(overlay$Group))
run_pca_plot(ov_mat, as.character(overlay$Group), overlay_colors, a1_dir, "a1_overlay",
             "Analysis 1 overlay: basins vs Longtan (shared z-score)",
             "Do the basins overlap each other & LT_Quina, yet separate from LT_Ordinary?")
ov_pw <- pairwise_adonis(ov_mat, overlay$Group)
write.csv(ov_pw, file.path(a1_dir, "a1_overlay_pairwise_permanova.csv"), row.names = FALSE)
cat("\nOverlay 4-group pairwise PERMANOVA (one shared z-scope = cleanest comparison):\n")
print(ov_pw, row.names = FALSE)

## ---- 1c. loadings / driver comparison (this basin PCA vs SC-Longtan PCA) ----
load_cmp <- a1$pca$loadings |>
  rename(basin_PC1 = PC1, basin_PC2 = PC2)
stored_load <- file.path(out_root, "pca_loadings.csv")
if (file.exists(stored_load)) {
  sl <- read.csv(stored_load) |> rename(longtan_PC1 = PC1, longtan_PC2 = PC2)
  load_cmp <- load_cmp |> left_join(sl, by = "Variable")
}
write.csv(load_cmp, file.path(a1_dir, "a1_loadings_comparison.csv"), row.names = FALSE)
cat("\nLoadings comparison (same variables driving both analyses?):\n"); print(load_cmp, row.names = FALSE)

## ---- 1d. composition diagnostic: is "Heqing" really "Tianhua/Longtan area"? ----
heq <- sc_cc |> filter(Basin == "Heqing")
heq_by_site <- heq |> count(Site_ID, name = "n_SC") |> arrange(desc(n_SC))
prox <- c("THC", "LT")                                   # Tianhua Cave, Longtan
n_prox <- sum(heq$Site_ID %in% prox)
frac_prox <- n_prox / nrow(heq)
write.csv(heq_by_site, file.path(a1_dir, "a1_heqing_composition.csv"), row.names = FALSE)
cat(sprintf("\nComposition diagnostic: Heqing SC n = %d; from THC/LT = %d (%.0f%%).\n",
            nrow(heq), n_prox, 100 * frac_prox))
if (frac_prox >= 0.4) {
  cat("WARNING: 'Heqing basin' is dominated by Tianhua/Longtan-proximal surface finds.\n",
      "  => here 'Heqing basin' ~= 'Tianhua/Longtan area'; Basin and site are CONFOUNDED.\n")
}
## sensitivity: drop THC/LT from Heqing, re-run the basin contrast
sens_data <- sc_cc |> filter(!(Basin == "Heqing" & Site_ID %in% prox))
a1_sens <- run_categorical(sens_data, "Basin", c("Binchuan", "Heqing"), basin_colors,
                           a1_dir, "a1_basin_sensitivity",
                           "Analysis 1 sensitivity: Basin (Heqing excl. THC/LT)")
sens_tbl <- data.frame(
  Model = c("Basin (all SC)", "Basin (Heqing excl. THC/LT)"),
  R2 = c(a1$R2, a1_sens$R2), p_value = c(a1$p, a1_sens$p),
  PERMDISP_p = c(a1$disp_p, a1_sens$disp_p))
write.csv(sens_tbl, file.path(a1_dir, "a1_basin_sensitivity_summary.csv"), row.names = FALSE)
cat("\nBasin sensitivity (with vs without THC/LT in Heqing):\n"); print(sens_tbl, row.names = FALSE)

## ============================================================================
## ANALYSIS 2 -- LANDFORM (geomorphic position; categorical)
## ============================================================================
a2_dir <- file.path(out_root, "analysis2_landform")
a2 <- run_categorical(sc_cc, "Landform", c("T2", "T3", "T4", "hilltop"), landform_colors,
                      a2_dir, "a2_landform", "Analysis 2: technical metrics ~ Landform")
push_cat("2_landform", a2)

## ---- 2a. collinearity with Basin: marginal (Type-III-like) PERMANOVA ----
mat2 <- scale(as.matrix(sc_cc[, variables]))
d2   <- dist(mat2, method = "euclidean")
margin_BL <- adonis2(d2 ~ Basin + Landform, data = sc_cc, by = "margin", permutations = 999)
write.csv(as.data.frame(margin_BL), file.path(a2_dir, "a2_margin_basin_landform.csv"))
cat("\nMarginal PERMANOVA dist ~ Basin + Landform (by='margin'):\n"); print(margin_BL)
cat("Reading: Landform's marginal R2/p = its contribution AFTER Basin is controlled.\n")
ct <- as.data.frame.matrix(table(sc_cc$Basin, sc_cc$Landform))
write.csv(ct, file.path(a2_dir, "a2_basin_landform_contingency.csv"))
cat("\nBasin x Landform (flag small cells: T2 Heqing-only; hilltop/T4 Binchuan-only):\n")
print(ct)

## ============================================================================
## ANALYSIS 3 -- DISTANCE TO WATER (continuous gradient)
## ============================================================================
a3_dir <- file.path(out_root, "analysis3_distance")
a3 <- run_gradient(sc_cc, "Distance_to_water", "Distance to river (m)",
                   a3_dir, "a3_distance")
push_grad("3_distance", a3)

## ============================================================================
## ANALYSIS 4 -- SITE SIZE (continuous gradient) + bin robustness
## ============================================================================
a4_dir <- file.path(out_root, "analysis4_size")
a4 <- run_gradient(sc_cc, "Site_size", "Site size (SC artifact count)",
                   a4_dir, "a4_size")
push_grad("4_size_continuous", a4)
writeLines(c(guardrails, "",
  "ANALYSIS 4 SPECIFIC CAVEAT:",
  "Site_size has a tiny dynamic range (Binchuan median ~1-2) and the RESPONSE",
  "(retouch intensity) may SHARE COLLECTION BIAS with the PREDICTOR (size): more",
  "intensively collected sites yield both more pieces AND more retouched pieces.",
  "A correlation here can be a collection artefact, possibly even sign-reversed."),
  file.path(a4_dir, "a4_size_CAVEAT.txt"))

## bin robustness: does a continuous trend survive coarse binning, or is it
## dragged by a few large sites?
sc_bin <- sc_cc |>
  mutate(Size_bin = cut(Site_size, breaks = c(-Inf, 1, 3, Inf),
                        labels = c("1", "2-3", "4+")))
cat("\nSite_size bins (artifact-level N):\n"); print(table(sc_bin$Size_bin))
a4b <- run_categorical(sc_bin, "Size_bin", c("1", "2-3", "4+"), sizebin_colors,
                       a4_dir, "a4_sizebin",
                       "Analysis 4 robustness: technical metrics ~ size bin")
push_cat("4_size_bins", a4b)

## ============================================================================
## CROSS-ANALYSIS TRIAGE SUMMARY
## ============================================================================
summary_tbl <- bind_rows(summary_rows)
write.csv(summary_tbl, file.path(out_root, "summary_effect_sizes.csv"), row.names = FALSE)
cat("\n########## CROSS-ANALYSIS TRIAGE SUMMARY (rank by effect size) ##########\n")
print(summary_tbl, row.names = FALSE)

## per-analysis multivariate headline (which grouping/gradient structures most?)
mv_head <- bind_rows(
  data.frame(analysis = "1_basin",          mv_R2 = a1$R2,    mv_p = a1$p,    permdisp_p = a1$disp_p),
  data.frame(analysis = "2_landform",       mv_R2 = a2$R2,    mv_p = a2$p,    permdisp_p = a2$disp_p),
  data.frame(analysis = "3_distance",       mv_R2 = a3$mv_R2, mv_p = a3$mv_p, permdisp_p = NA_real_),
  data.frame(analysis = "4_size_continuous",mv_R2 = a4$mv_R2, mv_p = a4$mv_p, permdisp_p = NA_real_),
  data.frame(analysis = "4_size_bins",      mv_R2 = a4b$R2,   mv_p = a4b$p,   permdisp_p = a4b$disp_p)
) |> arrange(desc(mv_R2))
write.csv(mv_head, file.path(out_root, "summary_multivariate_R2.csv"), row.names = FALSE)
cat("\nMultivariate R2 ranking (largest = most structured; all exploratory):\n")
print(mv_head, row.names = FALSE)

## ---- optional: combined marginal model 'who structures technique most?' ----
cross_dir <- file.path(out_root, "cross_analysis")
dir.create(cross_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(c(guardrails, "",
  "COMBINED MARGINAL MODEL: Basin, Landform, Distance, Size are STRONGLY COLLINEAR",
  "(basin ~ landform ~ distance ~ elevation). Marginal R2 only ranks RELATIVE",
  "structure; do not read the terms as independent effects."),
  file.path(cross_dir, "_COLLINEARITY_WARNING.txt"))
combo <- adonis2(d2 ~ Basin + Landform + Distance_to_water + Site_size,
                 data = sc_cc, by = "margin", permutations = 999)
write.csv(as.data.frame(combo), file.path(cross_dir, "combined_margin_permanova.csv"))
cat("\nCombined marginal PERMANOVA (exploratory; strong collinearity):\n"); print(combo)

## collinearity panel: site-level predictor correlations + basin association
site_pred <- size_check |>
  filter(Site_ID %in% unique(sc_cc$Site_ID)) |>
  mutate(Basin_num = as.integer(Basin))
num_pred <- site_pred |> select(Distance_to_water, Site_size, elev_m, Basin_num)
pred_cor <- cor(num_pred, use = "pairwise.complete.obs", method = "spearman")
write.csv(round(pred_cor, 3), file.path(cross_dir, "predictor_spearman_matrix.csv"))
cat("\nSite-level predictor Spearman matrix (collinearity check):\n"); print(round(pred_cor, 3))
## Cramer's V for Basin x Landform (site-level)
bl <- table(site_pred$Basin, site_pred$Landform)
chi <- suppressWarnings(chisq.test(bl))
cramers_v <- sqrt(as.numeric(chi$statistic) / (sum(bl) * (min(dim(bl)) - 1)))
cat(sprintf("Basin x Landform association (site-level): Cramer's V = %.2f\n", cramers_v))

cat("\n########## DONE. Outputs under ", out_root,
    " (analysis1_basin/ ... analysis4_size/, cross_analysis/, summary_effect_sizes.csv) ##########\n", sep = "")
