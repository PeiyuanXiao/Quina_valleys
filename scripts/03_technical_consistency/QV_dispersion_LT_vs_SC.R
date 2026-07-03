## QV_dispersion_LT_vs_SC.R
## ============================================================================
## EXPLORATORY dispersion (variability) comparison: SC vs Longtan Quina scrapers.
## Question: is the technical VARIABILITY of surface-collected (SC) Quina scrapers
## larger / different from excavated Longtan (LT) Quina scrapers?
##
##   Block A  multivariate dispersion (PERMDISP, betadisper/permutest)
##   Block B  per-variable dispersion, two families
##              B1 CV family (ratio/dimensional) : Length, Width, Thickness, Mass, Edge_Angle
##              B2 robust family (reduction)     : Ave_GIUR, Retouch_length_index, N_Scar, Ave_RG
##   Block C  independence sensitivity (drop SC pieces proximal to LT/THC)
##   Block D  cross-variable summary
##
## Groups:
##   SC_Quina    = Quina_scraper_surface.xlsx  :: "Quina scraper"   (focus)
##   LT_Quina    = Longtan_lithic_tools.xlsx   :: "Quina scraper"   (focus comparator)
##   LT_Ordinary = Longtan_lithic_tools.xlsx   :: "Ordinary scraper"(yardstick only)
## FOCUS contrast = SC_Quina vs LT_Quina (same tool-class, different burial/time-
## averaging). LT_Ordinary is only a yardstick (how far apart can two *real* tool
## classes from the same site be?).
##
## ----------------------------------------------------------------------------
## INTERPRETATION GUARDRAILS (read before citing ANY number below)
##  * EXPLORATORY. Do NOT quantify "how much time"; do NOT read dispersion as
##    transmission fidelity. Rank by EFFECT SIZE (dispersion ratio / mean distance
##    to centroid), NOT by p < 0.05.
##  * Dispersion depends on the mean: every dispersion stat is reported NEXT TO the
##    group mean. If SC and LT_Quina means differ, a "spread difference" is
##    confounded with a location difference. (Prior result: SC approx= LT_Quina,
##    PERMANOVA R^2 = 0.003 -> expect small; but verify per variable here.)
##  * CV is only valid for ratio/dimensional variables (+Edge_Angle by convention).
##    Bounded indices / counts use logit / Fano + robust spread + Fligner instead.
##  * Reduction-indicator dispersion = the range of REDUCTION STAGES sampled.
##    SC wider than LT is consistent with stronger time-averaging, but is NOT
##    evidence of transmission fidelity (a curated single tool-class has variance
##    dominated by reduction stage).
##  * Pooling 26 SC sites inflates variance, BUT prior homogeneity (between-basin
##    R^2 = 0.003, PERMDISP between-basin p = 0.77) shows near-zero between-site
##    location differences, so pooling's variance impact is limited. Weathering
##    adds non-behavioural measurement spread to surface pieces (unavoidable) --
##    see Block C sensitivity.
## ============================================================================

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan", "rstatix")
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

has_cvequality <- requireNamespace("cvequality", quietly = TRUE)  # optional cross-check; never install

set.seed(123)
B_BOOT <- 5000   # bootstrap / permutation replicates (>= 5000 as specified)

## ---- paths -----------------------------------------------------------------
proj_dir <- "H:/Quina_valleys"
sc_path  <- file.path(proj_dir, "data", "Quina_scraper_surface.xlsx")
lt_path  <- file.path(proj_dir, "data", "Longtan_lithic_tools.xlsx")
out_root <- file.path(proj_dir, "output", "03_technical_consistency")
base_dir <- file.path(out_root, "dispersion_LT_vs_SC")
sub <- list(mv  = file.path(base_dir, "multivariate_permdisp"),
            cv  = file.path(base_dir, "cv_dimensional"),
            rob = file.path(base_dir, "robust_reduction"),
            sen = file.path(base_dir, "sensitivity"))
for (d in c(base_dir, unlist(sub))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## ---- variables / families --------------------------------------------------
cv_vars      <- c("Length", "Width", "Thickness", "Mass", "Edge_Angle")     # CV family
bounded_vars <- c("Ave_GIUR", "Retouch_length_index")                       # [0,1] indices
count_vars   <- c("N_Scar", "Ave_RG")                                       # counts / count-like
robust_vars  <- c(bounded_vars, count_vars)
need_vars    <- c(cv_vars, robust_vars)
tech6        <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
                  "N_Scar", "Ave_RG", "Edge_Angle")  # the established technical space (Block A)
grp_levels   <- c("SC_Quina", "LT_Quina", "LT_Ordinary")

group_colors <- c(SC_Quina = "#E07C90", LT_Quina = "#E6C25C", LT_Ordinary = "#6BA8CE")

guardrails <- c(
  "DISPERSION (variability) comparison SC vs Longtan Quina scrapers -- EXPLORATORY.",
  "* Rank by effect size (dispersion ratio / mean distance to centroid), not p<0.05.",
  "* Dispersion depends on the mean -> every spread stat is reported next to the mean;",
  "  if SC and LT_Quina means differ, 'spread difference' is confounded with location.",
  "* CV only for ratio/dimensional vars (+Edge_Angle by convention; interval scale).",
  "  Bounded [0,1] indices -> empirical-logit; counts -> Fano + sqrt scale; robust spread+Fligner.",
  "* Reduction-indicator dispersion = range of reduction stages sampled; SC>LT is",
  "  consistent with time-averaging but is NOT transmission-fidelity evidence.",
  "* Focus = SC_Quina vs LT_Quina; LT_Ordinary is a yardstick only.",
  "* Surface weathering adds non-behavioural measurement spread (see sensitivity)."
)
writeLines(guardrails, file.path(base_dir, "_GUARDRAILS.txt"))

## ---- shared visual style (QV idiom) ----------------------------------------
ordination_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649", margin = margin(b = 8)),
    plot.caption = element_text(size = 7, color = "#454649", hjust = 0),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "right", legend.title = element_text(face = "bold"), legend.key = element_blank(),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )
corr_theme <- ordination_theme +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.grid.major.x = element_blank())

guard_caption <- "EXPLORATORY: rank by effect size, not p<0.05. Dispersion depends on the mean (reported alongside)."

make_centroids <- function(scores) scores |>
  group_by(Group) |>
  summarise(x_centroid = mean(PC1), y_centroid = mean(PC2), .groups = "drop")
make_hulls <- function(scores) scores |>
  group_by(Group) |> filter(n() >= 3) |>
  group_modify(function(g, k) { h <- chull(g$PC1, g$PC2); bind_rows(g[h, ], g[h[1], ]) }) |>
  ungroup()

## ============================================================================
## STATISTICAL HELPERS (base R; no new packages)
## ============================================================================
finite <- function(x) x[is.finite(x)]
cv_raw    <- function(x) { x <- finite(x); sd(x) / mean(x) }
cv_corr   <- function(x) { x <- finite(x); n <- length(x); (sd(x) / mean(x)) * (1 + 1 / (4 * n)) }  # Sokal-Rohlf
fano      <- function(x) { x <- finite(x); var(x) / mean(x) }
emp_logit <- function(x) qlogis(pmin(pmax(x, 1e-3), 1 - 1e-3))  # 0/1 clamped to (1e-3, 1-1e-3)

## bootstrap percentile CI of a one-sample statistic
boot_stat_ci <- function(x, FUN, B = B_BOOT) {
  x <- finite(x)
  rr <- replicate(B, FUN(sample(x, replace = TRUE)))
  unname(quantile(rr, c(0.025, 0.975), na.rm = TRUE))
}
## bootstrap percentile CI of a ratio FUN(x_sc)/FUN(x_lt); skip if a mean ~ 0
boot_ratio_ci <- function(x_sc, x_lt, FUN, B = B_BOOT, mean_guard = FALSE) {
  x_sc <- finite(x_sc); x_lt <- finite(x_lt)
  if (mean_guard && (abs(mean(x_sc)) < 1e-8 || abs(mean(x_lt)) < 1e-8)) return(c(NA_real_, NA_real_))
  rr <- replicate(B, FUN(sample(x_sc, replace = TRUE)) / FUN(sample(x_lt, replace = TRUE)))
  unname(quantile(rr, c(0.025, 0.975), na.rm = TRUE))
}
## permutation test of CV* equality between two groups (statistic = |dCV*|)
perm_cv_equal <- function(x_sc, x_lt, B = B_BOOT) {
  x_sc <- finite(x_sc); x_lt <- finite(x_lt)
  obs <- abs(cv_corr(x_sc) - cv_corr(x_lt))
  pool <- c(x_sc, x_lt); n1 <- length(x_sc)
  perm <- replicate(B, {
    idx <- sample.int(length(pool))
    abs(cv_corr(pool[idx[seq_len(n1)]]) - cv_corr(pool[idx[(n1 + 1):length(pool)]]))
  })
  (1 + sum(perm >= obs)) / (B + 1)
}
## Fligner-Killeen p for a 2-group contrast
fligner_pair <- function(a, b) {
  a <- finite(a); b <- finite(b)
  g <- factor(rep(c("SC", "LT"), c(length(a), length(b))))
  fligner.test(c(a, b), g)$p.value
}

## ============================================================================
## LOAD + SCHEMA CHECK  (printed BEFORE any statistics)
## ============================================================================
cat("\n########## SCHEMA CHECK / PER-VARIABLE N ##########\n")
read_group <- function(path, sheet, g, has_site) {
  df <- read_excel(path, sheet = sheet)
  miss <- setdiff(need_vars, names(df))
  if (length(miss) > 0)
    stop("Group ", g, " [", sheet, "] is MISSING required columns: ",
         paste(miss, collapse = ", "), "\nAvailable columns: ",
         paste(names(df), collapse = ", "))
  df$Group   <- g
  df$Site_ID <- if (has_site) trimws(as.character(df$Site_ID)) else NA_character_
  df |> mutate(across(all_of(need_vars), as.numeric)) |>
    select(Group, Site_ID, all_of(need_vars))
}
dat <- bind_rows(
  read_group(sc_path, "Quina scraper",    "SC_Quina",    TRUE),
  read_group(lt_path, "Quina scraper",    "LT_Quina",    FALSE),
  read_group(lt_path, "Ordinary scraper", "LT_Ordinary", FALSE)
) |> mutate(Group = factor(Group, levels = grp_levels))

## per-variable per-group complete-case n (each variable on its own complete-case)
n_tbl <- dat |>
  pivot_longer(all_of(need_vars), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |>
  count(Variable, Group) |>
  pivot_wider(names_from = Group, values_from = n, values_fill = 0) |>
  mutate(Variable = factor(Variable, levels = need_vars)) |> arrange(Variable)
cat("\nPer-variable complete-case n by group:\n"); print(as.data.frame(n_tbl), row.names = FALSE)
write.csv(n_tbl, file.path(base_dir, "per_variable_n.csv"), row.names = FALSE)
cat("\ncvequality available for optional Feltz-Miller cross-check:", has_cvequality, "\n")

## focus / proximal-exclusion masks (Block C)
proximal_ids <- c("LT", "THC")
n_prox <- sum(dat$Group == "SC_Quina" & dat$Site_ID %in% proximal_ids)
cat(sprintf("SC_Quina proximal to LT/THC: %d of %d (%.0f%% of all SC); SC excl-proximal n = %d\n",
            n_prox, sum(dat$Group == "SC_Quina"), 100 * n_prox / sum(dat$Group == "SC_Quina"),
            sum(dat$Group == "SC_Quina") - n_prox))

## ============================================================================
## BLOCK A -- MULTIVARIATE DISPERSION (PERMDISP)
## ============================================================================
## Runs betadisper/permutest on z-scored {tech6} -> Euclidean for a given data
## frame; returns per-group mean distance-to-centroid + pairwise permuted p.
run_permdisp <- function(df, outdir, prefix, title) {
  mvd <- df |> filter(if_all(all_of(tech6), is.finite)) |> mutate(Group = droplevels(Group))
  cat("\n[", title, "] N per group (complete-case on tech6):\n", sep = ""); print(table(mvd$Group))
  mat <- scale(as.matrix(mvd[, tech6]))
  d   <- dist(mat, method = "euclidean")
  bd  <- betadisper(d, mvd$Group)
  pt  <- permutest(bd, permutations = 999, pairwise = TRUE)

  means <- tapply(bd$distances, mvd$Group, mean)
  dist_df <- data.frame(Group = mvd$Group, DistanceToCentroid = bd$distances)
  write.csv(dist_df, file.path(outdir, paste0(prefix, "_distances.csv")), row.names = FALSE)
  write.csv(data.frame(Group = names(means), mean_dist_to_centroid = as.numeric(means)),
            file.path(outdir, paste0(prefix, "_group_mean_dist.csv")), row.names = FALSE)
  overall_p <- pt$tab$`Pr(>F)`[1]
  pw <- pt$pairwise$permuted
  write.csv(data.frame(pair = names(pw), permuted_p = as.numeric(pw)),
            file.path(outdir, paste0(prefix, "_pairwise_p.csv")), row.names = FALSE)

  cat("Mean distance to centroid (= dispersion size):\n"); print(round(means, 3))
  cat("permutest overall p =", signif(overall_p, 3), "\n")
  cat("pairwise permuted p:\n"); print(round(pw, 3))

  ## plot 1: distance-to-centroid box + violin
  p1 <- ggplot(dist_df, aes(Group, DistanceToCentroid, fill = Group, color = Group)) +
    geom_violin(alpha = 0.18, color = NA, width = 0.9) +
    geom_boxplot(fill = NA, color = "black", width = 0.42, linewidth = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.12, height = 0, size = 1.3, alpha = 0.55, shape = 16) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.6, fill = "white", color = "black") +
    scale_fill_manual(values = group_colors) + scale_color_manual(values = group_colors) +
    labs(title = title, subtitle = sprintf("PERMDISP overall p = %s (larger mean = more dispersed)",
                                            fmt <- ifelse(overall_p < 0.001, "<0.001", signif(overall_p, 3))),
         x = NULL, y = "Distance to group centroid", caption = guard_caption) +
    ordination_theme + theme(legend.position = "none")
  ggsave(file.path(outdir, paste0(prefix, "_distance_boxplot.png")), p1,
         width = 7.0, height = 5.2, dpi = 300)

  ## plot 2: PCA (= PCoA of Euclidean) ordination with convex hulls
  pca <- prcomp(mat, center = TRUE, scale. = FALSE)
  vexp <- pca$sdev^2 / sum(pca$sdev^2) * 100
  scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Group = mvd$Group)
  cent <- make_centroids(scores); hull <- make_hulls(scores)
  p2 <- ggplot(scores, aes(PC1, PC2, color = Group)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_polygon(data = hull, aes(fill = Group, group = Group), alpha = 0.12, color = NA) +
    geom_path(data = hull, aes(group = Group), linewidth = 0.45, alpha = 0.65) +
    geom_point(size = 1.8, alpha = 0.8, shape = 16) +
    geom_point(data = cent, aes(x_centroid, y_centroid, color = Group),
               shape = 21, fill = "white", size = 4, stroke = 1.1, inherit.aes = FALSE) +
    scale_color_manual(values = group_colors) + scale_fill_manual(values = group_colors) +
    labs(title = title, subtitle = "Convex hulls: wider hull = more dispersed group",
         x = paste0("PC1 (", round(vexp[1], 1), "%)"), y = paste0("PC2 (", round(vexp[2], 1), "%)"),
         caption = guard_caption) +
    coord_equal() + ordination_theme + guides(fill = "none")
  ggsave(file.path(outdir, paste0(prefix, "_pca_ordination.png")), p2, width = 7.6, height = 5.8, dpi = 300)

  list(means = means, overall_p = overall_p, pairwise = pw,
       ratio_SC_LT = unname(means["SC_Quina"] / means["LT_Quina"]))
}

cat("\n########## BLOCK A: MULTIVARIATE PERMDISP ##########\n")
permA <- run_permdisp(dat, sub$mv, "permdisp_all",
                      "Technical-space dispersion (PERMDISP): SC vs Longtan")

## ============================================================================
## BLOCK B1 -- CV FAMILY (ratio / dimensional variables)
## ============================================================================
cat("\n########## BLOCK B1: CV (dimensional) ##########\n")
cv_group <- list(); cv_ratio <- list()
for (v in cv_vars) {
  for (g in grp_levels) {
    x <- finite(dat[[v]][dat$Group == g])
    ci <- boot_stat_ci(x, cv_corr)
    cv_group[[length(cv_group) + 1]] <- data.frame(
      variable = v, group = g, n = length(x), mean = mean(x), sd = sd(x),
      CV = cv_raw(x), CVstar = cv_corr(x), CVstar_lo = ci[1], CVstar_hi = ci[2],
      note = if (v == "Edge_Angle") "interval scale; CV by convention only" else "")
  }
  x_sc <- finite(dat[[v]][dat$Group == "SC_Quina"])
  x_lt <- finite(dat[[v]][dat$Group == "LT_Quina"])
  ratio <- cv_corr(x_sc) / cv_corr(x_lt)
  rci <- boot_ratio_ci(x_sc, x_lt, cv_corr, mean_guard = TRUE)
  pp  <- perm_cv_equal(x_sc, x_lt)
  cv_ratio[[length(cv_ratio) + 1]] <- data.frame(
    variable = v, CVstar_SC = cv_corr(x_sc), CVstar_LT = cv_corr(x_lt),
    CVstar_ratio_SC_LT = ratio, ratio_lo = rci[1], ratio_hi = rci[2],
    perm_p_CVequal = pp, mean_SC = mean(x_sc), mean_LT = mean(x_lt),
    note = if (v == "Edge_Angle") "interval scale; CV by convention only" else "")
}
cv_group <- bind_rows(cv_group); cv_ratio <- bind_rows(cv_ratio)
write.csv(cv_group, file.path(sub$cv, "cv_group_stats.csv"), row.names = FALSE)
write.csv(cv_ratio, file.path(sub$cv, "cv_ratio_SC_vs_LTquina.csv"), row.names = FALSE)
cat("\nCV* by group:\n");        print(cv_group, row.names = FALSE)
cat("\nCV* ratio SC:LT_Quina:\n"); print(cv_ratio, row.names = FALSE)

## optional cvequality (Feltz-Miller) cross-check, 3 groups, per variable (never installs)
if (has_cvequality) {
  tryCatch({
    fm <- lapply(cv_vars, function(v) {
      dd <- dat |> select(Group, all_of(v)) |> rename(Value = all_of(v)) |> filter(is.finite(Value))
      t <- cvequality::asymptotic_test(dd$Value, dd$Group)
      data.frame(variable = v, FM_stat = t$test_statistic, FM_p = t$p_value)
    })
    write.csv(bind_rows(fm), file.path(sub$cv, "cv_feltz_miller_3group.csv"), row.names = FALSE)
    cat("\nFeltz-Miller (cvequality) 3-group cross-check written.\n")
  }, error = function(e) message("  cvequality cross-check skipped: ", conditionMessage(e)))
}

## plot: CV* by group (point + bootstrap CI)
cvg_p <- ggplot(cv_group, aes(group, CVstar, color = group)) +
  geom_pointrange(aes(ymin = CVstar_lo, ymax = CVstar_hi), size = 0.55, linewidth = 0.7) +
  facet_wrap(~ factor(variable, levels = cv_vars), scales = "free_y", nrow = 1) +
  scale_color_manual(values = group_colors) +
  labs(title = "Corrected CV* by group (bootstrap 95% CI)",
       subtitle = "Edge_Angle: interval scale, CV by convention only",
       x = NULL, y = "CV* (Sokal-Rohlf corrected)", caption = guard_caption) +
  corr_theme + theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(sub$cv, "cv_by_group.png"), cvg_p, width = 10.5, height = 4.2, dpi = 300)

## forest plot: CV* ratio SC:LT_Quina
cvr_p <- ggplot(cv_ratio, aes(CVstar_ratio_SC_LT, factor(variable, levels = rev(cv_vars)))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#202124") +
  geom_errorbarh(aes(xmin = ratio_lo, xmax = ratio_hi), height = 0.22, color = "#454649") +
  geom_point(color = "#E07C90", size = 3) +
  scale_x_continuous(trans = "log2") +
  labs(title = "CV* ratio SC_Quina : LT_Quina (>1 = SC more variable)",
       subtitle = "log2 x-axis; dashed = equal dispersion", x = "CV* ratio (SC : LT_Quina)",
       y = NULL, caption = guard_caption) +
  corr_theme
ggsave(file.path(sub$cv, "cv_ratio_forest.png"), cvr_p, width = 7.2, height = 4.2, dpi = 300)

## ============================================================================
## BLOCK B2 -- ROBUST FAMILY (reduction indicators)
## ============================================================================
cat("\n########## BLOCK B2: robust (reduction indicators) ##########\n")
## overall (3-group) + SC-vs-LT_Quina pairwise dispersion tests on a given scale
disp_tests <- function(value, group) {
  ok <- is.finite(value)
  value <- value[ok]; group <- droplevels(factor(group[ok]))
  d3 <- data.frame(Value = value, Group = group)
  fl_all <- fligner.test(Value ~ Group, data = d3)$p.value
  lv_all <- tryCatch(rstatix::levene_test(d3, Value ~ Group, center = median)$p,
                     error = function(e) NA_real_)
  pair <- d3 |> filter(Group %in% c("SC_Quina", "LT_Quina")) |> mutate(Group = droplevels(Group))
  fl_pr <- fligner.test(Value ~ Group, data = pair)$p.value
  lv_pr <- tryCatch(rstatix::levene_test(pair, Value ~ Group, center = median)$p,
                    error = function(e) NA_real_)
  c(fligner_overall = fl_all, levene_overall = lv_all,
    fligner_SC_LTq = fl_pr, levene_SC_LTq = lv_pr)
}

rob_group <- list(); rob_ratio <- list(); rob_extra <- list()
for (v in robust_vars) {
  fam <- if (v %in% bounded_vars) "bounded" else "count"
  x_sc <- finite(dat[[v]][dat$Group == "SC_Quina"])
  x_lt <- finite(dat[[v]][dat$Group == "LT_Quina"])
  for (g in grp_levels) {
    x <- finite(dat[[v]][dat$Group == g])
    rob_group[[length(rob_group) + 1]] <- data.frame(
      variable = v, family = fam, group = g, n = length(x),
      mean = mean(x), median = median(x), sd = sd(x),
      MAD = mad(x), IQR = IQR(x), variance = var(x),
      Fano = if (fam == "count") fano(x) else NA_real_)
  }
  ## robust dispersion ratios SC:LT_Quina (+ bootstrap CI). mean reported for confound check.
  mad_r <- mad(x_sc) / mad(x_lt); mad_ci <- boot_ratio_ci(x_sc, x_lt, function(z) mad(z))
  iqr_r <- IQR(x_sc) / IQR(x_lt); iqr_ci <- boot_ratio_ci(x_sc, x_lt, function(z) IQR(z))
  tests_raw <- disp_tests(dat[[v]], dat$Group)
  row <- data.frame(
    variable = v, family = fam,
    MAD_ratio_SC_LT = mad_r, MAD_lo = mad_ci[1], MAD_hi = mad_ci[2],
    IQR_ratio_SC_LT = iqr_r, IQR_lo = iqr_ci[1], IQR_hi = iqr_ci[2],
    fligner_overall_p = tests_raw["fligner_overall"], fligner_SC_LTq_p = tests_raw["fligner_SC_LTq"],
    levene_overall_p = tests_raw["levene_overall"], levene_SC_LTq_p = tests_raw["levene_SC_LTq"],
    mean_SC = mean(x_sc), mean_LT = mean(x_lt), row.names = NULL)
  rob_ratio[[length(rob_ratio) + 1]] <- row

  ## family-specific extra scale
  if (fam == "bounded") {
    ## empirical-logit: variance not mechanically compressed near 0/1
    ls <- emp_logit(x_sc); ll <- emp_logit(x_lt)
    tl <- disp_tests(emp_logit(dat[[v]]), dat$Group)
    rob_extra[[length(rob_extra) + 1]] <- data.frame(
      variable = v, scale = "empirical_logit",
      SD_SC = sd(ls), SD_LT = sd(ll), MAD_SC = mad(ls), MAD_LT = mad(ll),
      SD_ratio_SC_LT = sd(ls) / sd(ll), MAD_ratio_SC_LT = mad(ls) / mad(ll),
      fligner_overall_p = tl["fligner_overall"], fligner_SC_LTq_p = tl["fligner_SC_LTq"],
      Fano_ratio_SC_LT = NA_real_,
      note = "raw [0,1] compresses variance near boundaries; logit more comparable", row.names = NULL)
  } else {
    ## counts: Fano = var/mean (count analogue of CV); sqrt = variance-stabilizing
    ts <- disp_tests(sqrt(pmax(dat[[v]], 0)), dat$Group)
    rob_extra[[length(rob_extra) + 1]] <- data.frame(
      variable = v, scale = "sqrt_variance_stabilizing",
      SD_SC = sd(sqrt(x_sc)), SD_LT = sd(sqrt(x_lt)), MAD_SC = mad(sqrt(x_sc)), MAD_LT = mad(sqrt(x_lt)),
      SD_ratio_SC_LT = sd(sqrt(x_sc)) / sd(sqrt(x_lt)), MAD_ratio_SC_LT = mad(sqrt(x_sc)) / mad(sqrt(x_lt)),
      fligner_overall_p = ts["fligner_overall"], fligner_SC_LTq_p = ts["fligner_SC_LTq"],
      Fano_ratio_SC_LT = fano(x_sc) / fano(x_lt),
      note = "N_Scar integer counts; Ave_RG small positive mean (treated as count-like)", row.names = NULL)
  }
}
rob_group <- bind_rows(rob_group); rob_ratio <- bind_rows(rob_ratio); rob_extra <- bind_rows(rob_extra)
write.csv(rob_group, file.path(sub$rob, "robust_group_stats.csv"), row.names = FALSE)
write.csv(rob_ratio, file.path(sub$rob, "robust_ratio_tests_SC_vs_LTquina.csv"), row.names = FALSE)
write.csv(rob_extra, file.path(sub$rob, "robust_extra_scale.csv"), row.names = FALSE)
cat("\nRobust group stats (dispersion next to mean):\n"); print(rob_group, row.names = FALSE)
cat("\nRobust ratios + dispersion-equality tests (SC vs LT_Quina):\n"); print(rob_ratio, row.names = FALSE)
cat("\nRobust extra-scale (logit / sqrt+Fano):\n"); print(rob_extra, row.names = FALSE)

## plot: per-variable distributions by group + Fligner pairwise p
rob_long <- dat |> select(Group, all_of(robust_vars)) |>
  pivot_longer(all_of(robust_vars), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |> mutate(Variable = factor(Variable, levels = robust_vars))
rob_lab <- rob_ratio |>
  transmute(Variable = factor(variable, levels = robust_vars),
            label = sprintf("Fligner(SC:LTq) p %s\nMAD ratio = %.2f",
                            ifelse(fligner_SC_LTq_p < 0.001, "<0.001", paste0("= ", signif(fligner_SC_LTq_p, 2))),
                            MAD_ratio_SC_LT))
rob_p <- ggplot(rob_long, aes(Group, Value, fill = Group, color = Group)) +
  geom_jitter(width = 0.22, height = 0, size = 1.2, alpha = 0.5, shape = 16) +
  geom_boxplot(fill = NA, color = "black", width = 0.55, linewidth = 0.55, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.2, fill = "white", color = "black") +
  geom_text(data = rob_lab, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = -0.06, vjust = 1.15, size = 2.9, color = "#202124", lineheight = 0.95) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2,
             labeller = as_labeller(function(x) gsub("_", " ", x))) +
  scale_fill_manual(values = group_colors) + scale_color_manual(values = group_colors) +
  labs(title = "Reduction-indicator distributions & spread by group",
       subtitle = "white diamond = mean (read spread next to location)", x = NULL, y = NULL,
       caption = guard_caption) +
  ordination_theme + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(sub$rob, "robust_boxplots.png"), rob_p, width = 8.6, height = 7.0, dpi = 300)

## ============================================================================
## BLOCK C -- INDEPENDENCE SENSITIVITY (drop SC pieces proximal to LT/THC)
## ============================================================================
cat("\n########## BLOCK C: sensitivity (SC excl. LT/THC-proximal) ##########\n")
dat_excl <- dat |> filter(!(Group == "SC_Quina" & Site_ID %in% proximal_ids))

## headline dispersion ratio (SC:LT_Quina) by family -- reused for all/excl
headline_ratio <- function(d, v) {
  fam <- if (v %in% cv_vars) "CV" else if (v %in% bounded_vars) "bounded" else "count"
  x_sc <- finite(d[[v]][d$Group == "SC_Quina"]); x_lt <- finite(d[[v]][d$Group == "LT_Quina"])
  if (fam == "CV") {
    metric <- "CVstar_ratio"; ratio <- cv_corr(x_sc) / cv_corr(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, cv_corr, mean_guard = TRUE); p <- perm_cv_equal(x_sc, x_lt)
  } else if (fam == "bounded") {
    metric <- "MAD_ratio"; ratio <- mad(x_sc) / mad(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, function(z) mad(z)); p <- fligner_pair(x_sc, x_lt)
  } else {
    metric <- "Fano_ratio"; ratio <- fano(x_sc) / fano(x_lt)
    ci <- boot_ratio_ci(x_sc, x_lt, fano); p <- fligner_pair(x_sc, x_lt)
  }
  data.frame(variable = v, family = fam, metric = metric, n_SC = length(x_sc), n_LT = length(x_lt),
             ratio_SC_LT = ratio, lo = ci[1], hi = ci[2], equal_p = p,
             mean_SC = mean(x_sc), mean_LT = mean(x_lt), row.names = NULL)
}
all_h  <- bind_rows(lapply(need_vars, function(v) headline_ratio(dat,      v)))
excl_h <- bind_rows(lapply(need_vars, function(v) headline_ratio(dat_excl, v)))

## PERMDISP re-run on SC-excl
permC <- run_permdisp(dat_excl, sub$sen, "permdisp_excl",
                      "PERMDISP sensitivity: SC (excl. LT/THC) vs Longtan")

sens_tbl <- all_h |>
  transmute(variable, family, metric,
            ratio_SCall_LT = ratio_SC_LT, p_SCall = equal_p, mean_SCall = mean_SC) |>
  left_join(excl_h |> transmute(variable,
            ratio_SCexcl_LT = ratio_SC_LT, p_SCexcl = equal_p, mean_SCexcl = mean_SC),
            by = "variable") |>
  bind_rows(data.frame(variable = "PERMDISP_meandist", family = "multivariate", metric = "meandist_ratio",
            ratio_SCall_LT = permA$ratio_SC_LT, p_SCall = permA$overall_p, mean_SCall = NA_real_,
            ratio_SCexcl_LT = permC$ratio_SC_LT, p_SCexcl = permC$overall_p, mean_SCexcl = NA_real_))
write.csv(sens_tbl, file.path(sub$sen, "sensitivity_SCall_vs_SCexcl.csv"), row.names = FALSE)
cat("\nSensitivity side-by-side (each relative to LT_Quina):\n"); print(sens_tbl, row.names = FALSE)

## plot: SC-all vs SC-excl dispersion ratio per variable
sens_long <- sens_tbl |> filter(family != "multivariate") |>
  select(variable, ratio_SCall_LT, ratio_SCexcl_LT) |>
  pivot_longer(c(ratio_SCall_LT, ratio_SCexcl_LT), names_to = "SC_set", values_to = "ratio") |>
  mutate(SC_set = recode(SC_set, ratio_SCall_LT = "SC all", ratio_SCexcl_LT = "SC excl. LT/THC"))
sens_p <- ggplot(sens_long, aes(ratio, factor(variable, levels = rev(need_vars)),
                                color = SC_set, shape = SC_set)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#202124") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(trans = "log2") +
  scale_color_manual(values = c("SC all" = "#E07C90", "SC excl. LT/THC" = "#9B4A5C")) +
  labs(title = "Dispersion ratio SC:LT_Quina -- all vs proximal-excluded",
       subtitle = "log2 x; >1 = SC more variable; shift on exclusion = composition confound",
       x = "Dispersion ratio (SC : LT_Quina)", y = NULL, color = NULL, shape = NULL,
       caption = guard_caption) +
  corr_theme
ggsave(file.path(sub$sen, "sensitivity_forest.png"), sens_p, width = 7.6, height = 4.6, dpi = 300)

## ============================================================================
## BLOCK D -- CROSS-VARIABLE SUMMARY
## ============================================================================
cat("\n########## BLOCK D: cross-variable summary ##########\n")
summary_tbl <- all_h |>
  transmute(variable, family,
            dispersion_ratio_SC_LT = ratio_SC_LT, ratio_metric = metric,
            ratio_lo = lo, ratio_hi = hi, equality_p = equal_p,
            mean_SC = mean_SC, mean_LT = mean_LT,
            mean_ratio_SC_LT = mean_SC / mean_LT) |>
  arrange(desc(abs(log(dispersion_ratio_SC_LT))))
write.csv(summary_tbl, file.path(base_dir, "dispersion_summary.csv"), row.names = FALSE)

permdisp_headline <- data.frame(
  metric = "PERMDISP mean-distance-to-centroid ratio SC_Quina : LT_Quina",
  ratio = permA$ratio_SC_LT, permutest_overall_p = permA$overall_p,
  mean_dist_SC = unname(permA$means["SC_Quina"]), mean_dist_LTq = unname(permA$means["LT_Quina"]),
  mean_dist_LTo = unname(permA$means["LT_Ordinary"]))
write.csv(permdisp_headline, file.path(base_dir, "permdisp_headline.csv"), row.names = FALSE)

cat("\n--- PERMDISP HEADLINE (multivariate) ---\n"); print(permdisp_headline, row.names = FALSE)
cat("\n--- dispersion_summary.csv (sorted by ratio magnitude) ---\n"); print(summary_tbl, row.names = FALSE)

cat("\n########## DONE -> ", base_dir,
    "\n  multivariate_permdisp/  cv_dimensional/  robust_reduction/  sensitivity/",
    "\n  dispersion_summary.csv  permdisp_headline.csv  per_variable_n.csv  _GUARDRAILS.txt ##########\n", sep = "")
