## QV_dist_river_by_group.R
## ============================================================================
## Question: does site distance-to-river (d_river_m, straight-line/Euclidean)
## differ (1) between basins and (2) among rivers?
##
## DESIGN: one value per SITE (clean 27) -> site-level, NO pseudoreplication.
##   Basin (2 grp: Binchuan 20, Heqing 5) : Mann-Whitney U (primary) + Welch t.
##   River (3 grp: Sangyuan 6, Liandong 14, Caifeng 5): Kruskal-Wallis + Dunn
##      (Bonferroni) (primary) + Welch ANOVA + Games-Howell.
##   Consistency with the PERMANOVA thread: adonis2(dist(d) ~ grp) is, for one
##   variable on a Euclidean distance, a permutational ANOVA; PERMDISP checks
##   whether the SPREAD of distances differs (relevant: THC/DPD_1 are outliers).
##
## GUARDRAILS
##  * Small, unbalanced N + right-skew with outliers (THC 2690 m, DPD_1 1390 m)
##    -> rank-based tests are primary; read medians, not means; rank tests are
##    unaffected by the monotonic log axis used for plotting.
##  * NESTING/CONFOUND: river_ID nests in basin (Sangyuan,Liandong c Binchuan;
##    Caifeng = Heqing). So the basin contrast = (Sangyuan+Liandong) vs Caifeng,
##    and the river analysis partly re-expresses the basin difference. The nested
##    adonis2(d ~ basin/river_ID) separates between-basin vs river-within-basin.
##  * Sites on one river may be spatially autocorrelated in d_river_m (minor;
##    not pseudoreplication -- each site still contributes one independent value).
## ============================================================================

required <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan", "rstatix", "ggpubr", "patchwork")
miss <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: ", paste(miss, collapse = ", "))
library(readxl); library(dplyr); library(tidyr); library(ggplot2)
library(vegan); library(rstatix); library(ggpubr); library(patchwork)
set.seed(123)

proj_dir  <- "H:/Quina_valleys"
site_path <- file.path(proj_dir, "data", "Site_information.xlsx")
out_dir   <- file.path(proj_dir, "output", "01_landscape_raw_material", "dist_river_by_group")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
perm <- 999
drop_sites <- c("PJDD", "ZKZ")

basin_levels <- c("Binchuan", "Heqing")
basin_colors <- c(Binchuan = "#C9603F", Heqing = "#3F7CAC")
river_levels <- c("Sangyuan", "Liandong", "Caifeng")
river_colors <- c(Sangyuan = "#D55E00", Liandong = "#E69F00", Caifeng = "#0072B2")
strip_basin  <- function(x) sub(" basin$", "", trimws(as.character(x)))
fmt_p <- function(p) ifelse(is.na(p), "NA",
  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3))))

base_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E6E8EB", linewidth = 0.35),
        panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
        axis.ticks = element_line(color = "#202124", linewidth = 0.35),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#454649"),
        axis.text = element_text(color = "#303238"), legend.position = "none",
        plot.background = element_rect(color = NA, fill = "white"),
        panel.background = element_rect(color = NA, fill = "white"))

## ---- data: one d_river_m per clean site ------------------------------------
sites <- read_excel(site_path); names(sites) <- trimws(names(sites))
dat <- sites |>
  transmute(Code = trimws(as.character(Code)),
            basin = factor(strip_basin(basin), levels = basin_levels),
            river_ID = factor(trimws(river_ID), levels = river_levels),
            d = as.numeric(d_river_m)) |>
  filter(!Code %in% drop_sites, !is.na(d), !is.na(basin), !is.na(river_ID))

cat("N =", nrow(dat), "sites\n")
cat("\nBy basin:\n");  print(table(dat$basin))
cat("By river:\n");    print(table(dat$river_ID))

## ---- descriptives + assumption checks --------------------------------------
desc_basin <- dat |> group_by(basin)    |> get_summary_stats(d, type = "common") |> ungroup()
desc_river <- dat |> group_by(river_ID) |> get_summary_stats(d, type = "common") |> ungroup()
cat("\n== d_river_m descriptives by basin ==\n"); print(desc_basin |> select(basin, n, median, iqr, mean, sd, min, max))
cat("\n== d_river_m descriptives by river ==\n"); print(desc_river |> select(river_ID, n, median, iqr, mean, sd, min, max))

sh_basin <- dat |> group_by(basin)    |> shapiro_test(d) |> ungroup()
sh_river <- dat |> group_by(river_ID) |> shapiro_test(d) |> ungroup()
lev_basin <- dat |> levene_test(d ~ basin)
lev_river <- dat |> levene_test(d ~ river_ID)
cat("\nShapiro (normality) by basin:\n"); print(sh_basin)
cat("Shapiro by river:\n");               print(sh_river)
cat("Levene (variance homogeneity): basin p =", signif(lev_basin$p, 3),
    "| river p =", signif(lev_river$p, 3), "\n")

## ============================================================================
## (1) BASIN -- Mann-Whitney U (primary) + Welch t (cross-check) + effect sizes
## ============================================================================
mw_basin  <- dat |> wilcox_test(d ~ basin) |> add_significance()
eff_basin <- dat |> wilcox_effsize(d ~ basin)                       # rank-biserial r
t_basin   <- dat |> t_test(d ~ basin, var.equal = FALSE)            # Welch
cd_basin  <- dat |> cohens_d(d ~ basin, var.equal = FALSE)
cat("\n########## (1) BASIN ##########\n")
cat(sprintf("Mann-Whitney U: p %s | rank-biserial r = %.2f (%s)\n",
            fmt_p(mw_basin$p), eff_basin$effsize, eff_basin$magnitude))
cat(sprintf("Welch t: t = %.2f, df = %.1f, p %s | Cohen's d = %.2f (%s)\n",
            t_basin$statistic, t_basin$df, fmt_p(t_basin$p), cd_basin$effsize, cd_basin$magnitude))

## ============================================================================
## (2) RIVER -- Kruskal-Wallis + Dunn (Bonferroni) + Welch ANOVA + Games-Howell
## ============================================================================
kw      <- dat |> kruskal_test(d ~ river_ID)
kw_eff  <- dat |> kruskal_effsize(d ~ river_ID)                     # epsilon^2
dunn    <- dat |> dunn_test(d ~ river_ID, p.adjust.method = "bonferroni")
welch_a <- dat |> welch_anova_test(d ~ river_ID)
gh      <- dat |> games_howell_test(d ~ river_ID)
cat("\n########## (2) RIVER ##########\n")
cat(sprintf("Kruskal-Wallis: chi2 = %.2f, df = %d, p %s | epsilon^2 = %.2f (%s)\n",
            kw$statistic, kw$df, fmt_p(kw$p), kw_eff$effsize, kw_eff$magnitude))
cat(sprintf("Welch ANOVA:    F = %.2f, p %s\n", welch_a$statistic, fmt_p(welch_a$p)))
cat("Dunn post-hoc (Bonferroni):\n")
print(dunn |> select(group1, group2, n1, n2, statistic, p.adj, p.adj.signif))
cat("Games-Howell post-hoc:\n")
print(gh |> select(group1, group2, estimate, conf.low, conf.high, p.adj, p.adj.signif))

## ============================================================================
## (3) PERMANOVA-consistency: permutational ANOVA on Euclidean d + PERMDISP
## ============================================================================
D <- dist(dat$d, method = "euclidean")
run_perm <- function(fac) {
  ad <- adonis2(as.formula(paste("D ~", fac)), data = dat, permutations = perm)
  disp_p <- NA_real_
  tryCatch({ pt <- permutest(betadisper(D, dat[[fac]]), permutations = perm)
             disp_p <- pt$tab$`Pr(>F)`[1] }, error = function(e) {})
  cat(sprintf("adonis2(d ~ %-8s): R2 = %.3f, F = %.2f, p %s | PERMDISP p %s\n",
              fac, ad$R2[1], ad$F[1], fmt_p(ad$`Pr(>F)`[1]), fmt_p(disp_p)))
  data.frame(term = fac, R2 = ad$R2[1], F = ad$F[1], p = ad$`Pr(>F)`[1], permdisp_p = disp_p)
}
cat("\n########## (3) permutational ANOVA (Euclidean, 1 var) + PERMDISP ##########\n")
pm <- bind_rows(run_perm("basin"), run_perm("river_ID"))
nested <- adonis2(D ~ basin / river_ID, data = dat, permutations = perm, by = "terms")
cat("\nNested d ~ basin/river_ID (between-basin vs river-within-basin):\n"); print(nested)

## ============================================================================
## VISUALISATION: boxplots (log10 y) by basin and by river, with post-hoc
## ============================================================================
lab_out <- dat |> filter(d >= 1200)                          # THC, DPD_1

## house boxplot style (matches QV_landscape_triage.R): jittered coloured points,
## unfilled black box, solid black point = mean.
box_layer <- function(g, colors)
  list(geom_jitter(aes(color = .data[[g]]), width = 0.28, height = 0,
                   size = 1.7, alpha = 0.6, shape = 16),
       geom_boxplot(color = "black", fill = NA, width = 0.62, linewidth = 0.6,
                    outlier.shape = NA),
       stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "black"),
       geom_text(data = lab_out, aes(label = Code), size = 2.7,
                 hjust = -0.28, color = "grey25"),
       scale_color_manual(values = colors),
       scale_y_log10(breaks = c(200, 300, 500, 800, 1200, 2000, 3000)))

p_basin <- ggplot(dat, aes(basin, d)) + box_layer("basin", basin_colors) +
  labs(title = "By basin",
       subtitle = sprintf("Mann-Whitney p %s;  r = %.2f (%s)",
                          fmt_p(mw_basin$p), eff_basin$effsize, eff_basin$magnitude),
       x = NULL, y = "Distance to river (m, log scale)") + base_theme

p_river <- ggplot(dat, aes(river_ID, d)) + box_layer("river_ID", river_colors) +
  labs(title = "By river",
       subtitle = sprintf("Kruskal-Wallis p %s;  epsilon^2 = %.2f;  pairwise ns",
                          fmt_p(kw$p), kw_eff$effsize),
       x = NULL, y = NULL) + base_theme

combined <- (p_basin | p_river) +
  plot_annotation(
    title = "Site distance-to-river by basin and by river",
    subtitle = sprintf("n = %d sites; jittered points = sites, box = median/IQR, black dot = mean; THC & DPD_1 labelled (outliers)", nrow(dat)),
    caption = "Rank-based tests are primary (unaffected by the log axis). River_ID nests in basin: Caifeng = Heqing.",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
                  plot.subtitle = element_text(hjust = 0.5, size = 10.5, color = "#454649"),
                  plot.caption = element_text(hjust = 0, size = 8.5, color = "#454649")))
ggsave(file.path(out_dir, "dist_river_by_group.png"), combined, width = 9.6, height = 5.2, dpi = 300)

writeLines(c(
  "GUARDRAILS -- d_river_m by basin/river",
  "* Small, unbalanced N (basin 20/5; river 6/14/5) + right-skew with outliers",
  "  (THC 2690 m, DPD_1 1390 m). Rank-based tests (Mann-Whitney, Kruskal-Wallis,",
  "  Dunn) are primary; read MEDIANS not means; Welch is a cross-check only.",
  "* river_ID nests in basin (Caifeng = Heqing). The basin contrast and the river",
  "  contrast overlap; the nested adonis2(d ~ basin/river_ID) separates them.",
  "* PERMDISP flags whether a 'difference' is really a SPREAD difference (outliers)."),
  file.path(out_dir, "_GUARDRAILS.txt"))

cat("\n########## DONE. Outputs under", out_dir, "##########\n")
