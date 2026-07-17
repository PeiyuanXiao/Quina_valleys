## QV_clast_size_by_material.R
## ============================================================================
## Question: are TRACHYTE river clasts larger than SANDSTONE clasts? (supports the
## Results claim "粗面岩砾石显著大于砂岩", U = 16356.00, p = 0.003).
##
## SIZE METRIC: per-clast geometric mean of the three orthogonal dimensions,
##   size_gm = (Length * Breadth * Thickness)^(1/3)   [mm].
## The geometric mean is the natural single-number size for a 3-axis measurement
## (scale-consistent, less outlier-driven than the arithmetic mean, and defined
## only for positive dimensions -> clasts with a missing/non-positive axis drop).
##
## TEST: Mann-Whitney U (Wilcoxon rank-sum), two-sided, Trachyte vs Sandstone,
##   with the rank-biserial correlation r as the effect size. Direction is read
##   from the group medians / geometric means (rank test alone is symmetric).
##
## DATA: Raw_mat_basin.xlsx, "Sheet1" (469 river-gravel clasts; the same survey
##   used by QV_raw_material_electivity.R / QV_raw_material_permanova.R).
##
## ----------------------------------------------------------------------------
## SANDSTONE DEFINITION (read before citing the U statistic)
##  The sibling scripts harmonise  "Quartz sandstone" + "Coarse sandstone" ->
##  "Sandstone".  This script uses that HARMONISED definition as the PRIMARY test
##  (internally consistent with the composition / electivity / PERMANOVA results).
##  It ALSO re-runs the test against "Quartz sandstone" ONLY, because that
##  narrower definition is what reproduces the number currently in the manuscript
##  (U = 16356.00, p = 0.003; see the MANUSCRIPT RECONCILIATION block below).
##  Decide which definition the paper should report and align the two.
##
## DATA-QUALITY REPAIR
##  One Breadth cell is the typo "69..5" (Loc 2, a Trachyte clast). It is repaired
##  in-script to 69.5 (NOT in the source file); without the repair that clast
##  drops and Trachyte n = 128 instead of 129.
## ============================================================================

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "rstatix")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0)
  stop("Please install R packages before running: ",
       paste(missing_packages, collapse = ", "))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)

set.seed(123)

## ---- paths -----------------------------------------------------------------
proj_dir   <- "H:/Quina_valleys"
basin_path <- file.path(proj_dir, "data", "Raw_mat_basin.xlsx")
out_dir    <- file.path(proj_dir, "output", "01_landscape_raw_material", "clast_size_by_material")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- shared levels / palette (matches the other 01_ scripts) ---------------
material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone", "Andesite")
material_colors <- c(Trachyte = "#D55E00", Sandstone = "#E69F00",
                     Quartz = "#56B4E9", Mudstone = "#0072B2", Andesite = "#009E73")

harmonise_lithology <- function(x) {
  x <- trimws(as.character(x))
  dplyr::recode(x, "Quartz sandstone" = "Sandstone", "Coarse sandstone" = "Sandstone")
}
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

## ---- load + repair + compute the geometric-mean size -----------------------
raw <- read_excel(basin_path, sheet = "Sheet1")
names(raw) <- trimws(names(raw))

## repair the "69..5" Breadth typo (character column -> would otherwise be NA)
breadth_chr <- trimws(as.character(raw$Breadth))
n_repaired  <- sum(breadth_chr == "69..5", na.rm = TRUE)
breadth_chr[breadth_chr == "69..5"] <- "69.5"

clasts <- raw |>
  transmute(
    Lithology = trimws(as.character(Lithology)),
    Material  = factor(harmonise_lithology(Lithology), levels = material_levels),
    L  = suppressWarnings(as.numeric(as.character(Length))),
    B  = suppressWarnings(as.numeric(breadth_chr)),
    Th = suppressWarnings(as.numeric(as.character(Thickness)))
  ) |>
  mutate(size_gm = (L * B * Th)^(1 / 3))

n_total    <- nrow(clasts)
clasts_ok  <- clasts |> filter(is.finite(size_gm), L > 0, B > 0, Th > 0)
n_dropped  <- n_total - nrow(clasts_ok)

cat("Loaded", n_total, "clasts | Breadth typos repaired:", n_repaired,
    "| dropped (missing/non-positive dimension):", n_dropped,
    "| valid size_gm:", nrow(clasts_ok), "\n")

## ---- descriptives by material (geometric mean = exp(mean(log(size)))) ------
geom_mean <- function(x) exp(mean(log(x)))
desc <- clasts_ok |>
  group_by(Material) |>
  summarise(n = n(),
            geom_mean_mm = geom_mean(size_gm),
            median_mm = median(size_gm),
            iqr_mm = IQR(size_gm),
            mean_mm = mean(size_gm),
            sd_mm = sd(size_gm),
            min_mm = min(size_gm),
            max_mm = max(size_gm),
            .groups = "drop") |>
  arrange(match(Material, material_levels))
cat("\n== size_gm descriptives by material (mm) ==\n"); print(as.data.frame(desc), digits = 4)

## ---- Mann-Whitney U engine (Trachyte vs a given sandstone set) --------------
## Reports U in the conventional (smaller) form to match SPSS/JASP-style output;
## direction is carried by the group geometric means / medians + effect size.
run_mw <- function(data, sand_label, sand_lithologies, tag) {
  d <- data |>
    filter(Lithology == "Trachyte" |
           Lithology %in% sand_lithologies) |>
    mutate(Grp = factor(ifelse(Lithology == "Trachyte", "Trachyte", "Sandstone"),
                        levels = c("Trachyte", "Sandstone")))
  nT <- sum(d$Grp == "Trachyte"); nS <- sum(d$Grp == "Sandstone")

  ## base wilcox.test for the exact U (W = U for the FIRST group, Trachyte)
  wt   <- suppressWarnings(wilcox.test(size_gm ~ Grp, data = d))          # two-sided
  wt_g <- suppressWarnings(wilcox.test(size_gm ~ Grp, data = d,
                                       alternative = "greater"))          # Trachyte > Sandstone
  U_TS <- unname(wt$statistic)          # Mann-Whitney U for Trachyte over Sandstone
  U_ST <- nT * nS - U_TS
  U_report <- min(U_TS, U_ST)           # conventional Mann-Whitney U (manuscript form)

  ## rank-biserial effect size (rstatix), project idiom
  eff <- d |> rstatix::wilcox_effsize(size_gm ~ Grp)

  gm_T <- geom_mean(d$size_gm[d$Grp == "Trachyte"])
  gm_S <- geom_mean(d$size_gm[d$Grp == "Sandstone"])
  direction <- ifelse(gm_T > gm_S, "Trachyte > Sandstone", "Trachyte < Sandstone")

  cat(sprintf(
    "\n[%s]  Trachyte (n=%d) vs %s (n=%d)\n  geom-mean: Trachyte %.1f mm vs Sandstone %.1f mm (%s)\n  Mann-Whitney U = %.2f | W(T>S) = %.0f | p (2-sided) %s | p (Trachyte greater) %s | r = %.2f (%s)\n",
    tag, nT, sand_label, nS, gm_T, gm_S, direction,
    U_report, U_TS, fmt_p(wt$p.value), fmt_p(wt_g$p.value),
    eff$effsize, eff$magnitude))

  data.frame(
    comparison = paste0("Trachyte vs ", sand_label),
    sandstone_definition = sand_label,
    n_trachyte = nT, n_sandstone = nS,
    geom_mean_trachyte_mm = round(gm_T, 2),
    geom_mean_sandstone_mm = round(gm_S, 2),
    direction = direction,
    U = U_report,                    # conventional (smaller) Mann-Whitney U
    W_trachyte_vs_sandstone = U_TS,  # wilcox.test statistic (Trachyte first)
    p_two_sided = wt$p.value,
    p_trachyte_greater = wt_g$p.value,
    effsize_r = eff$effsize, effsize_magnitude = eff$magnitude,
    stringsAsFactors = FALSE)
}

## ============================================================================
## PRIMARY TEST -- harmonised Sandstone (Quartz sandstone + Coarse sandstone)
## ============================================================================
cat("\n########## PRIMARY: Trachyte vs harmonised Sandstone (Q + Coarse) ##########")
primary <- run_mw(clasts_ok, "Sandstone (Quartz + Coarse)",
                  c("Quartz sandstone", "Coarse sandstone"), "PRIMARY / harmonised")

## ============================================================================
## MANUSCRIPT RECONCILIATION -- "Sandstone" = Quartz sandstone ONLY
##   This narrower set reproduces the figure now in the manuscript:
##   U = 16356.00, p = 0.003 (two-sided).
## ============================================================================
cat("\n########## RECONCILIATION: Trachyte vs Quartz sandstone ONLY ##########")
manuscript <- run_mw(clasts_ok, "Quartz sandstone only",
                     "Quartz sandstone", "manuscript reconciliation")

mw_tbl <- bind_rows(primary, manuscript)

cat("\n== Mann-Whitney summary (both sandstone definitions) ==\n")
print(mw_tbl[, c("sandstone_definition", "n_sandstone", "direction",
                 "U", "p_two_sided", "effsize_r")], row.names = FALSE, digits = 4)
cat("\nNote: the manuscript's 'U = 16356.00, p = 0.003' corresponds to the\n",
    "Quartz-sandstone-only row. The harmonised-Sandstone row is the definition\n",
    "used by every other 01_landscape_raw_material script.\n", sep = "")

## ============================================================================
## FIGURE -- size_gm by material, Trachyte vs harmonised Sandstone (log y)
## ============================================================================
plot_df <- clasts_ok |>
  filter(Material %in% c("Trachyte", "Sandstone")) |>
  mutate(Material = factor(Material, levels = c("Trachyte", "Sandstone")))

p <- ggplot(plot_df, aes(Material, size_gm)) +
  geom_jitter(aes(color = Material), width = 0.28, height = 0,
              size = 1.5, alpha = 0.5, shape = 16) +
  geom_boxplot(color = "black", fill = NA, width = 0.62, linewidth = 0.6,
               outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2.2, color = "black") +
  scale_color_manual(values = material_colors) +
  scale_x_discrete(labels = function(x) {
    n <- table(plot_df$Material)[x]; sprintf("%s\n(n = %d)", x, n) }) +
  scale_y_log10() +
  labs(title = "River-clast size by raw material",
       subtitle = sprintf("Geometric-mean size (L×B×Th)^(1/3);  Mann-Whitney U = %.0f, p %s, r = %.2f",
                          primary$U, fmt_p(primary$p_two_sided), primary$effsize_r),
       x = NULL, y = "Geometric-mean clast size (mm, log scale)") +
  base_theme
ggsave(file.path(out_dir, "clast_size_by_material.png"), p,
       width = 5.4, height = 5.2, dpi = 300)

## ---- guardrails note --------------------------------------------------------
writeLines(c(
  "GUARDRAILS -- clast size (geometric mean) by raw material",
  "* size_gm = (Length * Breadth * Thickness)^(1/3); defined only for clasts with",
  "  all three dimensions present and > 0 (others dropped).",
  "* Mann-Whitney is symmetric: direction is read from the group geometric means /",
  "  medians (Trachyte larger), NOT from the U statistic alone.",
  "* U is reported in the conventional (smaller) form to match the manuscript;",
  "  W_trachyte_vs_sandstone is the raw wilcox.test statistic (Trachyte first).",
  "* SANDSTONE DEFINITION drives the exact number:",
  "    - harmonised (Quartz + Coarse sandstone) = consistent with sibling scripts;",
  "    - Quartz sandstone only = reproduces the manuscript's U = 16356.00, p = 0.003.",
  "  Both give the same conclusion (Trachyte significantly larger). Pick one and",
  "  align the manuscript.",
  "* One Breadth typo ('69..5') is repaired to 69.5 in-script, not in the file."),
  file.path(out_dir, "_GUARDRAILS.txt"))

cat("\n########## DONE. Outputs under", out_dir, "##########\n")
