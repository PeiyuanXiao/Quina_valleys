# ==========================================================================
# _analysis.R -- the single source of every statistical result reported in
# the manuscript and in the supplementary material.
# Input:  data/Quina_scraper_surface.xlsx, data/Longtan_lithic_tools.xlsx,
#         data/Raw_mat_basin.xlsx, data/Site_information.xlsx
# ==========================================================================

library(tidyverse)
library(here)
library(readxl)
library(vegan)
library(rstatix)
library(ggpubr)
library(patchwork)
library(ggrepel)
library(grid)
library(cvequality)

set.seed(2226)
PERM    <- 9999
B_BOOT  <- 5000   # bootstrap replicates: every percentile interval reported
                  # (dispersion ratios, Spearman rho, Cohen's d)
MSLR_NR <- 1e5    # Monte Carlo iterations for the KL-MSLRT

# ---- inline-number formatters --------------------------------------------
f  <- function(x, d = 2) formatC(x, format = "f", digits = d)
fp <- function(p) {
  if (is.na(p)) "NA"
  else if (p < 0.001) "< 0.001"
  else paste0("= ", formatC(p, format = "f", digits = 3))
}
# significance stars on the cut points rstatix uses
psig <- function(p) {
  ifelse(is.na(p), "", ifelse(p <= 1e-4, "****", ifelse(p <= 1e-3, "***",
    ifelse(p <= 1e-2, "**", ifelse(p <= 5e-2, "*", "ns")))))
}
# non-breaking indent for the rows sitting under a panel heading in a table
IND <- strrep(intToUtf8(160), 3)

sc_path    <- here("data", "Quina_scraper_surface.xlsx")
lt_path    <- here("data", "Longtan_lithic_tools.xlsx")
basin_path <- here("data", "Raw_mat_basin.xlsx")
site_path  <- here("data", "Site_information.xlsx")

material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone", "Andesite")
river_levels    <- c("Sangyuan", "Liandong", "Caifeng")
basin_levels    <- c("Binchuan", "Heqing")
variables       <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
                     "N_Scar", "Ave_RG", "Edge_Angle")

# ---- display labels for those six variables -------------------------------
variable_labels <- c(Thickness            = "Thickness (mm)",
                     Retouch_length_index = "Retouched perimeter",
                     Ave_GIUR             = "GIUR",
                     N_Scar               = "Total scars",
                     Ave_RG               = "Retouch generations",
                     Edge_Angle           = "Edge angle (°)")
variable_labels_bare <- sub(" \\(.*\\)$", "", variable_labels)
names(variable_labels_bare) <- names(variable_labels)
disp_labels <- c(Length = "Length (mm)", Width = "Width (mm)",
                 Mass = "Mass (g)", variable_labels)

harmonise_lithology <- function(x) {
  x <- trimws(as.character(x))
  recode(x, "Quartz sandstone" = "Sandstone", "Coarse sandstone" = "Sandstone")
}
strip_basin <- function(x) sub(" basin$", "", trimws(as.character(x)))
gm <- function(x) exp(mean(log(x[is.finite(x) & x > 0])))

q     <- read_excel(sc_path, sheet = "Quina scraper")
rfl   <- read_excel(sc_path, sheet = "Resharpening flake")
sites <- read_excel(site_path); names(sites) <- trimws(names(sites))

# --------------------------------------------------------------------------
# 1. Distance-to-river by basin and valley (site-level; rank tests)
# --------------------------------------------------------------------------
dist_dat <- sites |>
  transmute(Code = trimws(as.character(Code)),
            basin    = factor(strip_basin(basin), levels = basin_levels),
            river_ID = factor(trimws(river_ID),   levels = river_levels),
            d = as.numeric(d_river_m)) |>
  filter(!is.na(d), !is.na(basin), !is.na(river_ID))
dist_median  <- median(dist_dat$d)
mw_basin     <- dist_dat |> wilcox_test(d ~ basin)
dist_basin_U <- mw_basin$statistic; dist_basin_p <- mw_basin$p
kw_river     <- dist_dat |> kruskal_test(d ~ river_ID)
dist_river_H <- kw_river$statistic; dist_river_df <- kw_river$df; dist_river_p <- kw_river$p

# --------------------------------------------------------------------------
# 2. Raw-material composition and Jacobs' D
# --------------------------------------------------------------------------
avail_raw <- harmonise_lithology(read_excel(basin_path, "Sheet1")[["Lithology"]])
comp_avail <- tibble(Material = avail_raw) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  count(Material) |> mutate(p = 100 * n / sum(n))
n_cobble        <- sum(comp_avail$n)
sand_avail_pct <- comp_avail$p[comp_avail$Material == "Sandstone"]
trach_avail_pct<- comp_avail$p[comp_avail$Material == "Trachyte"]

used_raw <- harmonise_lithology(q[["Raw_material"]])
comp_used <- tibble(Material = used_raw) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  count(Material) |> mutate(r = 100 * n / sum(n))
trach_used_pct <- comp_used$r[comp_used$Material == "Trachyte"]
andesite_n     <- sum(comp_used$n[comp_used$Material == "Andesite"])

jac <- full_join(comp_used |> transmute(Material, r = r / 100),
                 comp_avail |> transmute(Material, p = p / 100), by = "Material") |>
  mutate(r = replace_na(r, 0), p = replace_na(p, 0),
         D = (r - p) / (r + p - 2 * r * p))
Dget   <- function(m) jac$D[jac$Material == m]
D_trach <- Dget("Trachyte"); D_sand <- Dget("Sandstone")
D_quartz <- Dget("Quartz");  D_mud <- Dget("Mudstone"); D_and <- Dget("Andesite")

# PERMANOVA: used-tool composition ~ basin / valley (artifact-level, Bray-Curtis)
site_key <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            basin    = factor(strip_basin(basin), levels = basin_levels),
            river_ID = factor(trimws(river_ID),   levels = river_levels))
scA <- q |>
  transmute(Site_ID = trimws(as.character(Site_ID)),
            Material = harmonise_lithology(Raw_material)) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  left_join(site_key, by = "Site_ID") |>
  mutate(Material = factor(Material, levels = material_levels))
A_D    <- vegdist(as.matrix(unclass(table(seq_len(nrow(scA)), droplevels(scA$Material)))), "bray")
A_meta <- data.frame(basin = droplevels(scA$basin), river_ID = droplevels(scA$river_ID))
set.seed(2226); usedA_basin <- adonis2(A_D ~ basin,    data = A_meta, permutations = PERM)
set.seed(2226); usedA_river <- adonis2(A_D ~ river_ID, data = A_meta, permutations = PERM)

# PERMANOVA: available cobbles ~ sampling point / valley (cobble-level heterogeneity)
cobbles <- read_excel(basin_path, "Sheet1"); names(cobbles) <- trimws(names(cobbles))
cobbles <- cobbles |>
  transmute(Loc = as.character(Loc),
            river_ID = factor(trimws(river_ID), levels = river_levels),
            Material = factor(harmonise_lithology(Lithology), levels = material_levels)) |>
  filter(!is.na(Material))
C_D <- vegdist(as.matrix(unclass(table(seq_len(nrow(cobbles)), cobbles$Material))), "bray")
set.seed(2226); cobbleLoc   <- adonis2(C_D ~ Loc,      data = cobbles, permutations = PERM)
set.seed(2226); cobbleRiver <- adonis2(C_D ~ river_ID, data = cobbles, permutations = PERM)

# the `Pr(>F)` column name cannot be reached from inline `r ...` code
ad3 <- function(a) c(R2 = a$R2[1], F = a$F[1], p = a$`Pr(>F)`[1])
usedA_basin_v <- ad3(usedA_basin); usedA_river_v <- ad3(usedA_river)
cobbleLoc_v    <- ad3(cobbleLoc);    cobbleRiver_v  <- ad3(cobbleRiver)

# composition by valley: shared by the manuscript figure and the supplementary table
layer_levels <- c("River cobbles", "Surface Quina scrapers")
site_key_fig <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            river_ID = factor(trimws(river_ID), levels = river_levels))
used_fig <- q |>
  transmute(Site_ID = trimws(as.character(Site_ID)),
            Material = harmonise_lithology(Raw_material)) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  left_join(site_key_fig, by = "Site_ID") |> filter(!is.na(river_ID)) |>
  transmute(Layer = layer_levels[2], river_ID, Material)
avail_fig <- read_excel(basin_path, sheet = "Sheet1")
names(avail_fig) <- trimws(names(avail_fig))
avail_fig <- avail_fig |>
  transmute(river_ID = factor(trimws(river_ID), levels = river_levels),
            Material = harmonise_lithology(Lithology)) |>
  filter(!is.na(Material), !is.na(river_ID)) |>
  transmute(Layer = layer_levels[1], river_ID, Material)
both_fig <- bind_rows(avail_fig, used_fig) |>
  mutate(Layer = factor(Layer, levels = layer_levels),
         Material = factor(Material, levels = material_levels))
comp_fig <- both_fig |>
  count(Layer, river_ID, Material, name = "n") |>
  complete(Layer, river_ID, Material, fill = list(n = 0)) |>
  group_by(Layer, river_ID) |> mutate(percent = 100 * n / sum(n)) |>
  arrange(desc(as.integer(Material)), .by_group = TRUE) |>
  mutate(pos = cumsum(percent) - percent / 2) |> ungroup()
counts_fig <- both_fig |> count(Layer, river_ID, name = "N")

# --------------------------------------------------------------------------
# 3. Clast size and shape by raw material
# --------------------------------------------------------------------------
raw <- read_excel(basin_path, "Sheet1"); names(raw) <- trimws(names(raw))
bchr <- trimws(as.character(raw$Breadth)); bchr[bchr == "69..5"] <- "69.5"  # repair typo
cl <- raw |>
  transmute(Lithology = trimws(as.character(Lithology)),
            Material = factor(harmonise_lithology(Lithology), levels = material_levels),
            L  = suppressWarnings(as.numeric(as.character(Length))),
            B  = suppressWarnings(as.numeric(bchr)),
            Th = suppressWarnings(as.numeric(as.character(Thickness))),
            Shape = trimws(as.character(Shape))) |>
  mutate(size = (L * B * Th)^(1 / 3)) |>
  filter(is.finite(size), L > 0, B > 0, Th > 0)

# ---- clast form, taken from the axes rather than from the field record ----
# the field shape terms mix form with regularity, so form comes from the axes
cl_ax <- t(apply(as.matrix(cl[, c("L", "B", "Th")]), 1, sort, decreasing = TRUE))
cl <- cl |>
  mutate(a_ax = cl_ax[, 1], b_ax = cl_ax[, 2], c_ax = cl_ax[, 3],
         Sphericity = (c_ax^2 / (a_ax * b_ax))^(1 / 3))

# Sneed and Folk coordinates, for the figure. a = c leaves the ratios
# undefined; such a clast is perfectly compact, and n_ac_equal is 0 here
n_ac_equal <- sum(cl$a_ax == cl$c_ax)
cl <- cl |>
  mutate(sf_ab = ifelse(a_ax > c_ax, (a_ax - b_ax) / (a_ax - c_ax), 0.5),
         sf_bc = 1 - sf_ab,                       # = (b - c) / (a - c)
         sf_compact  = c_ax / a_ax,
         sf_elongate = (1 - sf_compact) * sf_ab,
         sf_platy    = (1 - sf_compact) * sf_bc)

sz  <- cl |> filter(Material %in% c("Trachyte", "Sandstone")) |>
  mutate(G = factor(Material, levels = c("Trachyte", "Sandstone")))
gmT <- gm(sz$size[sz$G == "Trachyte"]); gmS <- gm(sz$size[sz$G == "Sandstone"])
size_wt <- suppressWarnings(wilcox.test(size ~ G, data = sz))
nT <- sum(sz$G == "Trachyte"); nS <- sum(sz$G == "Sandstone")
size_U  <- min(unname(size_wt$statistic), nT * nS - unname(size_wt$statistic))
size_p  <- size_wt$p.value
size_r  <- sz |> wilcox_effsize(size ~ G) |> pull(effsize)

# ---- does form distinguish the two lithologies? ---------------------------
# the three axes as a composition: Aitchison distance, so size divides out and
# the ilr basis does not matter. Strictly positive, so no zero replacement
form_axes <- as.matrix(sz[, c("a_ax", "b_ax", "c_ax")])
D_form    <- vegdist(form_axes, method = "aitchison")
set.seed(2226)
form_perm <- adonis2(D_form ~ G, data = sz, permutations = PERM)
form_R2 <- form_perm$R2[1]; form_F <- form_perm$F[1]
form_df <- form_perm$Df[1]; form_df_res <- form_perm$Df[2]
form_p  <- form_perm$`Pr(>F)`[1]
# PERMDISP on the same distances
set.seed(2226)
form_disp <- betadisper(D_form, sz$G)
set.seed(2226)
form_disp_perm <- permutest(form_disp, permutations = PERM)
form_disp_F <- form_disp_perm$tab$F[1]
form_disp_p <- form_disp_perm$tab$`Pr(>F)`[1]
# sphericity is not tested: log psi is the clr coordinate of the short axis,
# a marginal direction of the comparison just made

# --------------------------------------------------------------------------
# 4. Quina scraper techno-typology descriptive analysis
# --------------------------------------------------------------------------
n_scraper        <- nrow(q)
cortex_bearing_n <- sum(q$Cortex != "0");  cortex_bearing_pct <- 100 * cortex_bearing_n / n_scraper
nocortex_n       <- sum(q$Cortex == "0");  nocortex_pct       <- 100 * nocortex_n / n_scraper
lateral_n <- sum(q$Cortex_position == "Lateral"); lateral_pct <- 100 * lateral_n / cortex_bearing_n
primary_n <- sum(q$Cortex_position == "Primary"); primary_pct <- 100 * primary_n / n_scraper

# typological variation: retouched edges (Bordes attributions -> tbl-bordes-typology)
styp   <- table(factor(trimws(q$Sub_typology), levels = c("Single", "Double", "Multi")))
styp_n <- as.integer(styp); names(styp_n) <- names(styp)
styp_p <- 100 * styp_n / n_scraper; names(styp_p) <- names(styp)

plat   <- q |> filter(!is.na(Platform_type))
plat_n <- nrow(plat); plat_pct <- 100 * plat_n / n_scraper
plain_n   <- sum(plat$Platform_type == "Plain");    plain_pct   <- 100 * plain_n / plat_n
natural_n <- sum(plat$Platform_type == "Natural");  natural_pct <- 100 * natural_n / plat_n
dihedral_n<- sum(plat$Platform_type == "Dihedral"); dihedral_pct<- 100 * dihedral_n / plat_n
pd  <- as.numeric(plat$Platform_depth)
pd_median <- median(pd, na.rm = TRUE); pd_q1 <- quantile(pd, .25, na.rm = TRUE); pd_q3 <- quantile(pd, .75, na.rm = TRUE)
ipa <- as.numeric(plat$IPA); ipa_mean <- mean(ipa, na.rm = TRUE); ipa_sd <- sd(ipa, na.rm = TRUE)

cxs <- rowSums(cbind(as.numeric(q$E1_N_CxS), as.numeric(q$E2_N_CxS), as.numeric(q$E3_N_CxS)), na.rm = TRUE)
cvs <- rowSums(cbind(as.numeric(q$E1_N_CvS), as.numeric(q$E2_N_CvS), as.numeric(q$E3_N_CvS)), na.rm = TRUE)
rg  <- as.numeric(q$Ave_RG)
rg_mean  <- mean(rg, na.rm = TRUE);  rg_sd  <- sd(rg, na.rm = TRUE)
cxs_mean <- mean(cxs); cxs_sd <- sd(cxs); cvs_mean <- mean(cvs); cvs_sd <- sd(cvs)

giur <- as.numeric(q$Ave_GIUR); rli <- as.numeric(q$Retouch_length_index); ea <- as.numeric(q$Edge_Angle)
giur_median <- median(giur, na.rm = TRUE); giur_q1 <- quantile(giur, .25, na.rm = TRUE); giur_q3 <- quantile(giur, .75, na.rm = TRUE)
rli_median  <- median(rli,  na.rm = TRUE); rli_q1  <- quantile(rli,  .25, na.rm = TRUE); rli_q3  <- quantile(rli,  .75, na.rm = TRUE)
ea_mean <- mean(ea, na.rm = TRUE); ea_sd <- sd(ea, na.rm = TRUE)

spear <- function(v) {
  ok <- is.finite(ea) & is.finite(v)
  ct <- suppressWarnings(cor.test(ea[ok], v[ok], method = "spearman", exact = FALSE))
  c(rho = unname(ct$estimate), p = ct$p.value)
}
ea_cor  <- vapply(list(giur, rli, rg), spear, numeric(2))
ea_cor["p", ] <- p.adjust(ea_cor["p", ], "bonferroni")
ea_giur <- ea_cor[, 1]; ea_rli <- ea_cor[, 2]; ea_rg <- ea_cor[, 3]

# --------------------------------------------------------------------------
# 5. Resharpening flakes: EPA vs Quina scraper edge angle
# --------------------------------------------------------------------------
epa   <- as.numeric(rfl$EPA); pd_rf <- as.numeric(rfl$Platform_depth)
rf_n  <- sum(is.finite(epa))
rf_pd_mean <- mean(pd_rf, na.rm = TRUE); rf_pd_sd <- sd(pd_rf, na.rm = TRUE)
epa_mean   <- mean(epa,  na.rm = TRUE);  epa_sd   <- sd(epa,  na.rm = TRUE)
da <- bind_rows(tibble(Group = "edge", Value = ea), tibble(Group = "epa", Value = epa)) |>
  filter(!is.na(Value)) |> mutate(Group = factor(Group, levels = c("edge", "epa")))
welch  <- t_test(da, Value ~ Group, var.equal = FALSE, detailed = TRUE)
welch_t <- welch$statistic; welch_df <- welch$df; welch_p <- welch$p
set.seed(2226)
cohd_res <- cohens_d(da, Value ~ Group, var.equal = FALSE, ci = TRUE, nboot = B_BOOT)
cohd <- cohd_res$effsize; cohd_lo <- cohd_res$conf.low; cohd_hi <- cohd_res$conf.high

# --------------------------------------------------------------------------
# 6. Technological consistency: surface vs Longtan
# --------------------------------------------------------------------------
grp_levels <- c("SC_Quina", "LT_Quina", "LT_Ordinary")
read_grp <- function(path, sheet, g, site = FALSE) {
  df <- read_excel(path, sheet = sheet)
  df$Group   <- g
  df$Site_ID <- if (site) trimws(as.character(df$Site_ID)) else NA_character_
  df |> mutate(across(all_of(variables), as.numeric)) |> select(Group, Site_ID, all_of(variables))
}
scraper_all <- bind_rows(
  read_grp(sc_path, "Quina scraper",    "SC_Quina",    TRUE),
  read_grp(lt_path, "Quina scraper",    "LT_Quina",    FALSE),
  read_grp(lt_path, "Ordinary scraper", "LT_Ordinary", FALSE)) |>
  mutate(Group = factor(Group, levels = grp_levels))
complete_data <- scraper_all |> filter(if_all(all_of(variables), ~ !is.na(.x)))
n_by_group <- table(complete_data$Group)
ltq_n <- n_by_group[["LT_Quina"]]; lto_n <- n_by_group[["LT_Ordinary"]]
amat  <- complete_data |> select(all_of(variables)) |> scale() |> as.matrix()

pairwise_permanova <- function(data, m) {
  gs <- levels(droplevels(data$Group))
  bind_rows(lapply(combn(gs, 2, simplify = FALSE), function(pair) {
    rows <- data$Group %in% pair; pd <- data[rows, ]; pd$Group <- droplevels(pd$Group)
    set.seed(2226); mm <- adonis2(dist(m[rows, ]) ~ Group, data = pd, permutations = PERM)
    data.frame(Comparison = paste(pair, collapse = " vs "),
               n1 = sum(data$Group == pair[1]), n2 = sum(data$Group == pair[2]),
               Df = mm$Df[1], Df_res = mm$Df[2],
               SumOfSqs = mm$SumOfSqs[1], SumOfSqs_res = mm$SumOfSqs[2],
               R2 = mm$R2[1], F = mm$F[1], p_value = mm$`Pr(>F)`[1])
  })) |> mutate(p_adjusted = p.adjust(p_value, "bonferroni"))
}
tc_pw <- pairwise_permanova(complete_data, amat)
pwget <- function(comp, col) tc_pw[[col]][tc_pw$Comparison == comp]
tc_scltq_R2 <- pwget("SC_Quina vs LT_Quina", "R2")
tc_scltq_F  <- pwget("SC_Quina vs LT_Quina", "F")
tc_scltq_p  <- pwget("SC_Quina vs LT_Quina", "p_adjusted")
tc_nonq_R2  <- c(pwget("SC_Quina vs LT_Ordinary", "R2"),
                 pwget("LT_Quina vs LT_Ordinary", "R2"))
tc_nonq_F   <- c(pwget("SC_Quina vs LT_Ordinary", "F"),
                 pwget("LT_Quina vs LT_Ordinary", "F"))
tc_nonq_p   <- max(pwget("SC_Quina vs LT_Ordinary", "p_adjusted"),
                   pwget("LT_Quina vs LT_Ordinary", "p_adjusted"))

# per-variable tests + post-hoc brackets (panel B of the figure)
variable_long <- complete_data |> select(Group, all_of(variables)) |>
  pivot_longer(all_of(variables), names_to = "Variable", values_to = "Value") |>
  mutate(Variable = factor(Variable, levels = variables))
kw_vars    <- c("Ave_GIUR", "N_Scar", "Ave_RG")
welch_vars <- c("Retouch_length_index", "Thickness", "Edge_Angle")
kw_ph <- variable_long |> filter(Variable %in% kw_vars) |> mutate(Variable = droplevels(Variable)) |>
  group_by(Variable) |> dunn_test(Value ~ Group, p.adjust.method = "bonferroni") |> ungroup()
we_ph <- variable_long |> filter(Variable %in% welch_vars) |> mutate(Variable = droplevels(Variable)) |>
  group_by(Variable) |> pairwise_t_test(Value ~ Group, pool.sd = FALSE, p.adjust.method = "bonferroni") |> ungroup()
posthoc_brackets <- bind_rows(
  kw_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif),
  we_ph |> transmute(Variable, group1, group2, p.adj, p.adj.signif)) |>
  mutate(Variable = factor(as.character(Variable), levels = variables)) |>
  group_by(Variable) |> mutate(step = row_number()) |> ungroup() |>
  left_join(variable_long |> group_by(Variable) |>
              summarise(ymax = max(Value, na.rm = TRUE),
                        yrange = diff(range(Value, na.rm = TRUE)), .groups = "drop"),
            by = "Variable") |>
  mutate(y.position = ymax + yrange * (0.06 + 0.10 * step))

kw_omni <- variable_long |> filter(Variable %in% kw_vars) |> mutate(Variable = droplevels(Variable)) |>
  group_by(Variable) |> kruskal_test(Value ~ Group) |> ungroup()
kw_eff  <- variable_long |> filter(Variable %in% kw_vars) |> mutate(Variable = droplevels(Variable)) |>
  group_by(Variable) |> kruskal_effsize(Value ~ Group) |> ungroup()
we_omni <- variable_long |> filter(Variable %in% welch_vars) |> mutate(Variable = droplevels(Variable)) |>
  group_by(Variable) |> welch_anova_test(Value ~ Group) |> ungroup()

# PERMDISP (Block A) on the z-scored technological space -> means, pairwise, PCA
run_permdisp <- function(df) {
  mvd <- df |> filter(if_all(all_of(variables), is.finite)) |> mutate(Group = droplevels(Group))
  mat <- scale(as.matrix(mvd[, variables])); d <- dist(mat)
  set.seed(2226); bd <- betadisper(d, mvd$Group)
  set.seed(2226); pt <- permutest(bd, permutations = PERM, pairwise = TRUE)
  pca <- prcomp(mat, center = TRUE, scale. = FALSE); vexp <- pca$sdev^2 / sum(pca$sdev^2) * 100
  # permutest() gives pairwise p but no pairwise F, so each pair is re-run on a
  # subset of the whole distance matrix, keeping the technological space fixed
  pair_tab <- bind_rows(lapply(combn(levels(mvd$Group), 2, simplify = FALSE), function(pr) {
    rows <- mvd$Group %in% pr
    set.seed(2226); b2 <- betadisper(dist(mat[rows, ]), droplevels(mvd$Group[rows]))
    set.seed(2226); p2 <- permutest(b2, permutations = PERM)
    data.frame(Comparison = paste(pr, collapse = "-"),
               n1 = sum(mvd$Group == pr[1]), n2 = sum(mvd$Group == pr[2]),
               Df = p2$tab$Df[1], Df_res = p2$tab$Df[2],
               SumOfSqs = p2$tab$`Sum Sq`[1], SumOfSqs_res = p2$tab$`Sum Sq`[2],
               F = p2$tab$F[1], p = p2$tab$`Pr(>F)`[1])
  }))
  list(means = tapply(bd$distances, mvd$Group, mean), overall_p = pt$tab$`Pr(>F)`[1],
       overall_F = pt$tab$F[1], overall_df = pt$tab$Df[1], overall_df_res = pt$tab$Df[2],
       distances = data.frame(Group = mvd$Group, distance = unname(bd$distances)),
       pairwise = pt$pairwise$permuted, pair_tab = pair_tab, n_by_group = table(mvd$Group),
       pca_scores = data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Group = mvd$Group),
       pca_loadings = data.frame(Variable = rownames(pca$rotation), pca$rotation, row.names = NULL),
       sdev = pca$sdev, var_explained = vexp)
}
pdget <- function(pd, comp, col) pd$pair_tab[[col]][pd$pair_tab$Comparison == comp]
permA <- run_permdisp(scraper_all)
pd_scq <- permA$means[["SC_Quina"]]; pd_ltq <- permA$means[["LT_Quina"]]; pd_lto <- permA$means[["LT_Ordinary"]]
permdisp_scltq_F <- pdget(permA, "SC_Quina-LT_Quina", "F")
permdisp_scltq_p <- pdget(permA, "SC_Quina-LT_Quina", "p")
permdisp_nonq_F  <- c(pdget(permA, "SC_Quina-LT_Ordinary", "F"), pdget(permA, "LT_Quina-LT_Ordinary", "F"))
permdisp_nonq_p  <- max(pdget(permA, "SC_Quina-LT_Ordinary", "p"), pdget(permA, "LT_Quina-LT_Ordinary", "p"))
# sensitivity: drop surface pieces from within the Longtan site area and repeat
scraper_excl  <- scraper_all |> filter(!(Group == "SC_Quina" & Site_ID %in% "LT"))
permC <- run_permdisp(scraper_excl)
sens_scltq_F <- pdget(permC, "SC_Quina-LT_Quina", "F")
sens_scltq_p <- pdget(permC, "SC_Quina-LT_Quina", "p")
sens_nonq_F  <- c(pdget(permC, "SC_Quina-LT_Ordinary", "F"), pdget(permC, "LT_Quina-LT_Ordinary", "F"))
sens_nonq_p  <- max(pdget(permC, "SC_Quina-LT_Ordinary", "p"), pdget(permC, "LT_Quina-LT_Ordinary", "p"))

complete_excl <- scraper_excl |> filter(if_all(all_of(variables), ~ !is.na(.x))) |>
  mutate(Group = droplevels(Group))
amat_excl <- complete_excl |> select(all_of(variables)) |> scale() |> as.matrix()
tc_pw_excl <- pairwise_permanova(complete_excl, amat_excl)
pwgetE <- function(comp, col) tc_pw_excl[[col]][tc_pw_excl$Comparison == comp]
sens_perm_scltq_R2 <- pwgetE("SC_Quina vs LT_Quina", "R2")
sens_perm_scltq_F  <- pwgetE("SC_Quina vs LT_Quina", "F")
sens_perm_scltq_p  <- pwgetE("SC_Quina vs LT_Quina", "p_adjusted")
sens_perm_nonq_R2  <- c(pwgetE("SC_Quina vs LT_Ordinary", "R2"),
                        pwgetE("LT_Quina vs LT_Ordinary", "R2"))
sens_perm_nonq_F   <- c(pwgetE("SC_Quina vs LT_Ordinary", "F"),
                        pwgetE("LT_Quina vs LT_Ordinary", "F"))
sens_perm_nonq_p   <- max(pwgetE("SC_Quina vs LT_Ordinary", "p_adjusted"),
                          pwgetE("LT_Quina vs LT_Ordinary", "p_adjusted"))

# univariate dispersion: KL-MSLRT for the CV family, Fligner-Killeen for the
# bounded indices and the counts
disp_vars <- c("Length", "Width", "Thickness", "Mass", "Edge_Angle",
               "Ave_GIUR", "Retouch_length_index", "N_Scar", "Ave_RG")
read_disp <- function(path, g) read_excel(path, sheet = "Quina scraper") |>
  mutate(Group = g, across(all_of(disp_vars), as.numeric)) |> select(Group, all_of(disp_vars))
disp_all <- bind_rows(read_disp(sc_path, "SC_Quina"), read_disp(lt_path, "LT_Quina"))

finite  <- function(x) x[is.finite(x)]
cv_corr <- function(x) { x <- finite(x); (sd(x) / mean(x)) * (1 + 1 / (4 * length(x))) }  # Sokal-Rohlf
fano    <- function(x) { x <- finite(x); var(x) / mean(x) }
boot_ratio_ci <- function(a, b, FUN, B = B_BOOT) {
  a <- finite(a); b <- finite(b)
  unname(quantile(replicate(B, FUN(sample(a, replace = TRUE)) / FUN(sample(b, replace = TRUE))),
                  c(.025, .975), na.rm = TRUE))
}
# mslr_test is Monte Carlo; its RNG use is insulated so the bootstrap intervals
# do not depend on how many tests ran before it
cv_equal_test <- function(a, b) {
  a <- finite(a); b <- finite(b)
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv))
  set.seed(2226)
  ml <- mslr_test(nr = MSLR_NR, x = c(a, b), y = rep(c("SC", "LT"), c(length(a), length(b))))
  c(stat = unname(ml$MSLRT), p = unname(ml$p_value))
}
fligner_pair <- function(a, b) {
  a <- finite(a); b <- finite(b)
  ft <- fligner.test(c(a, b), factor(rep(c("SC", "LT"), c(length(a), length(b)))))
  c(stat = unname(ft$statistic), p = unname(ft$p.value))
}
disp_stat <- function(v, family) {
  a <- disp_all[[v]][disp_all$Group == "SC_Quina"]
  b <- disp_all[[v]][disp_all$Group == "LT_Quina"]
  FUN <- switch(family, CV = cv_corr, bounded = function(z) mad(finite(z)), count = fano)
  set.seed(2226); ci <- boot_ratio_ci(a, b, FUN)
  tt <- if (family == "CV") cv_equal_test(a, b) else fligner_pair(a, b)
  data.frame(variable = v, family = family, sc = FUN(a), lt = FUN(b),
             ratio = FUN(a) / FUN(b), lo = ci[1], hi = ci[2],
             stat = unname(tt["stat"]), p = unname(tt["p"]))
}
disp_tests <- bind_rows(
  lapply(c("Length", "Width", "Thickness", "Mass", "Edge_Angle"), disp_stat, family = "CV"),
  lapply(c("Ave_GIUR", "Retouch_length_index"), disp_stat, family = "bounded"),
  lapply(c("N_Scar", "Ave_RG"), disp_stat, family = "count"))
disp_min_p <- min(disp_tests$p)

tc <- list(pca_scores = permA$pca_scores, pca_loadings = permA$pca_loadings,
           var_explained = permA$var_explained, n_by_group = permA$n_by_group,
           variable_long = variable_long, posthoc_brackets = posthoc_brackets,
           permanova_pairwise = tc_pw,
           permdisp = list(means = permA$means, overall_p = permA$overall_p, pairwise = permA$pairwise))

# --------------------------------------------------------------------------
# 7. Landscape structure of technological variation (locality-level)
#    The response set here is the six technological measures PLUS Quina scraper
#    size, GM = (length * width * thickness)^(1/3): seven measures in all. GM
#    enters in this section only; `variables`, the six-measure consistency space
#    of section 6, is left untouched.
#    The second basin is "Heqing" in Site_information.xlsx and "Huangping" in
#    the prose; the prose name is the one reported.
# --------------------------------------------------------------------------
land_variables <- c("GM", variables)
variable_labels_land <- c(GM = "Quina scraper size (mm)", variable_labels)
variable_labels_land_bare <- sub(" \\(.*\\)$", "", variable_labels_land)
names(variable_labels_land_bare) <- names(variable_labels_land)
basin_levels_land <- c("Binchuan", "Huangping")

site_land <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            Name = gsub("_", " ", trimws(as.character(name))),
            Basin = factor(recode(strip_basin(basin), Heqing = "Huangping"),
                           levels = basin_levels_land),
            Distance_to_water = as.numeric(d_river_m),
            Height_above_river = as.numeric(h_river_m))
scL <- q |>
  mutate(Site_ID = trimws(as.character(Site_ID)),
         across(all_of(c(variables, "Length", "Width")), as.numeric),
         GM = (Length * Width * Thickness)^(1 / 3)) |>
  left_join(site_land, by = "Site_ID") |>
  filter(if_all(all_of(land_variables), ~ !is.na(.x)))
site_size <- scL |> count(Site_ID, name = "Site_size")
site_df <- scL |> group_by(Site_ID) |>
  summarise(n_art = n(), across(all_of(land_variables), ~ median(.x, na.rm = TRUE)),
            Name = first(Name),
            Basin = first(Basin), Distance_to_water = first(Distance_to_water),
            Height_above_river = first(Height_above_river), .groups = "drop") |>
  left_join(site_size, by = "Site_ID")
n_localities <- nrow(site_df)
ls_mat <- scale(as.matrix(site_df[, land_variables]))
dL <- dist(ls_mat)

# ---- each landscape variable in a model of its own ------------------------
grad_permanova <- function(col) {
  set.seed(2226); a <- adonis2(dL ~ site_df[[col]], permutations = PERM)
  c(R2 = a$R2[1], F = a$F[1], p = a$`Pr(>F)`[1], Df = a$Df[1], Df_res = a$Df[2],
    SumOfSqs = a$SumOfSqs[1], SumOfSqs_res = a$SumOfSqs[2])
}
ls_size   <- grad_permanova("Site_size")
ls_dist   <- grad_permanova("Distance_to_water")
ls_height <- grad_permanova("Height_above_river")
bdf <- site_df |> filter(!is.na(Basin))
set.seed(2226); aB <- adonis2(dist(scale(as.matrix(bdf[, land_variables]))) ~ Basin, data = bdf, permutations = PERM)
ls_basin <- c(R2 = aB$R2[1], F = aB$F[1], p = aB$`Pr(>F)`[1], Df = aB$Df[1], Df_res = aB$Df[2],
              SumOfSqs = aB$SumOfSqs[1], SumOfSqs_res = aB$SumOfSqs[2])
n_basin_site <- table(droplevels(site_df$Basin))
# the four one-variable models together, for the range quoted in the text
ls_perm <- list(Site_size = ls_size, Basin = ls_basin,
                Distance_to_water = ls_dist, Height_above_river = ls_height)
ls_R2 <- vapply(ls_perm, function(v) unname(v[["R2"]]), numeric(1))
ls_p  <- vapply(ls_perm, function(v) unname(v[["p"]]), numeric(1))

# ---- all four landscape variables in ONE model ----------------------------
# The omnibus tests the whole model against its residual; the marginal table
# gives each term after the other three. The design is collinear, so the
# marginal terms rank relative structure and are not independent effects.
land_form <- dL ~ Basin + Height_above_river + Distance_to_water + Site_size
set.seed(2226)
ls_full_omni <- adonis2(land_form, data = site_df, permutations = PERM, by = NULL)
set.seed(2226)
ls_full_margin <- adonis2(land_form, data = site_df, permutations = PERM, by = "margin")
ls_full <- c(R2 = ls_full_omni$R2[1], F = ls_full_omni$F[1], p = ls_full_omni$`Pr(>F)`[1],
             Df = ls_full_omni$Df[1], Df_res = ls_full_omni$Df[2],
             SumOfSqs = ls_full_omni$SumOfSqs[1], SumOfSqs_res = ls_full_omni$SumOfSqs[2])
ls_margin_terms <- rownames(ls_full_margin)[seq_along(ls_perm)]
ls_margin_p <- setNames(ls_full_margin$`Pr(>F)`[seq_along(ls_perm)], ls_margin_terms)
# every landscape p-value in the section: each term after the others, and each
# term alone. The text quotes the smallest of them.
ls_any_p_min <- min(c(ls_margin_p, ls_p), na.rm = TRUE)

# ---- the same model as a constrained ordination ---------------------------
# The response is z-scored and the distance Euclidean, so db-RDA on dL is an
# RDA on ls_mat: the ordination and the PERMANOVA are one geometry.
ls_rda <- rda(ls_mat ~ Basin + Height_above_river + Distance_to_water + Site_size,
              data = site_df)
ls_rda_r2 <- RsquareAdj(ls_rda)
set.seed(2226)
ls_rda_anova <- anova.cca(ls_rda, permutations = PERM)
ls_rda_vexp <- ls_rda$CCA$eig / ls_rda$tot.chi * 100   # per cent of TOTAL variance

# ---- unconstrained locality ordination, for the single-variable panels ----
site_pca <- prcomp(ls_mat, center = TRUE, scale. = FALSE)
site_vexp <- site_pca$sdev^2 / sum(site_pca$sdev^2) * 100
site_scores <- bind_cols(site_df, as.data.frame(site_pca$x[, 1:2]))
land_vars <- c(Height_above_river = "Height above channel (m)",
               Site_size          = "Assemblage size (n specimens)",
               Distance_to_water  = "Distance to channel (m)")
land_ranges <- lapply(names(land_vars), function(v) range(site_df[[v]], na.rm = TRUE))
names(land_ranges) <- names(land_vars)

# bootstrap interval on rho: at this n the interval, not the p-value, is what
# shows how little the coefficients pin down
ls_dist_cor <- bind_rows(lapply(land_variables, function(v) {
  d <- site_df[is.finite(site_df[[v]]) & is.finite(site_df$Distance_to_water), ]
  ct <- suppressWarnings(cor.test(d[[v]], d$Distance_to_water, method = "spearman", exact = FALSE))
  set.seed(2226)
  bs <- replicate(B_BOOT, {
    i <- sample(nrow(d), replace = TRUE)
    suppressWarnings(cor(d[[v]][i], d$Distance_to_water[i], method = "spearman"))
  })
  ci <- unname(quantile(bs, c(.025, .975), na.rm = TRUE))
  data.frame(variable = v, n = nrow(d), rho = unname(ct$estimate),
             lo = ci[1], hi = ci[2], p = ct$p.value)
}))
ls_dist_cor$p_adj <- p.adjust(ls_dist_cor$p, "bonferroni")
ls_dist_rho <- setNames(ls_dist_cor$rho, ls_dist_cor$variable)
