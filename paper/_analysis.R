# _analysis.R -- every statistical result in the manuscript and supplementary.
# Sourced by both .qmd files; defines objects in memory, writes nothing.
# WdStar is not on CRAN: remotes::install_github("alekseyenko/WdStar").
# The Bayesian landscape model is NOT fitted here; see paper/barg/barg_fits.R.

library(tidyverse)
library(here)
library(readxl)
library(vegan)
library(coin)
library(rstatix)
library(ggpubr)
library(patchwork)
library(ggrepel)
library(grid)
library(cvequality)


set.seed(2226)
PERM    <- 9999
B_BOOT  <- 5000   # bootstrap replicates for every percentile interval
MSLR_NR <- 1e5    # Monte Carlo iterations for the KL-MSLRT

# ---- inline-number formatters ----
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
# non-breaking indent for sub-rows in tables
IND <- strrep(intToUtf8(160), 3)

sc_path    <- here("data", "Quina_scraper_surface.xlsx")
lt_path    <- here("data", "Longtan_lithic_tools.xlsx")
basin_path <- here("data", "Raw_mat_basin.xlsx")
site_path  <- here("data", "Site_information.xlsx")

material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone", "Andesite")
river_levels    <- c("Sangyuan", "Liandong", "Caifeng")
basin_levels    <- c("Binchuan", "Huangping")
variables       <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
                     "N_Scar", "Ave_RG", "Edge_Angle")

# ---- display labels ----
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

# ---- 1. Distance to river by basin and valley (rank tests) ----
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

# ---- 2. Raw-material composition and Jacobs' D ----
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

# ---- 3. Clast size and shape by raw material ----
raw <- read_excel(basin_path, "Sheet1"); names(raw) <- trimws(names(raw))
cl <- raw |>
  transmute(Lithology = trimws(as.character(Lithology)),
            Material = factor(harmonise_lithology(Lithology), levels = material_levels),
            L  = suppressWarnings(as.numeric(as.character(Length))),
            B  = suppressWarnings(as.numeric(as.character(Breadth))),
            Th = suppressWarnings(as.numeric(as.character(Thickness))),
            Shape = trimws(as.character(Shape))) |>
  mutate(size = (L * B * Th)^(1 / 3)) |>
  filter(is.finite(size), L > 0, B > 0, Th > 0)

# ---- clast form from the axes: the field shape terms mix form with regularity ----
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

# ---- does form distinguish the two lithologies? ----
# the three axes as a composition, Aitchison distance, so size divides out;
# strictly positive, so no zero replacement
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
# sphericity is not tested: log psi is the clr coordinate of the short axis, already covered above

# ---- 4. Quina scraper techno-typology descriptives ----
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

# ---- 5. Resharpening flakes: platform descriptives ----
# The flake EPA vs scraper edge-angle comparison was dropped 2026-08-26. EPA is
# still recorded and tabulated, but no longer tested against anything.
pd_rf <- as.numeric(rfl$Platform_depth)
rf_pd_mean <- mean(pd_rf, na.rm = TRUE); rf_pd_sd <- sd(pd_rf, na.rm = TRUE)
# rf_n is the flakes with a measurable EPA, 56 of the 58
rf_n  <- sum(is.finite(as.numeric(rfl$EPA)))

# ---- 6. Technological consistency: surface vs Longtan ----
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
  # subset of the fixed distance matrix
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
       # Site_ID travels with the scores: the jackknife figure drops one
       # locality's specimens from this ordination without refitting it
       pca_scores = data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                               Group = mvd$Group, Site_ID = mvd$Site_ID),
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

# ---- Wd*: the same comparisons without the equal-dispersion assumption ----
# PERMDISP fails against the non-Quina scrapers, so a PERMANOVA rejection there
# could follow from spread rather than position. Same distances, grouping and
# permutations as above.
tc_pairs <- combn(grp_levels, 2, simplify = FALSE)
wds_of <- function(dm, g) {
  g <- droplevels(factor(g))
  set.seed(2226)
  tt <- WdStar::WdS.test(dm, g, nrep = PERM)
  # Wd* is undefined for a group with no internal dissimilarity; replay the same
  # permutations to record how often that happens
  undef <- NA_real_
  if (is.na(tt$p.value)) {
    set.seed(2226)
    undef <- mean(!is.finite(replicate(PERM, WdStar::WdS(dm, g[sample(length(g))]))))
  }
  c(stat = unname(tt$statistic), p = unname(tt$p.value), undef = undef)
}
wds_pair <- function(M, grp, pr) {
  rows <- grp %in% pr
  wds_of(dist(M[rows, ]), droplevels(grp[rows]))
}
wds_tc <- lapply(tc_pairs, function(pr) wds_pair(amat, complete_data$Group, pr))
names(wds_tc) <- vapply(tc_pairs, paste, character(1), collapse = " vs ")
# the Bonferroni family is the same three pairwise comparisons as above
wds_tc_padj <- p.adjust(vapply(wds_tc, function(x) x[["p"]], numeric(1)), "bonferroni")
wgetd <- function(comp, what) if (what == "p") unname(wds_tc_padj[comp]) else
  unname(wds_tc[[comp]][[what]])
wds_scltq_stat <- wgetd("SC_Quina vs LT_Quina", "stat")
wds_scltq_p    <- wgetd("SC_Quina vs LT_Quina", "p")
wds_nonq_stat  <- c(wgetd("SC_Quina vs LT_Ordinary", "stat"),
                    wgetd("LT_Quina vs LT_Ordinary", "stat"))
wds_nonq_p     <- max(wgetd("SC_Quina vs LT_Ordinary", "p"),
                      wgetd("LT_Quina vs LT_Ordinary", "p"))

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

# ---- equivalence in ratio form (locality-level cluster bootstrap) ----
# rho = |mean(SC_Q) - mean(LT_Q)| / |mean(LT_Q) - mean(LT_nonQ)|; the multivariate
# rho uses centroid distances in the six-variable z-scored space (amat).
eq_gi  <- split(seq_len(nrow(complete_data)), complete_data$Group)
eq_M   <- as.matrix(complete_data[, variables])
eq_Z   <- amat
eq_Msc <- eq_M[eq_gi$SC_Quina, ]; eq_Mlq <- eq_M[eq_gi$LT_Quina, ]; eq_Mlo <- eq_M[eq_gi$LT_Ordinary, ]
eq_Zsc <- eq_Z[eq_gi$SC_Quina, ]; eq_Zlq <- eq_Z[eq_gi$LT_Quina, ]; eq_Zlo <- eq_Z[eq_gi$LT_Ordinary, ]

eq_loc      <- complete_data$Site_ID[eq_gi$SC_Quina]
eq_loc_rows <- split(seq_along(eq_loc), eq_loc)
eq_n_loc    <- length(eq_loc_rows)
eq_n_lq     <- nrow(eq_Mlq); eq_n_lo <- nrow(eq_Mlo)

eq_eu <- function(a, b) sqrt(sum((a - b)^2))
eq_cm <- function(X, i) colMeans(X[i, , drop = FALSE])

eq_m_sc <- colMeans(eq_Msc); eq_m_lq <- colMeans(eq_Mlq); eq_m_lo <- colMeans(eq_Mlo)
eq_num0 <- abs(eq_m_sc - eq_m_lq); eq_den0 <- abs(eq_m_lq - eq_m_lo)
eq_rho0 <- eq_num0 / eq_den0

eq_zc_sc <- colMeans(eq_Zsc); eq_zc_lq <- colMeans(eq_Zlq); eq_zc_lo <- colMeans(eq_Zlo)
eq_num0_m <- eq_eu(eq_zc_sc, eq_zc_lq); eq_den0_m <- eq_eu(eq_zc_lq, eq_zc_lo)
eq_rho0_m <- eq_num0_m / eq_den0_m

eq_K <- length(variables)
set.seed(2226)
eq_rho_b <- matrix(NA_real_, B_BOOT, eq_K + 1)
for (b in seq_len(B_BOOT)) {
  i_sc <- unlist(eq_loc_rows[sample.int(eq_n_loc, eq_n_loc, replace = TRUE)], use.names = FALSE)
  i_lq <- sample.int(eq_n_lq, eq_n_lq, replace = TRUE)
  i_lo <- sample.int(eq_n_lo, eq_n_lo, replace = TRUE)
  msc <- eq_cm(eq_Msc, i_sc); mlq <- eq_cm(eq_Mlq, i_lq); mlo <- eq_cm(eq_Mlo, i_lo)
  eq_rho_b[b, 1:eq_K] <- abs(msc - mlq) / abs(mlq - mlo)
  zsc <- eq_cm(eq_Zsc, i_sc); zlq <- eq_cm(eq_Zlq, i_lq); zlo <- eq_cm(eq_Zlo, i_lo)
  eq_rho_b[b, eq_K + 1] <- eq_eu(zsc, zlq) / eq_eu(zlq, zlo)
}

eq_qq <- function(x, p) unname(quantile(x, p, na.rm = TRUE))
eq_UB95 <- apply(eq_rho_b, 2, eq_qq, 0.95)
eq_UB95_multi <- eq_UB95[eq_K + 1]
eq_UB95_pct   <- round(eq_UB95_multi * 100)

# p-value curve bootstrap (higher R for finer resolution)
R_PC <- 20000
set.seed(2226)
eq_rho_pc <- matrix(NA_real_, R_PC, eq_K + 1)
# the same replicate's raw difference, so its interval can be set beside TOSTER's (Table S8)
eq_diff_pc <- matrix(NA_real_, R_PC, eq_K, dimnames = list(NULL, variables))
for (b in seq_len(R_PC)) {
  i_sc <- unlist(eq_loc_rows[sample.int(eq_n_loc, eq_n_loc, replace = TRUE)], use.names = FALSE)
  i_lq <- sample.int(eq_n_lq, eq_n_lq, replace = TRUE)
  i_lo <- sample.int(eq_n_lo, eq_n_lo, replace = TRUE)
  msc <- eq_cm(eq_Msc, i_sc); mlq <- eq_cm(eq_Mlq, i_lq); mlo <- eq_cm(eq_Mlo, i_lo)
  eq_diff_pc[b, ] <- msc - mlq
  eq_rho_pc[b, 1:eq_K] <- abs(msc - mlq) / abs(mlq - mlo)
  zsc <- eq_cm(eq_Zsc, i_sc); zlq <- eq_cm(eq_Zlq, i_lq); zlo <- eq_cm(eq_Zlo, i_lo)
  eq_rho_pc[b, eq_K + 1] <- eq_eu(zsc, zlq) / eq_eu(zlq, zlo)
}
# the 90% interval, the one a TOST at alpha = 0.05 inverts
eq_diff0    <- eq_m_sc - eq_m_lq
eq_diff_ci90 <- apply(eq_diff_pc, 2, eq_qq, c(0.05, 0.95))

eq_pc_lev <- c(unname(variable_labels[variables]), "Multivariate (centroid)")
eq_dgrid  <- seq(0, 1.2, by = 0.005)
eq_pcurve <- do.call(rbind, lapply(seq_len(eq_K + 1), function(j)
  data.frame(Measure = eq_pc_lev[j], Delta = eq_dgrid,
             p = vapply(eq_dgrid, function(d) mean(eq_rho_pc[, j] >= d), numeric(1)))))
eq_pcurve$Measure <- factor(eq_pcurve$Measure, levels = eq_pc_lev)

eq_pc_d05 <- apply(eq_rho_pc, 2, eq_qq, 0.95)
eq_pc_p1  <- apply(eq_rho_pc, 2, function(x) mean(x >= 1))

eq_pooled_sd <- function(a, b) {
  na <- length(a); nb <- length(b)
  sqrt(((na - 1) * var(a) + (nb - 1) * var(b)) / (na + nb - 2))
}
eq_d_discrim <- vapply(variables,
  function(v) abs((mean(eq_Mlq[, v]) - mean(eq_Mlo[, v])) /
                    eq_pooled_sd(eq_Mlq[, v], eq_Mlo[, v])), numeric(1))

# ---- leave-one-locality-out jackknife on the surface group ----
# The standardisation is fitted once on the full pooled data and held fixed, so
# the geometry does not move with the specimens dropped. LT_Quina vs LT_Ordinary
# is constant across iterations but stays in the Bonferroni family of three.
jk_localities <- sort(unique(complete_data$Site_ID[complete_data$Group == "SC_Quina" &
                                                     !is.na(complete_data$Site_ID)]))
jk_pmv <- function(keep, g1, g2) {
  sel <- keep & complete_data$Group %in% c(g1, g2)
  sub <- data.frame(Group = droplevels(complete_data$Group[sel]))
  set.seed(2226)
  a <- adonis2(dist(amat[sel, ]) ~ Group, data = sub, permutations = PERM)
  c(n1 = sum(sel & complete_data$Group == g1), R2 = a$R2[1], F = a$F[1],
    p = a$`Pr(>F)`[1])
}
jk_keep_all <- rep(TRUE, nrow(complete_data))
jk_p_lq_lo  <- unname(jk_pmv(jk_keep_all, "LT_Quina", "LT_Ordinary")["p"])
jk_one <- function(keep, dropped) {
  a <- jk_pmv(keep, "SC_Quina", "LT_Quina")
  b <- jk_pmv(keep, "SC_Quina", "LT_Ordinary")
  padj <- p.adjust(c(a[["p"]], b[["p"]], jk_p_lq_lo), "bonferroni")
  data.frame(Dropped = dropped, n_dropped = sum(!keep), n_SC = a[["n1"]],
             R2_q = a[["R2"]], F_q = a[["F"]], p_q = a[["p"]], padj_q = padj[1],
             R2_o = b[["R2"]], F_o = b[["F"]], p_o = b[["p"]], padj_o = padj[2])
}
jack <- bind_rows(
  jk_one(jk_keep_all, "(full data)"),
  bind_rows(lapply(jk_localities, function(l)
    jk_one(!(complete_data$Group == "SC_Quina" & complete_data$Site_ID %in% l), l))))
jack_i <- jack[-1, ]                       # the jackknife iterations alone
jk_n          <- nrow(jack_i)
jk_R2_q_range <- range(jack_i$R2_q)
jk_R2_o_range <- range(jack_i$R2_o)
jk_F_o_range  <- range(jack_i$F_o)
jk_p_o_max    <- max(jack_i$p_o)
jk_F_q_range  <- range(jack_i$F_q)
jk_p_q_min    <- min(jack_i$p_q)
jk_worst_q    <- jack_i$Dropped[which.max(jack_i$R2_q)]
jk_flip_raw   <- sum(jack_i$p_q    <= 0.05)   # iterations that would reverse the
jk_flip_adj   <- sum(jack_i$padj_q <= 0.05)   # non-significant Quina-to-Quina result

# univariate dispersion: KL-MSLRT for the CV family, Fligner-Killeen elsewhere
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
# mslr_test is Monte Carlo; its RNG is insulated so the intervals do not depend on test order
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
  # retouch generations is left out: it takes so few distinct values that its
  # resampled MAD is zero often enough to leave the ratio interval unbounded.
  lapply(c("Ave_GIUR", "Retouch_length_index"), disp_stat, family = "bounded"),
  lapply("N_Scar", disp_stat, family = "count"))
disp_min_p <- min(disp_tests$p)

tc <- list(pca_scores = permA$pca_scores, pca_loadings = permA$pca_loadings,
           var_explained = permA$var_explained, n_by_group = permA$n_by_group,
           variable_long = variable_long, posthoc_brackets = posthoc_brackets,
           permanova_pairwise = tc_pw,
           permdisp = list(means = permA$means, overall_p = permA$overall_p, pairwise = permA$pairwise))

# ---- 7. Locality-level analysis frame ----
# Seven measures here: the six of section 6 plus GM, Quina scraper size, which
# enters only in this section. Builds site_df and stops; the landscape analysis
# itself is the Bayesian model under paper/barg/. Table S16 prints site_df.
land_variables <- c("GM", variables)
variable_labels_land <- c(GM = "Quina scraper size (mm)", variable_labels)

site_land <- sites |>
  transmute(Site_ID = trimws(as.character(Code)),
            Name = gsub("_", " ", trimws(as.character(name))),
            Basin = factor(strip_basin(basin), levels = basin_levels),
            Distance_to_water = as.numeric(d_river_m),
            Height_above_river = as.numeric(h_river_m))
scL <- q |>
  mutate(Site_ID = trimws(as.character(Site_ID)),
         across(all_of(c(variables, "Length", "Width")), as.numeric),
         GM = (Length * Width * Thickness)^(1 / 3)) |>
  left_join(site_land, by = "Site_ID") |>
  filter(if_all(all_of(land_variables), ~ !is.na(.x)))
site_df <- scL |> group_by(Site_ID) |>
  summarise(n_art = n(), across(all_of(land_variables), ~ median(.x, na.rm = TRUE)),
            Name = first(Name),
            Basin = first(Basin), Distance_to_water = first(Distance_to_water),
            Height_above_river = first(Height_above_river), .groups = "drop")
n_localities <- nrow(site_df)
