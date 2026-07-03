# Analysis ↔ manuscript crosswalk

Maps every **Results** paragraph in `paper/manuscript.qmd` to the script(s) and output that
produce it, then lists the two gaps you asked about:

- **A.** analyses that exist in the scripts but are **not reported** in the manuscript;
- **B.** results **reported** in the manuscript that are **not produced by any script**.

Scripts are grouped under `scripts/<section>/`; each writes to the matching `output/<section>/`.
Last reconciled: 2026-07-02 (against the current draft; several Results numbers are still
placeholders, e.g. trachyte "xx%").

---

## 1. Results → script / output (what IS covered)

### Results 6.1 — Quina 遗址的景观分布格局与原料经济 (landscape distribution & raw-material economy)

| Manuscript claim | Script | Key output |
|---|---|---|
| Site distance-to-river by basin & valley; median 604 m; basin p=0.23, valley p=0.124 | `01_landscape_raw_material/QV_dist_river_by_group.R` | `output/01_landscape_raw_material/dist_river_by_group/` (`descriptives_by_river.csv`, `basin_mannwhitney.csv`, `river_kruskal.csv`) |
| Trachyte is the dominant tool raw material | `01_landscape_raw_material/QV_raw_material_electivity.R` | `raw_material_composition.csv`, `raw_material_stacked_bar.png` |
| River-gravel composition is spatially heterogeneous: PERMANOVA R²=10.8%, p=0.001; between-valley p=0.001 | `01_landscape_raw_material/QV_raw_material_permanova.R` (dataset **C**, clast level) | `raw_material_permanova/C_clast_permanova_*.csv`, `C_clast_variance_partition.csv` |
| Tool raw material is spatially uniform: basin R²=0.004 p=0.54; valley R²=0.005 p=0.75 | `01_landscape_raw_material/QV_raw_material_permanova.R` (dataset **A**, tools) | `raw_material_permanova/A_tools_permanova_basin.csv`, `A_tools_permanova_river_ID.csv` |
| Jacobs electivity: trachyte D=0.97, sandstone D=−0.98 | `01_landscape_raw_material/QV_raw_material_electivity.R` | `jacobs_electivity_index.csv`, `jacobs_electivity_index.png` |

### Results 6.2.1 — 技术类型特征 (techno-typological features)

| Manuscript claim | Script | Key output |
|---|---|---|
| Edge angle correlates (positively, monotonically) with GIUR, retouch ratio, retouch generation | `02_scraper_characterization/Quina_scraper_statistic.R` | `spearman_EdgeAngle_correlations.csv`, `spearman_EdgeAngle_scatter.png` |
| Resharpening-flake EPA (mean 72.8°) ≈ scraper edge angle (mean 70.0°) | `02_scraper_characterization/QV_edge_angle_resharpening.R` | `edgeangle_epa_welch_t.csv`, `edgeangle_epa_welch_boxplot.png` |

### Results 6.2.2 — 技术一致性分析 (technical consistency)

| Manuscript claim | Script | Key output |
|---|---|---|
| **Specimen level**: SC vs LT Quina not different (R²=0.004); both differ from ordinary scrapers (pairwise R²=0.29–0.36); per-variable tests | `03_technical_consistency/QV_analysis.R` | `permanova_overall.csv`, `permanova_posthoc_pairwise.csv`, `pca_ordination_scraper_groups.png`, `kruskal_omnibus.csv`, `welch_*` |
| **Specimen level**: dispersion equal between the two Quina groups, PERMDISP p=0.53 | `03_technical_consistency/QV_dispersion_LT_vs_SC.R` | `dispersion_LT_vs_SC/permdisp_headline.csv`, `.../multivariate_permdisp/` |
| **Site–landscape level**: technical variation not structured by basin (R²=0.003), landform (0.019), distance (0.017), site size (0.015) | `03_technical_consistency/QV_landscape_triage.R` | `summary_multivariate_R2.csv`, `analysis1_basin/…analysis4_size/` |

---

## 2. Gap list A — in the scripts, NOT reported in the manuscript

These analyses run and produce output but no corresponding number/figure appears in the current
Results text. Decide per item whether to add it to the paper or drop it from the pipeline.

1. **`maps/02_spatial_stats.R`** — point-pattern clustering (Ripley's *L*, `ripley_L_binchuan.png`),
   nearest-neighbour distance (`nnd_histogram.png`), and the one-sided Wilcoxon test that sites sit
   closer to rivers than random points (`spatial_stats.csv`). None of these are reported; the paper
   describes site distribution only qualitatively.
2. **`03_technical_consistency/QV_dispersion_LT_vs_SC.R`** — beyond the PERMDISP p=0.53 headline, the
   whole per-variable dispersion machinery is unreported: CV family (`cv_dimensional/`),
   robust-reduction family (`robust_reduction/`), Feltz–Miller test, and the independence
   sensitivity forests (`sensitivity/`).
3. **`03_technical_consistency/QV_landscape_triage.R`** — only the four R² headlines are cited. The
   per-variable Kruskal/Welch tests, PCA ordinations, boxplots, the `cross_analysis/` collinearity
   check, the Heqing-composition table, and the sensitivity summary are all unreported.
4. **`02_scraper_characterization/Quina_scraper_statistic.R`** — the `Ave_RG` and
   `Section_asymmetric` Spearman sets (`spearman_AveRG_*`, `spearman_Section_asymmetric_*`) are not
   cited; only the `Edge_Angle` set is.
5. **`01_landscape_raw_material/QV_raw_material_permanova.R`** — dataset **B** (locality-level
   Aitchison/CLR biplot, `B_avail_*`) is not cited; the paper uses only the clast-level (C) and
   tool-level (A) results.
6. **`03_technical_consistency/QV_analysis.R`** — the 3-group `betadisper` (`betadisper_anova.csv`,
   `betadisper_distances.csv`) is not cited. The PERMDISP p=0.53 in the paper is the 2-group SC-vs-LT
   test from `QV_dispersion_LT_vs_SC.R`, a different analysis.
7. **Redundant figure variants (maps)** — two site maps (`01_map.R` plain terrain vs
   `01_map_tiles.R` OpenTopoMap) and three geology maps (`04_geology_map.R` schematic,
   `05_geology_map_scan.R` georeferenced scan, `06_geology_classified.R` 6-class), plus the
   Illustrator layer export (`07_illustrator_layers.R`). The manuscript has no figure callouts yet,
   so which variant is "the figure" is undecided.

---

## 3. Gap list B — reported in the manuscript, NOT produced by any script

These numbers/tables appear in Results but no script computes them. They need a script (or the
source of the number needs to be located) before the compendium is fully reproducible.

1. **Clast size test** — "粗面岩砾石显著大于砂岩 (U = 16356.00, p = 0.003)". No script runs a
   Mann–Whitney/size comparison on `Raw_mat_basin.xlsx` clast dimensions. *(Data are available; the
   test is not scripted.)*
2. **Clast shape composition** — trachyte "板状 (n=34, 26.4%) / 不规则 (n=39, 30.2%)". No script
   tabulates clast shape.
3. **Techno-typological descriptive summaries (6.2.1)** — platform retention n=52 (31.3%), median
   platform depth 13.9 mm, mean IPA 122°, plain/natural/dihedral platform counts, 4 *talon à pan*,
   mean retouch generation 2.76, mean convex 4.4 / concave 18.7 scars, median GIUR 0.77, median
   edge-retouch ratio 0.46, retouch-flake platform depth 5.3 mm. No script emits these summary
   statistics from `Quina_scraper_surface.xlsx`. *(Only edge angle 70.0° and EPA 72.8° are scripted,
   in `QV_edge_angle_resharpening.R`.)*
4. **Bordes typology table** — "在 Bordes 类型学上有所体现 (表 [x])" incl. triple-scraper and
   *limace* counts. No script produces a Bordes type-count table.
5. **Assemblage counts / percentages** — the "166 scrapers" figure and the trachyte-in-tools "xx%"
   placeholder. The count is not scripted; the percentage is derivable from
   `raw_material_composition.csv` but is not stated there.

---

## 4. Minor discrepancy to reconcile (not a gap)

- The manuscript labels the basin distance test "Kruskal–Wallis (basin p=0.23)", but for two groups
  `QV_dist_river_by_group.R` runs **Mann–Whitney U** (the equivalent two-sample rank test; the
  Kruskal–Wallis label is correct only for the 3-river comparison, p=0.124). Align the wording.
