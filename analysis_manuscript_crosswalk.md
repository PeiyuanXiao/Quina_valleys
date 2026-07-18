# Analysis ↔ manuscript crosswalk

Maps every **Results** paragraph in `paper/manuscript.qmd` to the script(s) and output that
produce it, then lists the two gaps you asked about:

- **A.** analyses that exist in the scripts but are **not reported** in the manuscript;
- **B.** results **reported** in the manuscript that are **not produced by any script**.

Scripts are grouped under `scripts/<section>/`; each writes to the matching `output/<section>/`.
Last reconciled: 2026-07-16; script tree reorganized/renamed 2026-07-17 (folders →
`raw_material_analysis/`, `scraper_analysis/`, `technological_consistency/`; the former
`QV_analysis.R` + `QV_dispersion_LT_vs_SC.R` merged into `surface_vs_longtan.R`). The trachyte
"xx%" Results number is still a placeholder.

> **Output format change (2026-07-16).** The analysis scripts **no longer write CSV files.**
> Each script now prints its numbers to the **console** when run, and saves **figures (PNG)**
> only. The "Key output" column below therefore lists the PNG figure(s) for each claim; the
> reported statistics are read from the script's console output. `output/` holds figures only.

---

## 1. Results → script / output (what IS covered)

### Results 6.1 — Quina 遗址的景观分布格局与原料经济 (landscape distribution & raw-material economy)

| Manuscript claim | Script | Key output (figure / console) |
|---|---|---|
| Site distance-to-river by basin & valley; median 596 m; basin p=0.262 (Mann–Whitney), valley p=0.124 (Kruskal–Wallis) | `raw_material_analysis/raw_material_distance.R` | `dist_river_by_group/dist_river_by_group.png`; medians & test p-values printed to console |
| Trachyte is the dominant tool raw material | `raw_material_analysis/raw_material_electivity.R` | `raw_material_stacked_bar.png`; composition table printed to console |
| **Trachyte clasts significantly larger than sandstone (U = 16864.00, p < 0.001)** | `raw_material_analysis/rawmat_size_compare.R` | `clast_size_by_material/clast_size_by_material.png`; per-material geometric means + U/p/r printed to console |
| River-gravel composition is spatially heterogeneous: PERMANOVA R²=10.8%, p=0.001; between-valley p=0.001 | `raw_material_analysis/raw_material_permanova.R` (dataset **C**, clast level) | `raw_material_permanova/C_clast_variance_partition.png`, `C_clast_pcoa_scatter.png`; R²/p printed to console |
| Tool raw material is spatially uniform: basin R²=0.004 p=0.54; valley R²=0.005 p=0.75 | `raw_material_analysis/raw_material_permanova.R` (dataset **A**, tools) | `raw_material_permanova/A_tools_composition_by_basin.png`, `A_tools_composition_by_river.png`; R²/p printed to console |
| Jacobs electivity: trachyte D=0.97, sandstone D=−0.98 | `raw_material_analysis/raw_material_electivity.R` | `jacobs_electivity_index.png`; D values printed to console |

### Results 6.2.1 — 技术类型特征 (techno-typological features)

| Manuscript claim | Script | Key output (figure / console) |
|---|---|---|
| Edge angle correlates (positively, monotonically) with GIUR, retouch ratio, retouch generation | `scraper_analysis/attribute_correlations.R` | `spearman_EdgeAngle_scatter.png`; rho/p printed to console |
| Resharpening-flake EPA (mean 72.8°) ≈ scraper edge angle (mean 70.0°) | `scraper_analysis/QSEA_vs_RFEPA.R` | `edgeangle_epa_welch_boxplot.png`; group means + Welch t printed to console |

### Results 6.2.2 — 技术一致性分析 (technical consistency)

| Manuscript claim | Script | Key output (figure / console) |
|---|---|---|
| **Specimen level**: SC vs LT Quina not different (R²=0.004); both differ from ordinary scrapers (pairwise R²=0.29–0.36); per-variable tests | `technological_consistency/surface_vs_longtan.R` (Part 1, location) | `variable_boxplots.png`; PERMANOVA / Kruskal / Welch printed to console |
| **Specimen level**: dispersion equal between the two Quina groups, PERMDISP p=0.53 | `technological_consistency/surface_vs_longtan.R` (Part 2, dispersion) | `dispersion_LT_vs_SC/multivariate_permdisp/*.png`; PERMDISP p printed to console |
| **Site–landscape level**: technical variation not structured by basin (R²=0.011), landform (0.073), distance (0.031), site size (0.006) | `technological_consistency/landscape_structure.R` | `analysis1_basin/…analysis4_size/*.png`; multivariate R²/p printed to console |

---

## 2. Gap list A — in the scripts, NOT reported in the manuscript

These analyses run and produce a figure / console output but no corresponding number appears in
the current Results text. Decide per item whether to add it to the paper or drop it from the
pipeline.

1. **`technological_consistency/surface_vs_longtan.R` (Part 2)** — beyond the PERMDISP p=0.53
   headline, the whole per-variable dispersion machinery is unreported: CV family
   (`cv_dimensional/`), robust-reduction family (`robust_reduction/`), the Krishnamoorthy–Lee
   MSLRT CV-equality test, and the independence sensitivity forests (`sensitivity/`).
2. **`technological_consistency/landscape_structure.R`** — only the four R² headlines are cited. The
   per-variable Kruskal/Welch tests, PCA ordinations, boxplots, the `cross_analysis/` collinearity
   check, the Heqing-composition diagnostic, and the sensitivity summary are all unreported.
3. **`scraper_analysis/attribute_correlations.R`** — the `Ave_RG` and
   `Section_asymmetric` Spearman sets (`spearman_AveRG_scatter.png`,
   `spearman_Section_asymmetric_scatter.png`) are not cited; only the `Edge_Angle` set is.
4. **`raw_material_analysis/raw_material_permanova.R`** — dataset **B** (locality-level
   Aitchison/CLR biplot, `B_avail_*`) is not cited; the paper uses only the clast-level (C) and
   tool-level (A) results.
5. *(Resolved 2026-07-17.)* The former duplicate 3-group `betadisper` (in the old `QV_analysis.R`)
   was removed; PERMDISP now lives only in `surface_vs_longtan.R` Part 2 (the 2-group SC-vs-LT
   test, PERMDISP p=0.53), so there is no longer a redundant, uncited dispersion computation here.
6. **Redundant figure variants (maps)** — two site maps (`01_map.R` plain terrain vs
   `01_map_tiles.R` OpenTopoMap) and three geology maps (`04_geology_map.R` schematic,
   `05_geology_map_scan.R` georeferenced scan, `06_geology_classified.R` 6-class), plus the
   Illustrator layer export (`07_illustrator_layers.R`). The manuscript has no figure callouts yet,
   so which variant is "the figure" is undecided.

*Resolved since the last reconciliation:* the point-pattern script `maps/02_spatial_stats.R`
(nearest-neighbour, Clark–Evans, Ripley's L, and the sites-vs-random distance-to-river Wilcoxon)
and its outputs were **deleted** on 2026-07-16, so the former Gap A item for it no longer applies.

---

## 3. Gap list B — reported in the manuscript, NOT produced by any script

These numbers/tables appear in Results but no script computes them. They need a script (or the
source of the number needs to be located) before the compendium is fully reproducible.

1. **Clast shape composition** — trachyte "板状 (n=34, 26.4%) / 不规则 (n=39, 30.2%)". No script
   tabulates clast shape. *(Data are available: `Raw_mat_basin.xlsx` has a `Shape` column.)*
2. **Techno-typological descriptive summaries (6.2.1)** — platform retention n=52 (31.3%), median
   platform depth 13.9 mm, mean IPA 122°, plain/natural/dihedral platform counts, 4 *talon à pan*,
   mean retouch generation 2.76, mean convex 4.4 / concave 18.7 scars, median GIUR 0.77, median
   edge-retouch ratio 0.46, retouch-flake platform depth 5.3 mm. No script emits these summary
   statistics from `Quina_scraper_surface.xlsx`. *(Only edge angle 70.0° and EPA 72.8° are scripted,
   in `QSEA_vs_RFEPA.R`.)*
3. **Bordes typology table** — "在 Bordes 类型学上有所体现 (表 [x])" incl. triple-scraper and
   *limace* counts. No script produces a Bordes type-count table.
4. **Assemblage counts / percentages** — the "166 scrapers" figure and the trachyte-in-tools "xx%"
   placeholder. The count is not scripted; the percentage is derivable from the electivity script's
   console composition output but is not stated as a single number there.

*Resolved since the last reconciliation:* the clast **size** test ("粗面岩砾石显著大于砂岩") is now
scripted in `rawmat_size_compare.R` (see §1). The manuscript number was updated from the
old Quartz-sandstone-only value (U=16356.00, p=0.003) to the harmonised-Sandstone value
(U=16864.00, p<0.001); see §4.

---

## 4. Minor discrepancies to reconcile (not gaps)

- The manuscript labels the basin distance test "Kruskal–Wallis (basin p=0.23)", but for two groups
  `raw_material_distance.R` runs **Mann–Whitney U** (the equivalent two-sample rank test; the
  Kruskal–Wallis label is correct only for the 3-river comparison, p=0.124). Align the wording.
- **Sandstone definition in the clast-size test.** The paper now reports the harmonised definition
  (Quartz + Coarse sandstone → "Sandstone"), consistent with the composition / electivity /
  PERMANOVA analyses (U=16864.00, p<0.001). `rawmat_size_compare.R` also prints the
  Quartz-sandstone-only result (U=16356.00, p=0.003) as a reconciliation with the earlier draft
  number; both give the same conclusion (trachyte significantly larger).
