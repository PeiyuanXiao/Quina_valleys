# Spatial analysis — honest interpretation notes

## Sample sizes and what is testable
- **Heqing** (n = 5): no point-pattern test is statistically meaningful; reported as **description only** ('n=5, not tested').
- **Binchuan** (n = 22): sites lie in a **near-linear arrangement along the valley/rivers**.

## Why 2-D CSR is the wrong null model (read before citing C.3/C.4)
- Clark-Evans (C.3) and Ripley's L (C.4) test against 2-D Complete Spatial Randomness.
- A linear, along-river distribution reads as 'significantly clustered' under CSR **purely because of the linear geometry**, not because of meaningful clustering.
- These two analyses are therefore **EXPLORATORY ONLY** and must not be cited as evidence of clustering.

## The defensible core spatial test: distance to river (C.5)
- Tests whether observed sites sit closer to rivers than random points in the same study window (one-sided Wilcoxon).
- This addresses a meaningful hypothesis (water-/raw-material-oriented placement) and replaces the uninformative 2-D CSR test.
- Reported per basin; compare against the thesis §3 median site-river distance (604 m).

## Other guardrails enforced
- **Lithic counts are NOT compared across basins.** Heqing includes excavated sites (LT, THC); higher counts reflect collection method (excavation vs surface survey), not occupation intensity. On the map, n_lithics is a visual size only; no cross-basin count test is run.
- **Geomorph x basin is descriptive only** (geomorph_basin_table.csv). Structural zeros (hilltop/T4 only Binchuan; T2 only Heqing) make any chi-square/Fisher test trivially significant but uninformative — no test performed.
- **Height-above-river (h_river_m) is descriptive only.** If terraces were defined by height, 'T4 higher than T3' is circular; h_river is used only to separate near-river terraces from uplifted hilltops.

## Data note
- geomorph in Site_information.xlsx was corrupted (mojibake) and was recovered from station codes via guardrail #3; the basin x geomorph table is internally consistent with that rule.
