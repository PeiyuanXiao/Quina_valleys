<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>The Quina Valley: a regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau</i></b></h1>

<hr />

</p>

[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![R \>= 4.1](https://img.shields.io/badge/R-%3E%3D4.1-blue.svg)](https://www.r-project.org/)

This repository contains the data and code for our manuscript, **in preparation**:

> **Xiao, P., Ruan, Q., Delpiano, D., Peresani, M., Jia, Z., Yang, L., Marwick, B., & Li, H. (in prep.). The Quina Valley: a regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau.**

The manuscript and its supplementary material are written in Quarto and live in [`paper/`](paper); every number they report is computed by [`paper/_analysis.R`](paper/_analysis.R), except the Bayesian landscape analysis, which has a pipeline of its own in [`paper/barg/`](paper/barg).

------------------------------------------------------------------------

### 👥 Authors and Affiliations

**Peiyuan Xiao**<sup>a,b,c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-9733-5875), **Qijun Ruan**<sup>d</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-2143-5335)✉, **Davide Delpiano**<sup>e</sup>, **Marco Peresani**<sup>e,f</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-6562-6336), **Zhenxiu Jia**<sup>a</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0002-7514-4514), **Lijing Yang**<sup>d</sup>, **Ben Marwick**<sup>c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-7879-4531)✉, **Hao Li**<sup>a</sup>✉

- <sup>a</sup> *Alpine Paleoecology and Human Adaptation Group (ALPHA Group), State Key Laboratory of Tibetan Plateau Earth System, Environment and Resources, Institute of Tibetan Plateau Research, Chinese Academy of Sciences, Beijing, China.*
- <sup>b</sup> *University of Chinese Academy of Sciences, Beijing, China.*
- <sup>c</sup> *Department of Anthropology, University of Washington, Seattle, WA, USA.*
- <sup>d</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, China.*
- <sup>e</sup> *Department of Human Studies, Prehistoric and Anthropological Science Unit, University of Ferrara, Ferrara, Italy.*
- <sup>f</sup> *Consiglio Nazionale delle Ricerche–Institute of Environmental Geology and Geoengineering, Laboratory of Palynology and Palaeoecology, Research Group on Vegetation, Climate and Human Stratigraphy, Milan, Italy.*

**✉ Corresponding Authors:** Qijun Ruan \* Ben Marwick ([bmarwick\@uw.edu](mailto:bmarwick@uw.edu)) \* Hao Li ([lihao\@itpcas.ac.cn](mailto:lihao@itpcas.ac.cn))

🔧 **Maintainers:** [Peiyuan Xiao](mailto:xiaopeiyuan@itpcas.ac.cn) & [Ben Marwick](mailto:bmarwick@uw.edu)

------------------------------------------------------------------------

### 📁 Contents

`paper/_analysis.R` is the single source of every number, table and statistical figure in the two documents bar the Bayesian landscape analysis, which `paper/barg/` carries end to end; `scripts/` now holds only the cartographic pipeline behind Figure 1. That map is the one figure no document draws for itself: every statistical figure in either document, Figure 9 included, is drawn by the `.qmd` that carries it at render time, from the objects `_analysis.R` and (for Figure 9) `paper/barg/` leave in the environment. What is left in `figures/` is therefore only the map and the specimen photographs.

- [:file_folder: data](data) — the source spreadsheets and CSVs, read by every script:

  - `Quina_scraper_surface.xlsx` — the surface-collected Quina scrapers and resharpening flakes (the core assemblage).
  - `Longtan_lithic_tools.xlsx` — the excavated Longtan retouched tools, used as the comparative reference.
  - `Raw_mat_basin.xlsx` — river-gravel clast survey: lithology, size and shape of the *available* raw material.
  - `Site_information.xlsx` — the single source of truth for the localities: the 27 Quina-bearing localities as re-verified in the field record, with coordinates, elevation, geomorphic position and distance/height above the local channel. An earlier flat CSV of an older, larger site list (`Quina_sites_27_clean.csv`) was deleted on 2026-07-30; it named a different 27 and is superseded by this file. Nothing reads it and it remains in the git history.
  - `geology_scan.jpg` — scan of the 1:200,000 regional geological sheet (Yunnan Geological Bureau, 1973). The geology map is being drawn by hand from this sheet, so no script renders it; the scripts that once did (schematic, scan-based and classified variants) were removed on 2026-07-29 and remain in the git history.
  - `cache/` — downloaded and DEM-derived spatial data (SRTM DEM, hillshade, channel network, admin boundaries). **Untracked since 2026-08-19** and no longer committed: it is \~51 MB of rebuildable binaries that only the Figure 1 pipeline reads, and nothing in the manuscript or the supplementary material touches it. `scripts/map/setup.R` regenerates it from scratch, which now means a clone needs a network connection and WhiteboxTools before Figure 1 can be redrawn. The wider `*_regional.*` cache is **not** committed — `scripts/map/terra_map_2D_regional.R` builds it, and `terra_map_2D.R` needs it whenever `MAP_EXT` widens the frame past the site extent, which is how the published panel A reaches the Jinsha. A third, `*_setibet.*` cache for an abandoned SE-Tibet overview was deleted on 2026-08-18 along with the scripts that read it.

- [:file_folder: figures](figures) — the images the two documents include as finished files, and nothing else: `study_area.png` and `geology_map.png`, the two maps, and the specimen and field photographs (`quina_scraper_1.png`, `quina_scraper_2.png`, `resharpening_flake.png`, `sample_point_photo.png`, `thin_section.png`). Every statistical figure is drawn by the `.qmd` that carries it and none is stored here; the ones that were, the jackknife, the equivalence *p*-value curves and the electivity sensitivity, moved into `supplementary.qmd` on 2026-08-26.

- [:file_folder: scripts](scripts) — all analysis code (R), grouped to match the Results:

  - [`map/`](scripts/map) — the whole cartographic / DEM pipeline behind Figure 1, in run order. `setup.R` prepares everything the panels read (the site table and the cached DEM, hillshade, DEM-derived channel network and administrative boundaries) and `terra_map_2D_regional.R` extends that cache north to the Jinsha; `terra_map_2D.R` draws the plan site map, `terra_map_3D_hyps.R` the path-traced oblique block model, `traverse_profile_figure.R` the traverse long profile and `locator_globe_figure.R` the orthographic locator inset; `fig01_export_panels.R` then re-renders the map and the profile at their final placed sizes into `output/figures/fig01_panels/`. `figures/study_area.png` is assembled by hand in Illustrator from four files: `panel_A_map_with_route_landscape_north_bare.png` and `panel_C_profile_landscape.png` from that folder, `locator_globe.png` beside them, and `output/maps/terrain_3d_hyps.png`. Earlier variants — a tiled basemap, a latitude-ordered elevation profile, three geology maps, an automatic patchwork composite — were deleted once superseded; a further round on 2026-08-18 removed the SE-Tibet overview maps, the regional locator and its composite, the pre-hypsometric 3-D block (`terra_map_3D.R`, still the baseline the comments in `terra_map_3D_hyps.R` refer to) and its v2 successor. The git history has them all.
  - `raw_material_analysis/`, `scraper_analysis/`, `technological_consistency/` — **removed on 2026-08-19.** These eleven scripts were a second, independently written implementation of analyses that `_analysis.R` already carries in full, and which the documents source directly; nothing outside them read their output. Keeping two implementations of a published statistic invites the two to drift apart, so the duplicate was retired. The git history has them. Note what went with them, since none of it is in `_analysis.R`: `landscape_structure.R` held the robustness work behind the "suggestive rather than conclusive" reading of the distance trend — the scraper-size correlation reweighted by specimens per locality (which reverses sign), its leave-one-locality-out range, an artefact-level test with localities permuted whole, PERMDISP across height bands, assemblage-size binning and a rank-transformed refit — together with the `_GUARDRAILS.txt` notes each analysis wrote beside its output.
  - `figures/` — **removed on 2026-08-18.** Every statistical figure is now drawn inside the `.qmd` that carries it, from the objects `_analysis.R` leaves in the environment, so a figure can no longer drift from the numbers beside it. The folder had held five scripts that wrote PNGs into `output/figures/`: `statistic_figures.R`, `raw_material_composition_figure.R` and `edge_angle_reduction_figure.R`, whose figures the documents now draw inline (`fig-technological-consistency`, `fig-raw-material-composition`, `fig-edge-angle-reduction`), and `terrace_position_figure.R` and `liandong_section_figure.R`, two superseded Figure 1 panel candidates that nothing sourced. The git history has them, and with them the methodological notes their headers carried — that the edge-angle correlations are quoted unadjusted with the Bonferroni values sent to the console, and that `d_river_m` and `h_river_m` disagree with the DEM.

- [:file_folder: paper](paper) — the manuscript (`manuscript.qmd`), the supplementary material (`supplementary.qmd`), `references.bib`, and the shared analysis:

  - [`_analysis.R`](paper/_analysis.R) **computes every statistical result reported in either document.** It loads the packages, sets the seed (2226) and the permutation counts (`PERM = 9999`, `B_BOOT = 5000`, `MSLR_NR = 1e5`), defines the inline-number formatters, reads the data and leaves its results in the environment. Both `.qmd` files source it, so a number cannot differ between the paper and its supplement. Sourcing it writes nothing to disk.

  - [`barg/`](paper/barg) — the Bayesian side of the landscape analysis, self-contained and run separately from `_analysis.R`. `barg_data.R` builds the specimen-level frame (165 Quina scrapers in 26 localities) and defines the link-scale SD that sets the ROPE; `barg_priors.R` holds all six prior specifications; `barg_fits.R` runs and caches the nine MCMC fits into `paper/barg/fits/` (git-ignored, ~20 MB each; `BARG_QUICK=1` diverts a reduced run to `fits_quick/`); `barg_quantities.R` derives every reported quantity; `barg_main_figure.R` defines the two-panel display the manuscript carries as Figure 9; `barg_figures.R` draws every figure into `paper/barg/figures/`, that one included; `barg_report.qmd` renders `barg_report.html`, the Supplementary Bayesian Report, written under the Bayesian Analysis Reporting Guidelines (Kruschke 2021). `barg_dummy_data.R` writes a synthetic stand-in dataset for the case where the specimen data cannot be released. This is the one part of the paper `_analysis.R` does not compute: both `manuscript.qmd` and `supplementary.qmd` source these files and read the cached fits themselves — the manuscript for Figure 9, the supplementary for Tables S17 and S18 — so the figure, the tables and the report are one fit and cannot drift, and the percentages quoted beside Figure 9 in the Results are read from the report.

- [:file_folder: templates](templates) — Quarto/Pandoc templates used when rendering: `template.docx`, the `.lua` filters and the `.csl` style. `supplement-numbering.lua` closes up the supplementary cross-reference labels ("Table S1" rather than "Table S 1").

- [:file_folder: output](output) — figures and cached results generated by the scripts (git-ignored; regenerate by re-running them). The scripts write no CSV tables; results print to the console.

> **Note:** paths are resolved with `here::here()` from the project root (the folder holding `Quina_valleys.Rproj` / `.git`), so the scripts run as-is wherever the compendium is checked out — but they must be run **from the project root**. Opening `Quina_valleys.Rproj` in RStudio guarantees this.

------------------------------------------------------------------------

### 🚀 How to reproduce

The files hosted at <https://github.com/PeiyuanXiao/Quina_valleys> are the development version.

1.  Clone the repository and open the project:

    ``` sh
    git clone https://github.com/PeiyuanXiao/Quina_valleys.git
    cd Quina_valleys
    ```

    Open `Quina_valleys.Rproj` in RStudio — this sets the working directory that `here::here()` anchors to.

2.  Install the R packages the two documents load. They are listed with the versions actually used in the supplementary material (`tbl-software`), and all of them are on CRAN except one: `WdStar`, the Welch-type distance-based MANOVA that `_analysis.R` sets beside the pairwise PERMANOVA and that Part 3 of the supplement extends to every other one, is installed from its authors' repository.

    ``` r
    remotes::install_github("alekseyenko/WdStar")
    ```

3.  **Build the spatial cache** (needed once, and only for Figure 1). This downloads the SRTM DEM and the administrative boundaries and extracts the channel network from the DEM, so it needs a network connection and the WhiteboxTools binary (installed on first run, \~70 MB):

    ``` r
    source("scripts/map/setup.R")
    ```

    Everything it builds lands in `data/cache/` and is skipped on a re-run if already present; every later script reads that cache and needs no network.

4.  **Render the documents.** This is the authoritative route: both `.qmd` files source `paper/_analysis.R`, so rendering recomputes every reported number from the raw data.

    ``` sh
    quarto render paper/manuscript.qmd
    quarto render paper/supplementary.qmd
    ```

    Both documents read the MCMC fits cached in `paper/barg/fits/`, which are git-ignored and so absent from a fresh clone; run the first command of Step 6 once before this step. The manuscript draws Figure 9 from that cache and the supplementary tabulates the same fit in Tables S17 and S18. Both only read it and never sample, so a render cannot silently refit.

5.  **Rebuild the Figure 1 panels** (optional; only if the map itself changes). `figures/study_area.png` is assembled by hand from these, so the script run is one step of a two-step process — the run order is given under [`map/`](scripts/map) above, since `fig01_export_panels.R` re-sources the two panel scripts:

    ``` r
    source("scripts/map/fig01_export_panels.R")
    ```

    Step 4 does not depend on this: the documents read the map and the photographs from `figures/` as finished images. Apart from the Bayesian pipeline in Step 6, no other script is needed to reproduce anything in the manuscript or the supplementary material — every other reported number, table and statistical figure is computed by `_analysis.R` and drawn inside the `.qmd` that reports it.

6.  **Rebuild the Bayesian landscape analysis** (optional; only if the model, the data behind it or its figures change). This is the one analysis `_analysis.R` does not carry: it fits the model behind Figure 9 of the manuscript and writes the Supplementary Bayesian Report.

    ``` sh
    Rscript paper/barg/barg_fits.R        # nine MCMC runs; caches to paper/barg/fits/
    Rscript paper/barg/barg_figures.R     # reads the cache, writes paper/barg/figures/
    quarto render paper/barg/barg_report.qmd
    ```

    `barg_fits.R` samples only for a fit it cannot find in the cache, so a re-run after the first is cheap; delete a `.rds` to force that one fit again, and set `BARG_QUICK=1` to run the whole chain at reduced iterations into `paper/barg/fits_quick/` for a pipeline smoke test. Only the fits matter to the manuscript: it draws Figure 9 from them itself through `barg_main_figure.R`, so a refit reaches the paper on the next render of Step 4 whether or not `barg_figures.R` is run.

------------------------------------------------------------------------

### 📄 License

Code and data in this repository are intended for release under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.
