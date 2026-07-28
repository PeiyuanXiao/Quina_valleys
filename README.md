<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>The Quina Valley: a regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau</i></b></h1>

<hr />

</p>

[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![R >= 4.1](https://img.shields.io/badge/R-%3E%3D4.1-blue.svg)](https://www.r-project.org/)

This repository contains the data and code for our manuscript, **in preparation**:

> **Xiao, P.-Y., Ruan, Q.-J., Jia, Z.-X., Peresani, M., Delpiano, D., & Marwick, B. (in prep.). The Quina Valley: a regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau.**

The manuscript and its supplementary material are written in Quarto and live in [`paper/`](paper); every number they report is computed by [`paper/_analysis.R`](paper/_analysis.R).

------------------------------------------------------------------------

### 👥 Authors and Affiliations

**Pei-Yuan Xiao**<sup>a,b,c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-9733-5875), **Qi-Jun Ruan**<sup>d</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-2143-5335)✉, **Zhen-Xiu Jia**<sup>a</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0002-7514-4514), **Marco Peresani**<sup>e,f</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-6562-6336), **Davide Delpiano**<sup>e</sup>✉, **Ben Marwick**<sup>c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-7879-4531)✉

-   <sup>a</sup> *Alpine Paleoecology and Human Adaptation Group (ALPHA Group), State Key Laboratory of Tibetan Plateau Earth System, Environment and Resources, Institute of Tibetan Plateau Research, Chinese Academy of Sciences, Beijing, China.*
-   <sup>b</sup> *University of Chinese Academy of Sciences, Beijing, China.*
-   <sup>c</sup> *Department of Anthropology, University of Washington, Seattle, WA, USA.*
-   <sup>d</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, China.*
-   <sup>e</sup> *Department of Human Studies, Prehistoric and Anthropological Science Unit, University of Ferrara, Ferrara, Italy.*
-   <sup>f</sup> *Consiglio Nazionale delle Ricerche–Institute of Environmental Geology and Geoengineering, Laboratory of Palynology and Palaeoecology, Research Group on Vegetation, Climate and Human Stratigraphy, Milan, Italy.*

**✉ Corresponding Authors:** Qi-Jun Ruan \* Davide Delpiano ([dlpdvd\@unife.it](mailto:dlpdvd@unife.it)) \* Ben Marwick ([bmarwick\@uw.edu](mailto:bmarwick@uw.edu))

🔧 **Maintainers:** [Pei-yuan Xiao](mailto:xiaopeiyuan@itpcas.ac.cn) & [Ben Marwick](mailto:bmarwick@uw.edu)

> **Note:** the author list, ORCIDs and affiliations above mirror the YAML of [`paper/manuscript.qmd`](paper/manuscript.qmd), which still carries a `TODO` to finalise them. Update the `.qmd`, [`paper/supplementary.qmd`](paper/supplementary.qmd) and this README together.

------------------------------------------------------------------------

### 📝 Abstract

Middle Palaeolithic assemblages vary widely in technology, and whether that variability reflects separate cultural traditions or shared responses to environmental pressures is a long-standing question in the study of human evolution. Among the most distinctive of these technologies is Quina, a specific technological behaviour built around thick, asymmetric blanks and the prolonged resharpening and recycling of the tools made on them, and long understood as an adaptation to high mobility in open, seasonally structured environments. In Europe, where it has been studied for over a century, Quina documented within a narrow glacial interval; in the eastern Old World it was, until recently, unknown. The identification of a complete Quina system at Longtan, on the southeastern margin of the Tibetan Plateau, changed this, but rested on a single case. Here we show that Longtan is not an isolated occurrence but one instance of a regional practice. Analysing Quina scrapers and resharpening flakes surface collected across two basins, and testing them against the Longtan material, we find that they share the same technological concept and the same strong preference for a single raw material, without systematic variation across the landscape; we term this regional entity the Quina Valley. Its setting, a dry-hot valley on the plateau margin, is far removed from the cold, open landscapes of the European Quina, yet poses an ecological problem of the same structure, one of patchy, seasonally organised resources reached by moving through the terrain. The significance of this study lies not only in identifying a regional Quina entity on the eastern side of the Old World, but also in providing an environmental rationale for its emergence in this region. Future excavation, by recovering absolute ages and faunal remains, will be essential to building a regional chronological framework, identifying the people behind this technology and reconstructing the subsistence strategies it served.

### 🔑 Keywords

Middle Paleolithic; Southeastern Tibetan Plateau; Quina technology; Quina valleys

------------------------------------------------------------------------

### 📁 Contents

`scripts/` and `output/` mirror the manuscript's **Results** section, so each analysis folder maps to a Results subsection and each script writes into the matching `output/<same-subfolder>/`.

- [:file_folder: data](data) — the source spreadsheets and CSVs, read by every script:

  - `Quina_scraper_surface.xlsx` — the surface-collected Quina scrapers and resharpening flakes (the core assemblage).
  - `Longtan_lithic_tools.xlsx` — the excavated Longtan retouched tools, used as the comparative reference.
  - `Raw_mat_basin.xlsx` — river-gravel clast survey: lithology, size and shape of the *available* raw material.
  - `Site_information.xlsx` — the 29 surveyed localities with coordinates, elevation, geomorphic position and distance/height above the local channel. Dropping PJDD and ZKZ gives the analysed "clean 27".
  - `Quina_sites_27_clean.csv` — that clean 27 as a flat CSV.
  - `geology_scan.jpg` — scan of the 1:200,000 regional geological sheet (Yunnan Geological Bureau, 1973), georeferenced by the geology-map scripts.
  - `cache/` — downloaded and DEM-derived spatial data (SRTM DEM, hillshade, channel network, tiles, admin boundaries). Regenerated by `maps/00_setup.R` and `maps/00b_rivers_from_dem.R`; not version-controlled.

- [:file_folder: scripts](scripts) — all analysis code (R), grouped to match the Results:

  - [`maps/`](scripts/maps) — the cartographic / DEM pipeline: cache bootstrap (`00_setup.R`), the DEM-derived stream network (`00b_rivers_from_dem.R`), the plan site map (`01_map.R`), the oblique 3-D block model (`08_terrain_3d.R`), and the geology maps (`04`–`07`).
  - [`raw_material_analysis/`](scripts/raw_material_analysis) — **Results 4.1** (landscape distribution and raw-material economy): `raw_material_electivity.R`, `raw_material_permanova.R`, `rawmat_size_compare.R`, `raw_material_distance.R`.
  - [`scraper_analysis/`](scripts/scraper_analysis) — **Results 4.2.1** (techno-typological features): `attribute_correlations.R`, `QSEA_vs_RFEPA.R`, `bordes_typology.R`, `retouch_product_attributes.R`.
  - [`technological_consistency/`](scripts/technological_consistency) — **Results 4.2.2** (technical consistency): `surface_vs_longtan.R` (surface vs Longtan: Part 1 location, Part 2 dispersion), `landscape_structure.R`.
  - [`figures/`](scripts/figures) — the manuscript figures. `statistic_figures.R` contains **no analysis**: it reads the tidy results the analysis scripts cache under `output/cache/analysis/*.rds`, so re-styling a panel never re-runs a permutation test. `fig01_compose.R` assembles Figure 1 from the plan map, the 3-D model and the traverse profile.

- [:file_folder: paper](paper) — the manuscript (`manuscript.qmd`), the supplementary material (`supplementary.qmd`), `references.bib`, and the shared analysis:

  - [`_analysis.R`](paper/_analysis.R) **computes every statistical result reported in either document.** It loads the packages, sets the seed (2226) and the permutation counts (`PERM = 9999`, `B_BOOT = 5000`, `MSLR_NR = 1e5`), defines the inline-number formatters, reads the data and leaves its results in the environment. Both `.qmd` files source it, so a number cannot differ between the paper and its supplement. Sourcing it writes nothing to disk.

- [:file_folder: templates](templates) — Quarto/Pandoc templates used when rendering: `template.docx`, the `.lua` filters and the `.csl` style. `supplement-numbering.lua` closes up the supplementary cross-reference labels ("Table S1" rather than "Table S 1").

- [:file_folder: output](output) — figures and cached results generated by the scripts (git-ignored; regenerate by re-running them). The scripts write no CSV tables; results print to the console.

- [`analysis_manuscript_crosswalk.md`](analysis_manuscript_crosswalk.md) — maps each Results paragraph to the script that produces it, and lists both analyses not yet reported and reported results not yet scripted.

> **Note:** paths are resolved with `here::here()` from the project root (the folder holding `Quina_valleys.Rproj` / `.git`), so the scripts run as-is wherever the compendium is checked out — but they must be run **from the project root**. Opening `Quina_valleys.Rproj` in RStudio guarantees this.

------------------------------------------------------------------------

### 🚀 How to reproduce

The files hosted at <https://github.com/PeiyuanXiao/Quina_valleys> are the development version.

1.  Clone the repository and open the project:
    ```sh
    git clone https://github.com/PeiyuanXiao/Quina_valleys.git
    cd Quina_valleys
    ```
    Open `Quina_valleys.Rproj` in RStudio — this sets the working directory that `here::here()` anchors to.

2.  Install the R packages listed under [Computational environment](#-computational-environment).

3.  **Build the spatial cache** (needed once, and only for the map figures). These steps download the SRTM DEM and administrative boundaries, so they need a network connection and, for `00b`, the WhiteboxTools binary (installed on first run, ~70 MB):
    ```r
    source("scripts/maps/00_setup.R")        # DEM, hillshade, admin boundaries -> data/cache/
    source("scripts/maps/00b_rivers_from_dem.R")  # hydrological channel network
    ```
    Every later script reads `data/cache/` and needs no network.

4.  **Render the documents.** This is the authoritative route: both `.qmd` files source `paper/_analysis.R`, so rendering recomputes every reported number from the raw data.
    ```sh
    quarto render paper/manuscript.qmd
    quarto render paper/supplementary.qmd
    ```

5.  **Or run the scripts individually.** Each analysis script is self-contained and reads only from `data/`, so the three analysis folders and the scripts within them can be run in any order. They mirror `_analysis.R` — same numbers, plus their diagnostic figures:
    ```r
    source("scripts/raw_material_analysis/raw_material_permanova.R")
    source("scripts/technological_consistency/surface_vs_longtan.R")
    # ... etc.
    ```
    The figure scripts come last: `scripts/figures/statistic_figures.R` reads the `.rds` files the analysis scripts cache, and `scripts/figures/fig01_compose.R` needs `output/maps/terrain_3d.png` from `scripts/maps/08_terrain_3d.R`.

> **Not yet in place:** this compendium has no `renv.lock`, `DESCRIPTION` or `Dockerfile`, so the package library is not pinned and the environment is not containerised. Adding them is the next step towards full computational reproducibility.

------------------------------------------------------------------------

### 📤 Outputs

-   **Console:** the statistical tests and descriptive tables, printed as each script runs.
-   **`paper/manuscript.docx`** and **`paper/supplementary.docx`:** the rendered documents, with every figure and table.
-   **`output/figures/`:** the manuscript figures — `fig01_composite`, `fig_traverse_profile`, `fig_terrace_position`, `fig_liandong_section`, `fig_raw_material_composition`, `fig_edge_angle_reduction`, `fig_technological_consistency` (PNG at 600 dpi, several also PDF).
-   **`output/maps/`:** the cartographic products — `map_quina_sites`, `terrain_3d`, the geology maps and the Illustrator tracing layers.
-   **`output/cache/analysis/*.rds`:** tidy results cached by the analysis scripts so that the figure scripts can re-plot without re-running a permutation test.
-   **`output/<analysis folder>/`:** the diagnostic figures of each analysis script.

The whole of `output/` is git-ignored, as is `asset/` (specimen photographs and Illustrator sources — large, binary, and not needed to reproduce any result).

------------------------------------------------------------------------

### 💻 Computational environment

-   **R:** developed under **R 4.5.2**; the code requires **R ≥ 4.1** (it uses the native pipe `|>`).
-   **Quarto** is needed to render the manuscript and supplement.
-   **R packages:**

    | Package | Role |
    |---|---|
    | `tidyverse` | data wrangling and `ggplot2` graphics |
    | `here` | project-root-relative paths |
    | `readxl` | reading the `.xlsx` raw-data files |
    | `vegan` | PERMANOVA, PERMDISP and the ordinations |
    | `rstatix` | tidy wrappers for the univariate tests and effect sizes |
    | `cvequality` | Krishnamoorthy–Lee MSLRT test of CV equality |
    | `ggpubr`, `patchwork`, `ggrepel`, `ggnewscale`, `grid` | figure composition, labelling and multiple fill scales |
    | `ragg`, `magick` | high-resolution PNG devices and the Figure 1 raster panel |
    | `sf`, `terra`, `tidyterra`, `ggspatial` | vector/raster spatial data and map furniture |
    | `elevatr`, `rnaturalearth`, `osmdata`, `maptiles` | one-time downloads of the DEM, boundaries, waterways and tiles |
    | `whitebox` | hydrological extraction of the channel network from the DEM |
    | `rayshader` | the path-traced 3-D block model |

    The spatial packages in the last four rows are needed only for `scripts/maps/`; the analysis scripts and the two `.qmd` documents run without them.

To capture your own session for the record, run `sessionInfo()` after sourcing the scripts.

------------------------------------------------------------------------

### 📄 License

Code and data in this repository are intended for release under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license, matching the companion [Longtan compendium](https://github.com/PeiyuanXiao/Longtan_raw_data). You are free to share and adapt the material for any purpose, provided you give appropriate credit by citing the paper above.

> **Note:** a `LICENSE` file has not yet been added to the repository. Add one before the compendium is made public.
