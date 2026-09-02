<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>The Quina Landscape: A regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau</i></b></h1>

<hr />

</p>

[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![R 4.6.1](https://img.shields.io/badge/R-4.6.1-blue.svg)](https://www.r-project.org/) [![pipeline](https://github.com/PeiyuanXiao/Quina_valleys/actions/workflows/pipeline.yaml/badge.svg)](https://github.com/PeiyuanXiao/Quina_valleys/actions/workflows/pipeline.yaml) [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/PeiyuanXiao/Quina_valleys/main?urlpath=rstudio)

This repository contains the data and code for our manuscript, **in preparation**:

> **Xiao, P., Ruan, Q., Delpiano, D., Peresani, M., Jia, Z., Yang, L., Marwick, B., & Li, H. (in prep.). The Quina Landscape: A regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau.**

The manuscript and its supplementary material are written in Quarto and can be found in [`paper/`](paper). Every number they report is computed by [`paper/_analysis.R`](paper/_analysis.R), except the Bayesian landscape analysis, which has a pipeline of its own in [`paper/barg/`](paper/barg).

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

- [:file_folder: data](data) — the source spreadsheets and CSVs, read by every script:

  - `Quina_scraper_surface.xlsx` — the surface-collected Quina scrapers and resharpening flakes (the core assemblage).
  - `Longtan_lithic_tools.xlsx` — the excavated Longtan retouched tools, used as the comparative reference.
  - `Raw_mat_basin.xlsx` — river-gravel clast survey: lithology, size and shape of the *available* raw material.
  - `Site_information.xlsx` — the data for the localities: the 27 Quina localities as re-verified in the field record, with coordinates, elevation, geomorphic position and distance/height above the local channel. 

- [:file_folder: figures](figures) —  the maps and the specimen and field photographs.  

- [:file_folder: paper](paper) — the manuscript (`manuscript.qmd`), the supplementary material (`supplementary.qmd`), the bibliographies (`references.bib`, `packages.bib`), and the analyses:

  - [`_analysis.R`](paper/_analysis.R) computes every statistical result reported in either document. It loads the packages, sets the seed (2226) and the permutation counts (`PERM = 9999`, `B_BOOT = 5000`, `MSLR_NR = 1e5`), defines the inline-number formatters, reads the data and leaves its results in the environment. The pipeline runs it as a single target and both `.qmd` files load that one environment, so a number cannot differ between the paper and its supplement. Running it writes nothing to disk.

  - [`barg/`](paper/barg) — the Bayesian side of the landscape analysis, the one part of the paper `_analysis.R` does not compute. `barg_data.R` builds the specimen-level frame (165 Quina scrapers in 26 localities) and defines the link-scale SD that sets the ROPE; `barg_priors.R` holds all seven prior specifications; `barg_models.R` defines the eleven model specifications, the sampler call and the per-fit diagnostics; `barg_context.R` assembles the environments the rest runs in; `barg_quantities.R` derives every reported quantity; `barg_main_figure.R` defines the two-panel display the manuscript carries as Figure 9; `barg_figures.R` draws every figure into `paper/barg/figures/`, that one included; `barg_report.qmd` renders `barg_report.html`, the Supplementary Bayesian Report, written under the Bayesian Analysis Reporting Guidelines (Kruschke 2021). Nothing here is run by hand: [`_targets.R`](_targets.R) fits the models, and `manuscript.qmd` and `supplementary.qmd` read the same fits from the same store — the manuscript for Figure 9, the supplementary for Tables S17 and S18 — so the figure, the tables and the report cannot drift apart, and the percentages quoted beside Figure 9 in the Results are read from the report.

  - [`map/`](paper/map) — the Figure 1 map pipeline. `setup.R` builds the spatial cache in `data/cache/` (DEM-derived rasters and vector layers; needs a network connection and WhiteboxTools on the first run); `terra_map_2D.R`, `terra_map_2D_regional.R` and `terra_map_3D_hyps.R` render the plan, regional and three-dimensional terrain maps; `traverse_profile_figure.R` draws the topographic profile; `locator_globe_figure.R` draws the global locator inset; and `fig01_export_panels.R` re-sources the panel scripts and assembles the Figure 1 panels, which are then composed by hand into `figures/study_area.png`. These scripts write their outputs under `output/` and read the spatial cache from `data/cache/`, both not included here due to large file sizes, but can be rebuilt on demand.

- [:file_folder: templates](templates) — Quarto/Pandoc templates used when rendering: `template.docx`, the `.lua` filters and the `.csl` style. `supplement-numbering.lua` closes up the supplementary cross-reference labels ("Table S1" rather than "Table S 1").

- The reproducibility machinery, at the project root:

  - [`_targets.R`](_targets.R) and [`_targets.yaml`](_targets.yaml) — the pipeline: what depends on what, and what is rebuilt when. The `quick` project runs the same models at reduced iterations into a separate store.
  - [`renv.lock`](renv.lock) — every R package version the analysis was run under. `renv::restore()` reproduces the library.
  - [`Dockerfile`](Dockerfile) — the full environment including **CmdStan 2.39.0**, which `renv.lock` cannot pin because it is a C++ toolchain rather than an R package. Published to `ghcr.io/peiyuanxiao/quina_valleys`.
  - [`.binder/Dockerfile`](.binder/Dockerfile) — the browser environment behind the Binder badge.
  - [`.github/workflows/`](.github/workflows) — the quick pipeline on every push, the full pipeline on demand, and the image build.

> **Note:** paths are resolved with `here::here()` from the project root (the folder holding `Quina_valleys.Rproj` / `.git`), so the scripts run as-is wherever the compendium is checked out — but they must be run from the project root. Opening `Quina_valleys.Rproj` in RStudio guarantees this.

------------------------------------------------------------------------


### 🚀 How to reproduce

The whole compendium is one [targets](https://books.ropensci.org/targets/) pipeline: `tar_make()` fits the eleven Bayesian models, derives every quantity, draws every figure and renders the three documents, in dependency order. It rebuilds only what is out of date, so a second run after a text edit refits nothing, and a change to a prior or to the data refits exactly what that change touched. There is no cache to manage by hand.

Three ways to get the environment it needs. They differ only in how the software is installed; the command at the end is the same.

**1. Pull the container (nothing to install but Docker)**

``` sh
docker pull ghcr.io/peiyuanxiao/quina_valleys:latest
docker run --rm -it -e PASSWORD=quina -p 8787:8787 ghcr.io/peiyuanxiao/quina_valleys:latest
```

Then open <http://localhost:8787> (user `rstudio`, password `quina`) and run `targets::tar_make()` in the console. The image pins R 4.6.1, the package versions in `renv.lock`, and **CmdStan 2.39.0** — which is the reason it exists, since the CmdStan version is part of the model specification and is outside what `renv.lock` can pin.

**2. Run it in your browser (nothing to install at all)**

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/PeiyuanXiao/Quina_valleys/main?urlpath=rstudio)

Binder opens RStudio with the packages already installed. Its limits are real and worth stating: about 2 GB of memory and no CmdStan, so it will run `paper/_analysis.R` — every frequentist result in the paper — and let you read every file, but it cannot fit the Bayesian models. Use option 1 or 3 for those.

**3. Install locally**

``` sh
git clone https://github.com/PeiyuanXiao/Quina_valleys.git
cd Quina_valleys
```

Open `Quina_valleys.Rproj` in RStudio, then:

``` r
renv::restore()                      # the package versions in renv.lock
cmdstanr::install_cmdstan(version = "2.39.0")
targets::tar_make()
```

You will also need [Quarto](https://quarto.org/) 1.4 or later on your PATH.

------------------------------------------------------------------------

### 🔍 Working with the pipeline

``` r
targets::tar_make()                       # build whatever is out of date
targets::tar_visnetwork()                 # the graph, and what in it is stale
targets::tar_make(names = "fit_ref")      # one target and its dependencies
targets::tar_read(barg_diag_tbl)          # per-fit Rhat, ESS, divergences
```

A cold `tar_make()` is 30–60 minutes, almost all of it MCMC sampling; everything after that is minutes. For a fast check that the plumbing works, the `quick` project of [`_targets.yaml`](_targets.yaml) runs the same eleven models at reduced iterations into a separate store, and stops short of rendering so it cannot overwrite the real documents:

``` sh
TAR_PROJECT=quick Rscript -e "targets::tar_make()"
```

The store `_targets/` is git-ignored and holds about 300 MB, most of it the eleven fitted models. It is the object to archive alongside the paper, and what a reader restores to re-render the documents without refitting.

**The Figure 1 maps are not in the pipeline.** They need a network connection, WhiteboxTools and an OpenGL stack, and their output is committed as `figures/study_area.png`, so they are run on their own; see [`paper/map/README.md`](paper/map/README.md).

------------------------------------------------------------------------

### 📄 License

Code and data in this repository are intended for release under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.
