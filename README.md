<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>The Quina Landscape: A regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau</i></b></h1>

<hr />

[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![R 4.6.1](https://img.shields.io/badge/R-4.6.1-blue.svg)](https://www.r-project.org/) [![pipeline](https://github.com/PeiyuanXiao/Quina_valleys/actions/workflows/pipeline.yaml/badge.svg)](https://github.com/PeiyuanXiao/Quina_valleys/actions/workflows/pipeline.yaml) [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/PeiyuanXiao/Quina_valleys/main?urlpath=rstudio)

This repository contains the data and code for our manuscript, **in preparation**:

> **Xiao, PY., Ruan, QJ., Delpiano, D., Peresani, M., Jia, ZX., Yang, LJ., Marwick, B., & Li, H. (in prep.). The Quina Landscape: A regional Middle Palaeolithic technological system on the southeastern margin of the Tibetan Plateau.**

The manuscript and its supplementary material are written in Quarto and are in [`paper/`](paper). [`paper/_analysis.R`](paper/_analysis.R) computes every number they report, except for the Bayesian landscape analysis, which has a pipeline of its own in [`paper/barg/`](paper/barg).

------------------------------------------------------------------------

### 👥 Authors and Affiliations

**Pei-Yuan Xiao**<sup>a,b,c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-9733-5875), **Qi-Jun Ruan**<sup>d</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-2143-5335)✉, **Davide Delpiano**<sup>e</sup>, **Marco Peresani**<sup>e,f</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-6562-6336), **Zhen-Xiu Jia**<sup>a</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0002-7514-4514), **Li-Jing Yang**<sup>d</sup>, **Ben Marwick**<sup>c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0000-0001-7879-4531)✉, **Hao Li**<sup>a</sup>✉

- <sup>a</sup> *Alpine Paleoecology and Human Adaptation Group (ALPHA Group), State Key Laboratory of Tibetan Plateau Earth System, Environment and Resources, Institute of Tibetan Plateau Research, Chinese Academy of Sciences, Beijing, China.*
- <sup>b</sup> *University of Chinese Academy of Sciences, Beijing, China.*
- <sup>c</sup> *Department of Anthropology, University of Washington, Seattle, WA, USA.*
- <sup>d</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, China.*
- <sup>e</sup> *Department of Human Studies, Prehistoric and Anthropological Science Unit, University of Ferrara, Ferrara, Italy.*
- <sup>f</sup> *Consiglio Nazionale delle Ricerche–Institute of Environmental Geology and Geoengineering, Laboratory of Palynology and Palaeoecology, Research Group on Vegetation, Climate and Human Stratigraphy, Milan, Italy.*

**✉ Corresponding Authors:** Qi-Jun Ruan \* Ben Marwick ([bmarwick\@uw.edu](mailto:bmarwick@uw.edu)) \* Hao Li ([lihao\@itpcas.ac.cn](mailto:lihao@itpcas.ac.cn))

🔧 **Maintainers:** [Pei-Yuan Xiao](mailto:xiaopeiyuan@itpcas.ac.cn) & [Ben Marwick](mailto:bmarwick@uw.edu)

------------------------------------------------------------------------

### 📁 Contents

- [:file_folder: data](data): the source spreadsheets and CSVs, read by every script.

  - `Quina_scraper_surface.xlsx`: the surface-collected Quina scrapers and resharpening flakes, the core assemblage.
  - `Longtan_lithic_tools.xlsx`: the excavated Longtan retouched tools, used as the comparative reference.
  - `Raw_mat_basin.xlsx`: the river-gravel clast survey, giving the lithology, size and shape of the *available* raw material.
  - `Site_information.xlsx`: the 27 Quina localities as re-verified in the field record, with coordinates, elevation, geomorphic position, and distance and height above the local channel.

- [:file_folder: figures](figures): the maps and the specimen and field photographs.

- [:file_folder: paper](paper): the manuscript (`manuscript.qmd`), the supplementary material (`supplementary.qmd`), the bibliographies (`references.bib`, `packages.bib`), and the analyses.

  - [`_analysis.R`](paper/_analysis.R) computes every statistical result reported in either document. It sets the seed (2226) and the permutation counts (`PERM = 9999`, `B_BOOT = 5000`, `MSLR_NR = 1e5`), reads the data, and leaves its results in the environment without writing anything to disk. The pipeline runs it as a single target and both `.qmd` files load that one environment, so a number cannot differ between the paper and its supplement.

  - [`barg/`](paper/barg) is the Bayesian side of the landscape analysis, the one part of the paper that `_analysis.R` does not compute. `barg_data.R` builds the specimen-level frame (165 Quina scrapers in 26 localities), `barg_priors.R` and `barg_models.R` give the seven prior and eleven model specifications, `barg_quantities.R` derives every reported quantity, `barg_sbc.R` runs the posterior simulation-based calibration check on the reference fit, and `barg_figures.R` and `barg_main_figure.R` draw the figures, Figure 9 included. `barg_report.qmd` renders the Supplementary Bayesian Report, written under the Bayesian Analysis Reporting Guidelines (Kruschke 2021). Nothing here is run by hand: [`_targets.R`](_targets.R) fits the models, and both documents read those same fits, for Figure 9 and for Tables S17 and S18, so the figure, the tables and the report cannot drift apart.

  - [`map/`](paper/map) is the Figure 1 map pipeline. `setup.R` builds the spatial cache in `data/cache/`, five scripts render the plan, regional and three-dimensional terrain maps, the topographic profile and the global locator inset, and `fig01_export_panels.R` assembles the panels, which are then composed by hand into `figures/study_area.png`. It runs on its own rather than under `targets`, because it needs a network connection, WhiteboxTools and an OpenGL stack. Its outputs and cache are too large to include here and are rebuilt on demand. See [`paper/map/README.md`](paper/map/README.md).

- [:file_folder: templates](templates): the Quarto and Pandoc templates used when rendering, namely `template.docx`, the `.lua` filters and the `.csl` style. `supplement-numbering.lua` closes up the supplementary cross-reference labels, giving "Table S1" rather than "Table S 1".

- The reproducibility machinery, at the project root:

  - [`_targets.R`](_targets.R): the pipeline, defining what depends on what and what is rebuilt when. One pipeline, no reduced mode.
  - [`renv.lock`](renv.lock): every R package version the analysis was run under. `renv::restore()` reproduces the library.
  - [`Dockerfile`](Dockerfile): the full environment including **CmdStan 2.39.0**, which `renv.lock` cannot pin because it is a C++ toolchain rather than an R package. Published to `ghcr.io/peiyuanxiao/quina_valleys`.
  - [`.binder/Dockerfile`](.binder/Dockerfile): the browser environment behind the Binder badge.
  - [`.github/workflows/`](.github/workflows): the full pipeline on every push and pull request, and the image build whenever the `Dockerfile` or `renv.lock` changes.

> **Note:** paths are resolved with `here::here()` from the project root, the folder containing `Quina_valleys.Rproj` and `.git`, so the scripts run as-is wherever the compendium is checked out. They must be run from that root; opening `Quina_valleys.Rproj` in RStudio guarantees it.

------------------------------------------------------------------------

### 🚀 How to Reproduce

The whole compendium is one [targets](https://books.ropensci.org/targets/) pipeline. `tar_make()` fits the eleven Bayesian models, derives every quantity, draws every figure and renders the three documents, in dependency order, rebuilding only what is out of date. A second run after a text edit refits nothing; a change to a prior or to the data refits exactly what that change touched.

Three ways to get the environment it needs. They differ only in how the software is installed; the command at the end is the same.

**1. Pull the Container**

``` sh
docker pull ghcr.io/peiyuanxiao/quina_valleys:latest
docker run --rm -it -e PASSWORD=quina -p 8787:8787 ghcr.io/peiyuanxiao/quina_valleys:latest
```

Docker is the only thing to install. Open <http://localhost:8787> (user `rstudio`, password `quina`) and run `targets::tar_make()` in the console. The image pins R 4.6.1, the package versions in `renv.lock`, and **CmdStan 2.39.0**, which is why it exists: the CmdStan version is part of the model specification and outside what `renv.lock` can pin.

**2. Run It in Your Browser**

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/PeiyuanXiao/Quina_valleys/main?urlpath=rstudio)

Binder opens RStudio with the packages already installed and nothing to install locally. Its limits are worth stating: about 2 GB of memory and no CmdStan. It will run `paper/_analysis.R`, every frequentist result in the paper, but it cannot fit the Bayesian models. Use option 1 or 3 for those.

**3. Install Locally**

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

### 🔍 Working with the Pipeline

``` r
targets::tar_make()                       # build whatever is out of date
targets::tar_visnetwork()                 # the graph, and what in it is stale
targets::tar_make(names = "fit_ref")      # one target and its dependencies
targets::tar_read(barg_diag_tbl)          # per-fit Rhat, ESS, divergences
targets::tar_read(sbc_unif)               # the calibration check, per quantity
```

A cold `tar_make()` takes about four and a half hours, of which about three and a half are the 100 calibration refits of the posterior SBC check; the eleven reported fits are 40 minutes of that, and the figures and the three renders the rest. Everything after that takes minutes. Edit a prior and the eleven fits are refitted, and so are the calibration refits that check them; edit a figure and neither is touched; edit prose and only that document is re-rendered. There is deliberately no reduced or "quick" mode, because shortened chains would produce numbers the documents must not report.

The store `_targets/` is git-ignored and comes to about 460 MB, most of it the eleven fitted models. It is the object to archive alongside the paper, and what a reader restores to re-render the documents without refitting.

**The Figure 1 maps are outside the pipeline**, and their output is committed as `figures/study_area.png`. See [`paper/map/README.md`](paper/map/README.md).

------------------------------------------------------------------------

### 📄 License

Code and data in this repository are intended for release under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.
