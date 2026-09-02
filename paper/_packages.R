# ==========================================================================
# _packages.R -- the packages the analysis and the two documents need
# attached, in one place so they cannot drift apart.
#
# _analysis.R sources this, and so do manuscript.qmd and supplementary.qmd.
# The documents need it in their own right: they call ggplot(), mutate() and
# group_by() directly in their chunks, and since the pipeline runs _analysis.R
# in a separate process and the documents receive only its objects, attaching
# the packages is no longer something _analysis.R does for them as a side
# effect of being sourced into the render session.
#
# WdStar is not on CRAN: remotes::install_github("alekseyenko/WdStar").
# cmdstanr is on the Stan r-universe, not CRAN. Both, and the exact versions
# of everything else, are pinned in renv.lock.
# ==========================================================================
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
library(WdStar)
library(cmdstanr)
library(rnaturalearthhires)
