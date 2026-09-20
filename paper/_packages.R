# The packages the analysis and the two documents need attached, in one place
# so they cannot drift apart.  _analysis.R sources this, and so do
# manuscript.qmd and supplementary.qmd: the pipeline runs _analysis.R in a
# separate process and the documents receive only its objects, so they need
# the packages in their own right.
#
# WdStar is not on CRAN: remotes::install_github("alekseyenko/WdStar").
# cmdstanr is on the Stan r-universe.  Exact versions are in renv.lock.
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

# Deliberately NOT attached: posterior.  It would mask sd(), var(), mad(),
# match() and %in% for the whole render session, and _analysis.R uses sd()
# seven times.  Calls inside paper/barg/ are namespace-qualified instead.
