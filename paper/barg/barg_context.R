# ==========================================================================
# barg_context.R -- builds the shared environments the pipeline runs in.
#
# barg_data.R, barg_priors.R, barg_quantities.R, barg_theme.R and
# barg_main_figure.R are scripts, not packages: they define constants
# (resps, link_sd, ROPE_SD, resp_lab, fig_theme, ...) that the functions
# beside them close over.  Sourcing them into a private environment with
# sys.source() makes those functions resolve their constants there, so the
# scripts themselves need no rewriting and nothing leaks into the caller's
# workspace -- barg_data.R's mod_dat, ROPE_SD and variables collide by name
# with objects paper/_analysis.R defines for its own purposes.
#
# Two contexts rather than one, because targets invalidates on the whole
# object: a change to a figure helper must not refit eleven MCMC models.
#
#   ctx_model   barg_data.R + barg_priors.R          feeds the fits
#   ctx_report  the above + quantities, theme, main  feeds tables and figures
#
# Both take the two Excel files as arguments so that _targets.R can track
# them with format = "file" and refit when the data change.
# ==========================================================================

barg_context <- function(scraper_xlsx, site_xlsx, scripts) {
  e <- new.env(parent = globalenv())
  e$SCRAPER_XLSX <- normalizePath(scraper_xlsx, winslash = "/", mustWork = TRUE)
  e$SITE_XLSX    <- normalizePath(site_xlsx,    winslash = "/", mustWork = TRUE)
  for (s in scripts)
    sys.source(here::here("paper", "barg", s), envir = e)
  e
}

# The fits depend on the data and the priors and on nothing else.
barg_context_model <- function(scraper_xlsx, site_xlsx)
  barg_context(scraper_xlsx, site_xlsx,
               c("barg_data.R", "barg_priors.R"))

# Everything downstream of the fits: the derived quantities, the figure style,
# the two-panel main figure and the report figures.  barg_priors.R is included
# because the report prints the prior specifications beside the results.
barg_context_report <- function(scraper_xlsx, site_xlsx)
  barg_context(scraper_xlsx, site_xlsx,
               c("barg_data.R", "barg_priors.R", "barg_quantities.R",
                 "barg_theme.R", "barg_main_figure.R", "barg_figures.R"))

# A third context, for the posterior SBC refits of barg_sbc.R.  It is the model
# context plus barg_quantities.R, because the check ranks the seven ICCs as
# well as the 21 slopes and so needs icc_draws(); it deliberately stops short
# of the theme and the figure code, so that editing a figure cannot invalidate
# a run of a hundred refits.  The figure that draws the result takes
# ctx_report instead.
barg_context_sbc <- function(scraper_xlsx, site_xlsx)
  barg_context(scraper_xlsx, site_xlsx,
               c("barg_data.R", "barg_priors.R", "barg_quantities.R"))
