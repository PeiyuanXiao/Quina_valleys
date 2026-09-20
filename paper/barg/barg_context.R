# The shared environments the pipeline runs in.  The barg_*.R scripts define
# constants (resps, link_sd, ROPE_SD, resp_lab, fig_theme) that the functions
# beside them close over; sys.source() into a private environment makes those
# closures resolve there, and keeps names like mod_dat and ROPE_SD from
# colliding with paper/_analysis.R's own.
#
# Three contexts rather than one, because targets invalidates on the whole
# object: editing a figure helper must not refit eleven MCMC models.  The two
# Excel files are arguments so that _targets.R can track them by content.

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

# Everything downstream of the fits.  barg_priors.R is included because the
# report prints the prior specifications beside the results.
barg_context_report <- function(scraper_xlsx, site_xlsx)
  barg_context(scraper_xlsx, site_xlsx,
               c("barg_data.R", "barg_priors.R", "barg_quantities.R",
                 "barg_theme.R", "barg_main_figure.R", "barg_figures.R"))

# For the posterior SBC refits: the model context plus barg_quantities.R,
# because the check ranks the seven ICCs as well as the 21 slopes and so needs
# icc_draws().  It stops short of the figure code so that editing a figure
# cannot invalidate a hundred refits; the figure itself takes ctx_report.
barg_context_sbc <- function(scraper_xlsx, site_xlsx)
  barg_context(scraper_xlsx, site_xlsx,
               c("barg_data.R", "barg_priors.R", "barg_quantities.R"))
