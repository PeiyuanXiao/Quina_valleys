# ==========================================================================
# _targets.R -- the whole compendium as one pipeline.
#
#   tar_make()          builds whatever is out of date and renders the three
#                       documents.  That is the only command a reader needs.
#   tar_visnetwork()    shows the graph and what is stale.
#   tar_make(names = c("fit_ref"))   builds one target and its dependencies.
#
# What targets replaces: paper/barg/barg_fits.R used to keep its own cache in
# paper/barg/fits/, invalidated on file existence alone -- editing a prior or
# the data left the stale fit in place and the report re-rendered from it.
# Here the fits are hashed against the two Excel files and the code that makes
# them, so a change refits exactly what the change touched and nothing else.
# The BARG_QUICK / BARG_NOREFIT environment variables and the fits_quick/
# directory are gone; see _targets.yaml for the reduced-iteration project.
#
# Run from the project root.  The store is _targets/ (git-ignored, ~300 MB);
# it is the object to archive on Zenodo, not paper/barg/fits/.
# ==========================================================================
library(targets)
library(tarchetypes)

# Quarto resolves Rscript from PATH and ignores R_HOME, so a machine where R
# is not on PATH renders nothing.  Harmless where Rscript is already found.
if (!nzchar(Sys.which("Rscript")))
  Sys.setenv(PATH = paste(R.home("bin"), Sys.getenv("PATH"),
                          sep = .Platform$path.sep))

tar_option_set(
  packages = c("here", "brms", "posterior", "dplyr", "tidyr", "ggplot2",
               "patchwork", "bayesplot", "scales", "readxl", "cmdstanr"),
  # rds rather than qs: a brmsfit made with the cmdstanr backend carries an
  # external pointer, and saveRDS/readRDS is the round trip this project has
  # always used for these objects.
  format = "rds",
  seed = 2226,
  # eleven brmsfit objects are ~28 MB each; keep only what a target needs
  memory = "transient",
  garbage_collection = TRUE
)

tar_source(c("paper/barg/barg_context.R", "paper/barg/barg_models.R"))

# ---- sampler settings ----------------------------------------------------
# The "quick" project of _targets.yaml runs a reduced pipeline into its own
# store, so a smoke test can never overwrite the real fits.  This replaces the
# old BARG_QUICK environment variable and the fits_quick/ directory.
QUICK       <- identical(Sys.getenv("TAR_PROJECT"), "quick")
CHAINS      <- if (QUICK) 2L else 4L
ITER        <- if (QUICK) 800L else 10000L   # 20000 post-warmup draws when full
CORES       <- if (QUICK) 2L else 4L
NDRAWS      <- if (QUICK) 60L else 100L      # overlay draws in predictive checks
STAT_NDRAWS <- if (QUICK) 200L else 1000L
THIN        <- if (QUICK) 1L else 5L         # ECDF thinning, sensitivity panels
FIGDIR      <- here::here("paper", "barg",
                          if (QUICK) "figures_quick" else "figures")

fit_specs <- data.frame(spec = BARG_SPECS, stringsAsFactors = FALSE)

# ==========================================================================
fits <- tar_map(
  values = fit_specs,
  names = spec,
  tar_target(fit,  barg_fit_one(ctx_model, spec, CHAINS, ITER, CORES)),
  tar_target(diag, barg_diagnostics(fit, spec))
)

# The seven fits the sensitivity ECDFs overlay, each reduced to the quantities
# those panels need in a target of its own. One fit is opened at a time: eleven
# full brmsfit objects do not fit in 16 GB together, and the reductions are
# small enough that redrawing the panels never reopens a fit.
sens_specs <- list(
  spec   = c("ref", "s1", "s2", "s3", "s4", "s5", "s6"),
  label  = c("REF", "S1", "S2", "S3", "S4", "S5", "S6"),
  fitsym = rlang::syms(paste0("fit_", c("ref", "s1", "s2", "s3", "s4", "s5", "s6")))
)

sens <- tar_map(
  values = sens_specs,
  names = spec,
  tar_target(sens_draw, ctx_report$barg_sens_one(fitsym, label, THIN))
)

list(

  # ---- the data, tracked by content ------------------------------------
  tar_target(scraper_xlsx, here::here("data", "Quina_scraper_surface.xlsx"),
             format = "file"),
  tar_target(site_xlsx,    here::here("data", "Site_information.xlsx"),
             format = "file"),
  tar_target(longtan_xlsx, here::here("data", "Longtan_lithic_tools.xlsx"),
             format = "file"),
  tar_target(basin_xlsx,   here::here("data", "Raw_mat_basin.xlsx"),
             format = "file"),

  # ---- the code, also tracked by content ---------------------------------
  # barg_context.R and barg_models.R are loaded by tar_source() and tracked as
  # functions. The scripts the contexts sys.source() at run time are not seen
  # by that static analysis, so they are declared here. Without this, editing
  # a prior would leave the fits untouched -- exactly the silent staleness the
  # old file-existence cache suffered from, reintroduced through the back door.
  tar_target(barg_model_src,
             file.path(here::here("paper", "barg"),
                       c("barg_data.R", "barg_priors.R")),
             format = "file"),
  tar_target(barg_report_src,
             file.path(here::here("paper", "barg"),
                       c("barg_data.R", "barg_priors.R", "barg_quantities.R",
                         "barg_theme.R", "barg_main_figure.R", "barg_figures.R")),
             format = "file"),
  tar_target(analysis_src,
             file.path(here::here("paper"), c("_analysis.R", "_packages.R")),
             format = "file"),

  # ---- the two contexts --------------------------------------------------
  # Split so that editing a figure helper cannot invalidate eleven MCMC fits:
  # the fits see only the data and the priors.
  tar_target(ctx_model,  { barg_model_src
                           barg_context_model(scraper_xlsx, site_xlsx) }),
  tar_target(ctx_report, { barg_report_src
                           barg_context_report(scraper_xlsx, site_xlsx) }),

  # ---- the eleven fits and their diagnostics -----------------------------
  fits,
  tar_combine(barg_diag_tbl, fits[["diag"]],
              command = dplyr::bind_rows(!!!.x)),

  # ---- provenance --------------------------------------------------------
  # One tar_make() produces every fit in one session, so the R, brms and
  # CmdStan versions are single values rather than the per-fit reconciliation
  # the old manifest had to carry.
  tar_target(barg_mf, barg_manifest(barg_diag_tbl, seed = 2226,
                                    chains = CHAINS, iter = ITER,
                                    cores = CORES, quick = QUICK)),

  # ---- derived quantities and figures ------------------------------------
  tar_target(ppc_stat_tbl,
             ctx_report$barg_ppc_stats(fit_ref, nd = STAT_NDRAWS)),

  sens,
  tar_combine(sens_long, sens[["sens_draw"]],
              command = dplyr::bind_rows(!!!.x)),

  # Only three fits are open here; the sensitivity panels are drawn from the
  # reductions above.  The quick project writes to its own directory: the store
  # is separate but the figure paths would not be, and a smoke test must not
  # overwrite the figures the report carries.
  tar_target(barg_figs,
             ctx_report$barg_figures(
               list(ref = fit_ref, prior_ref = fit_prior_ref,
                    noland = fit_noland),
               ppc_stat_tbl, sens_long,
               ndraws = NDRAWS, stat_ndraws = STAT_NDRAWS, dir = FIGDIR),
             format = "file"),

  # ---- the frequentist half ----------------------------------------------
  # paper/_analysis.R is one target, not 245.  It runs in seconds and writes
  # nothing, so there is nothing for targets to save by splitting it; and it
  # sets one seed at the top and advances the RNG in source order, which a
  # dependency-ordered split would silently change.  Its 156 inline references
  # in manuscript.qmd keep working because the whole environment is loaded.
  tar_target(analysis_env, {
    # declares the dependencies; _analysis.R resolves the data paths itself
    # with here(), and analysis_src carries its own source and _packages.R
    list(scraper_xlsx, site_xlsx, longtan_xlsx, basin_xlsx, analysis_src)
    e <- new.env(parent = globalenv())
    sys.source(here::here("paper", "_analysis.R"), envir = e)
    e
  }),

  # ---- the documents -----------------------------------------------------
  # tar_quarto() reads each .qmd for tar_load()/tar_read() calls and wires the
  # dependencies itself, so rendering is part of the pipeline: tar_make() is
  # the single command, with no separate render step.
  #
  # The quick project stops short of rendering. Its store is separate, but the
  # three documents are not: they are written to the same paths whichever
  # project produced them, so a smoke test that rendered would overwrite the
  # real manuscript with reduced-iteration numbers. Under TAR_PROJECT=quick
  # the pipeline therefore checks the computation and leaves the documents
  # alone.
  if (QUICK) NULL else list(
    tar_quarto(barg_report,   path = "paper/barg/barg_report.qmd"),
    tar_quarto(manuscript,    path = "paper/manuscript.qmd"),
    tar_quarto(supplementary, path = "paper/supplementary.qmd")
  )
)
