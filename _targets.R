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
# There is one pipeline and no reduced mode: the BARG_QUICK / BARG_NOREFIT
# environment variables, the fits_quick/ directory and the short-lived "quick"
# targets project are all gone.  A reduced run was worth having when a rebuild
# meant refitting everything by hand; now targets rebuilds only what changed,
# so the honest full run is also usually the fast one, and a mode that skips
# rendering cannot catch the errors that only rendering exposes.
# ==========================================================================
library(targets)
library(tarchetypes)

if (!nzchar(Sys.which("Rscript")))
  Sys.setenv(PATH = paste(R.home("bin"), Sys.getenv("PATH"),
                          sep = .Platform$path.sep))

tar_option_set(
  packages = c("here", "brms", "posterior", "dplyr", "tidyr", "ggplot2",
               "patchwork", "bayesplot", "scales", "readxl", "cmdstanr"),
  format = "rds",
  seed = 2226,
  memory = "transient",
  garbage_collection = TRUE
)

tar_source(c("paper/barg/barg_context.R", "paper/barg/barg_models.R",
             "paper/barg/barg_sbc.R"))

# ---- sampler and figure settings -----------------------------------------
CHAINS      <- 4L
ITER        <- 10000L
CORES       <- 4L
NDRAWS      <- 100L
STAT_NDRAWS <- 1000L
THIN        <- 5L
FIGDIR      <- here::here("paper", "barg", "figures")

# ---- posterior SBC -------------------------------------------------------
# N_SBC simulations, each one refit of the reference model to a dataset the
# reference posterior generated, dealt round-robin into N_SBC_BATCH branches.
# The batches exist for two reasons: one Stan compilation is amortised over the
# simulations inside a batch, and an interrupted run resumes at the last
# completed batch rather than at the beginning.  Round-robin dealing means a
# partial result is still spread over the whole reference posterior rather than
# over its first tenth.
#
# 100 simulations put the simultaneous 95% band on the rank ECDF at about
# 0.135, which is the resolution of the check: a miscalibration smaller than
# that would not show.  Each refit costs about two minutes against about three
# for a reference fit -- a rank needs only 4,000 draws where the three-decimal
# quantiles of the report need 20,000, but each refit conditions on 330 rows
# rather than 165, because posterior SBC requires the observed data in the
# refit alongside the simulated data (see barg_sbc.R).  Raising N buys
# resolution slowly: the cost is linear in N while the band shrinks only as
# 1/sqrt(N), so doubling to 200 would cost twice as much for a band of 0.095.
N_SBC       <- 100L
N_SBC_BATCH <- 10L

fit_specs <- data.frame(spec = BARG_SPECS, stringsAsFactors = FALSE)

# ==========================================================================
fits <- tar_map(
  values = fit_specs,
  names = spec,
  tar_target(fit,  barg_fit_one(ctx_model, spec, CHAINS, ITER, CORES)),
  tar_target(diag, barg_diagnostics(fit, spec))
)

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
  tar_target(sbc_src,
             file.path(here::here("paper", "barg"),
                       c("barg_data.R", "barg_priors.R", "barg_quantities.R")),
             format = "file"),

  # ---- the three contexts ------------------------------------------------
  tar_target(ctx_model,  { barg_model_src
                           barg_context_model(scraper_xlsx, site_xlsx) }),
  tar_target(ctx_report, { barg_report_src
                           barg_context_report(scraper_xlsx, site_xlsx) }),
  tar_target(ctx_sbc,    { sbc_src
                           barg_context_sbc(scraper_xlsx, site_xlsx) }),

  # ---- the eleven fits and their diagnostics -----------------------------
  fits,
  tar_combine(barg_diag_tbl, fits[["diag"]],
              command = dplyr::bind_rows(!!!.x)),

  # ---- provenance --------------------------------------------------------
  tar_target(barg_mf, barg_manifest(barg_diag_tbl, seed = 2226,
                                    chains = CHAINS, iter = ITER,
                                    cores = CORES)),

  # ---- derived quantities and figures ------------------------------------
  tar_target(ppc_stat_tbl,
             ctx_report$barg_ppc_stats(fit_ref, nd = STAT_NDRAWS)),

  sens,
  tar_combine(sens_long, sens[["sens_draw"]],
              command = dplyr::bind_rows(!!!.x)),
  tar_target(barg_figs,
             ctx_report$barg_figures(
               list(ref = fit_ref, prior_ref = fit_prior_ref,
                    noland = fit_noland),
               ppc_stat_tbl, sens_long,
               ndraws = NDRAWS, stat_ndraws = STAT_NDRAWS, dir = FIGDIR),
             format = "file"),

  # ---- posterior SBC of the reference fit ---------------------------------
  # The one computational check the convergence diagnostics cannot supply: it
  # asks whether the posterior is calibrated, not whether the sampler explored
  # it.  fit_ref supplies both the parameter vectors treated as ground truth
  # and the simulated datasets; nothing else here reads it.
  tar_target(sbc_batch_id, seq_len(N_SBC_BATCH)),
  tar_target(sbc_ranks_b,
             barg_sbc_batch(ctx_sbc, fit_ref, sbc_batch_id, N_SBC, N_SBC_BATCH),
             pattern = map(sbc_batch_id), iteration = "list"),
  tar_target(sbc_ranks, dplyr::bind_rows(sbc_ranks_b)),
  tar_target(sbc_unif,  barg_sbc_uniformity(sbc_ranks)),
  tar_target(sbc_fig,   barg_sbc_figure(ctx_report, sbc_ranks, dir = FIGDIR),
             format = "file"),

  # ---- the frequentist half ----------------------------------------------
  tar_target(analysis_env, {
    list(scraper_xlsx, site_xlsx, longtan_xlsx, basin_xlsx, analysis_src)
    e <- new.env(parent = globalenv())
    sys.source(here::here("paper", "_analysis.R"), envir = e)
    e
  }),

  # ---- the documents -----------------------------------------------------
  tar_quarto(barg_report,   path = "paper/barg/barg_report.qmd"),
  tar_quarto(manuscript,    path = "paper/manuscript.qmd"),
  tar_quarto(supplementary, path = "paper/supplementary.qmd")
)
