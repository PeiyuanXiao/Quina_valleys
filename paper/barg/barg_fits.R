# ==========================================================================
# barg_fits.R -- every MCMC run behind paper/barg/barg_report.qmd.
#
# Reads:   data/Quina_scraper_surface.xlsx, data/Site_information.xlsx
#          via paper/barg/barg_data.R
# Writes:  paper/barg/fits/*.rds   (one brmsfit per specification)
#          paper/barg/fits/manifest.rds
#
# Run from the project root:   Rscript paper/barg/barg_fits.R
# Set BARG_QUICK=1 for a fast pipeline smoke test (iter = 800, chains = 2,
# written to paper/barg/fits_quick/ so the real cache is never overwritten).
#
# Eleven runs, all on the same 165 specimens, with the same seed and backend:
#
#   ref        reference prior, the model the report estimates from
#   noland     ~ 1 + (1 |p| Locality); no landscape terms.  Its ICC answers
#              Q1 without the landscape terms competing for the same variance
#   prior_ref  sample_prior = "only" under the reference prior (BARG 1.E)
#   s1 .. s6   one-at-a-time prior changes (BARG Step 5).  s6 is the odd one:
#              it fixes coi at 1 rather than widening a prior, making the two
#              zero-one-inflated beta responses one-inflated (they have no zeros)
#   ref_rg_ln  RG refitted as lognormal, the fallback if the gaussian
#              posterior predictive check fails (BARG 3.A)
#   ref_basinx one height slope and one distance slope per basin instead of
#              one of each shared by both: the test of whether the landscape
#              effects are uniform or basin-dependent (BARG Step 5)
#
# Nothing is refitted if the .rds already exists, so re-rendering the report
# never triggers sampling.  Delete a file to force its refit.
# ==========================================================================
suppressPackageStartupMessages({
  library(here); library(brms); library(posterior)
})
source(here("paper", "barg", "barg_data.R"))
source(here("paper", "barg", "barg_priors.R"))

QUICK <- nzchar(Sys.getenv("BARG_QUICK"))
CHAINS <- if (QUICK) 2 else 4
ITER   <- if (QUICK) 800 else 10000     # 20000 post-warmup draws in the real run
CORES  <- if (QUICK) 2 else 4
FITDIR <- here("paper", "barg", if (QUICK) "fits_quick" else "fits")
dir.create(FITDIR, showWarnings = FALSE, recursive = TRUE)

# the backend is locked: a fit produced by rstan is not bit-for-bit the same
# as one produced by cmdstanr, and the report quotes a CmdStan version
stopifnot(requireNamespace("cmdstanr", quietly = TRUE))
CMDSTAN_VERSION <- cmdstanr::cmdstan_version()
BACKEND <- "cmdstanr"

# Stamped on every fit this run produces and saved inside the .rds, so that a
# cache assembled across more than one session reports the environment each
# fit was actually made in rather than the environment of the latest run.
ENV_NOW <- c(r_version = R.version.string,
             brms      = as.character(utils::packageVersion("brms")),
             cmdstan   = CMDSTAN_VERSION)

fam_of <- function(f) switch(f, lognormal = lognormal(),
                             zoib = zero_one_inflated_beta(),
                             negbinomial = negbinomial(), gaussian = gaussian())

make_formula <- function(rhs, fams = resp_fam) {
  Reduce(`+`, lapply(names(fams), function(r)
    bf(as.formula(paste(r, "~", rhs)), family = fam_of(fams[[r]])))) + set_rescor(FALSE)
}

RHS_FULL  <- "Basin + zHeight + zDistance + (1 |p| Locality)"
RHS_NULL  <- "1 + (1 |p| Locality)"
# One gradient slope per basin.  A slope that varies by LOCALITY is not
# identified -- all three predictors are locality attributes, so within a
# locality the predictor never moves and the random slope is collinear with
# the random intercept.  The basin is the finest level at which the question
# can be put, because height and distance do vary across the localities inside
# each basin.  Two basins carry no variance to estimate, so this is a fixed
# interaction rather than (zHeight | Basin).
RHS_INTER <- "Basin * zHeight + Basin * zDistance + (1 |p| Locality)"

# --------------------------------------------------------------------------
fit_cached <- function(name, formula, prior, sample_prior = "no") {
  f <- file.path(FITDIR, paste0(name, ".rds"))
  if (file.exists(f)) {
    message(sprintf("[cache] %-10s %s", name, basename(f)))
    return(readRDS(f))
  }
  message(sprintf("[fit  ] %-10s chains=%d iter=%d ...", name, CHAINS, ITER))
  t0 <- Sys.time()
  fit <- brm(formula, data = mod_dat, prior = prior,
             sample_prior = sample_prior,
             chains = CHAINS, iter = ITER, cores = CORES,
             seed = SEED, backend = BACKEND, refresh = 0, silent = 2)
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  attr(fit, "barg_minutes") <- el
  attr(fit, "barg_name")    <- name
  attr(fit, "barg_env")     <- ENV_NOW
  saveRDS(fit, f, compress = "xz")
  message(sprintf("[done ] %-10s %.1f min -> %s", name, el,
                  format(structure(file.size(f), class = "object_size"), units = "MB")))
  fit
}

# --------------------------------------------------------------------------
fits <- list()

# 1. the reference fit
fits$ref <- fit_cached("ref", make_formula(RHS_FULL), prior_specs$REF$prior)

# 2. prior predictive draws under the reference prior (BARG 1.E)
fits$prior_ref <- fit_cached("prior_ref", make_formula(RHS_FULL),
                             prior_specs$REF$prior, sample_prior = "only")

# 3. the model with no landscape terms (Q1, BARG-independent)
fits$noland <- fit_cached("noland", make_formula(RHS_NULL), prior_noland)

# 4. the five one-at-a-time prior changes (BARG Step 5)
for (nm in c("S1", "S2", "S3", "S4", "S5", "S6"))
  fits[[tolower(nm)]] <- fit_cached(tolower(nm), make_formula(RHS_FULL),
                                    prior_specs[[nm]]$prior)

# 5. the RG fallback family, used only if the gaussian check fails (BARG 3.A)
fam_rg_ln <- resp_fam; fam_rg_ln[["RG"]] <- "lognormal"
prior_rg_ln <- local({
  keep <- prior_specs$REF$prior
  # everything RG-specific has to be respecified: the response moves from the
  # identity link to the log link, so its slope, sigma and intercept priors
  # are all on a different scale
  keep <- keep[!(keep$resp == "RG" & keep$class %in% c("sigma", "b", "Intercept")), ]
  ls_rg <- sd(log(mod_dat$RG))
  c(keep,
    set_prior(sprintf("normal(0, %.6f)", ls_rg),        class = "b",         resp = "RG"),
    set_prior(sprintf("student_t(3, 0, %.6f)", ls_rg),  class = "sigma",     resp = "RG"),
    set_prior("normal(1, 1)",                           class = "Intercept", resp = "RG"))
})
fits$ref_rg_ln <- fit_cached("ref_rg_ln",
                             make_formula(RHS_FULL, fam_rg_ln), prior_rg_ln)

# 6. one gradient slope per basin (BARG Step 5).  The reference prior carries
# over untouched: class "b" with no coef applies to every population-level
# coefficient of a response, the two new interaction terms included.
fits$ref_basinx <- fit_cached("ref_basinx", make_formula(RHS_INTER),
                              prior_specs$REF$prior)

# --------------------------------------------------------------------------
# Per-fit diagnostics, computed once here and carried in the manifest so that
# the report can print them without reopening eleven 28 MB objects.
diagnostics <- do.call(rbind, lapply(names(fits), function(nm) {
  f  <- fits[[nm]]
  s  <- summarise_draws(as_draws_df(f), "rhat", "ess_bulk", "ess_tail")
  s  <- s[!is.na(s$rhat) & !startsWith(s$variable, "lp"), ]
  np <- nuts_params(f)
  data.frame(fit = nm, n_par = nrow(s),
             max_rhat = max(s$rhat),
             min_ess_bulk = min(s$ess_bulk), min_ess_tail = min(s$ess_tail),
             divergent = sum(np$Value[np$Parameter == "divergent__"]),
             max_treedepth = sum(np$Value[np$Parameter == "treedepth__"] >= 10),
             row.names = NULL)
}))

# Environment per fit.  A fit made before ENV_NOW was stamped carries no
# attribute of its own; it inherits what the previous manifest recorded for
# it, which is the environment it was made in.  The scalar fields below
# collapse to one string when every fit agrees and list the distinct values
# when they do not, so the report can never claim a uniformity the cache does
# not have.
prev_mf <- tryCatch(readRDS(file.path(FITDIR, "manifest.rds")),
                    error = function(e) NULL)
env_of <- function(nm, f) {
  a <- attr(f, "barg_env")
  if (!is.null(a)) return(a[c("r_version", "brms", "cmdstan")])
  if (!is.null(prev_mf$env_by_fit) && nm %in% rownames(prev_mf$env_by_fit))
    return(prev_mf$env_by_fit[nm, ])
  if (!is.null(prev_mf))
    return(c(r_version = prev_mf$r_version, brms = prev_mf$brms,
             cmdstan = prev_mf$cmdstan))
  ENV_NOW
}
env_by_fit <- do.call(rbind, lapply(names(fits), function(nm) env_of(nm, fits[[nm]])))
rownames(env_by_fit) <- names(fits)
collapse1 <- function(v) paste(unique(v), collapse = "; ")

manifest <- list(
  diagnostics = diagnostics,
  fitted_at   = Sys.time(),
  chains = CHAINS, iter = ITER, warmup = ITER / 2, cores = CORES,
  ndraws      = CHAINS * ITER / 2,
  seed        = SEED, backend = BACKEND,
  cmdstan     = collapse1(env_by_fit[, "cmdstan"]),
  brms        = collapse1(env_by_fit[, "brms"]),
  r_version   = collapse1(env_by_fit[, "r_version"]),
  env_by_fit  = env_by_fit,
  quick       = QUICK,
  files       = list.files(FITDIR, pattern = "[.]rds$"),
  minutes     = vapply(fits, function(f) {
                  m <- attr(f, "barg_minutes"); if (is.null(m)) NA_real_ else m },
                  numeric(1)))
saveRDS(manifest, file.path(FITDIR, "manifest.rds"))

message("\n--- convergence at a glance ---")
for (nm in names(fits)) {
  if (nm == "prior_ref") next
  s <- summarise_draws(as_draws_df(fits[[nm]]), "rhat", "ess_bulk")
  s <- s[!is.na(s$rhat) & !startsWith(s$variable, "lp"), ]
  np <- nuts_params(fits[[nm]])
  message(sprintf("%-10s max Rhat %.4f   min bulk ESS %6.0f   divergences %d",
                  nm, max(s$rhat), min(s$ess_bulk),
                  sum(np$Value[np$Parameter == "divergent__"])))
}
