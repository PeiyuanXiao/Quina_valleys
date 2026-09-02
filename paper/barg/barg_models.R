# ==========================================================================
# barg_models.R -- the model specifications, the sampler call and the
# per-fit diagnostics behind paper/barg/barg_report.qmd.
#
# This file fits nothing on its own and writes nothing.  _targets.R calls
# barg_fit_one() once per specification and stores the result; targets
# decides what is stale by hashing the data files and this code, so there is
# no cache to keep by hand and no fit to delete to force a refit.  It replaces
# the former barg_fits.R, whose fit_cached() invalidated on file existence
# alone and so silently reused a fit after its priors or its data had changed.
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
# The backend is locked: a fit produced by rstan is not bit-for-bit the same
# as one produced by cmdstanr, and the report quotes a CmdStan version.
# ==========================================================================

BARG_BACKEND <- "cmdstanr"

BARG_SPECS <- c("ref", "prior_ref", "noland",
                "s1", "s2", "s3", "s4", "s5", "s6",
                "ref_rg_ln", "ref_basinx")

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

barg_fam_of <- function(f)
  switch(f,
         lognormal   = brms::lognormal(),
         zoib        = brms::zero_one_inflated_beta(),
         negbinomial = brms::negbinomial(),
         # gaussian() is stats'; brms does not export one
         gaussian    = stats::gaussian())

barg_formula <- function(rhs, fams) {
  bfs <- lapply(names(fams), function(r)
    brms::bf(stats::as.formula(paste(r, "~", rhs)),
             family = barg_fam_of(fams[[r]])))
  Reduce(function(a, b) a + b, bfs) + brms::set_rescor(FALSE)
}

# Everything RG-specific has to be respecified when RG moves to the lognormal:
# the response leaves the identity link for the log link, so its slope, sigma
# and intercept priors are all on a different scale.
barg_prior_rg_ln <- function(ctx) {
  keep  <- ctx$prior_specs$REF$prior
  keep  <- keep[!(keep$resp == "RG" & keep$class %in% c("sigma", "b", "Intercept")), ]
  ls_rg <- stats::sd(log(ctx$mod_dat$RG))
  c(keep,
    brms::set_prior(sprintf("normal(0, %.6f)", ls_rg),       class = "b",         resp = "RG"),
    brms::set_prior(sprintf("student_t(3, 0, %.6f)", ls_rg), class = "sigma",     resp = "RG"),
    brms::set_prior("normal(1, 1)",                          class = "Intercept", resp = "RG"))
}

# ---- one specification, resolved against a model context -----------------
barg_spec <- function(ctx, spec) {
  ps <- ctx$prior_specs
  switch(spec,
    ref        = list(rhs = RHS_FULL,  fams = ctx$resp_fam,
                      prior = ps$REF$prior,      sample_prior = "no"),
    # prior predictive draws under the reference prior (BARG 1.E)
    prior_ref  = list(rhs = RHS_FULL,  fams = ctx$resp_fam,
                      prior = ps$REF$prior,      sample_prior = "only"),
    # the model with no landscape terms (Q1, BARG-independent)
    noland     = list(rhs = RHS_NULL,  fams = ctx$resp_fam,
                      prior = ctx$prior_noland,  sample_prior = "no"),
    ref_rg_ln  = local({
                   fams <- ctx$resp_fam; fams[["RG"]] <- "lognormal"
                   list(rhs = RHS_FULL, fams = fams,
                        prior = barg_prior_rg_ln(ctx), sample_prior = "no") }),
    # the reference prior carries over untouched: class "b" with no coef
    # applies to every population-level coefficient of a response, the two
    # new interaction terms included
    ref_basinx = list(rhs = RHS_INTER, fams = ctx$resp_fam,
                      prior = ps$REF$prior,      sample_prior = "no"),
    # s1 .. s6, the one-at-a-time prior changes
    local({
      key <- toupper(spec)
      if (!key %in% names(ps))
        stop("unknown BARG specification: ", spec, call. = FALSE)
      list(rhs = RHS_FULL, fams = ctx$resp_fam,
           prior = ps[[key]]$prior, sample_prior = "no")
    }))
}

# ---- the sampler ---------------------------------------------------------
# The elapsed time is stamped on the fit because the report tabulates it.
# Nothing else is stamped: targets records the R version, the seed and the
# completion time of every target, so the provenance the old manifest had to
# reconstruct by hand is now read from tar_meta().
barg_fit_one <- function(ctx, spec, chains = 4L, iter = 10000L, cores = 4L) {
  s  <- barg_spec(ctx, spec)
  t0 <- Sys.time()
  fit <- brms::brm(barg_formula(s$rhs, s$fams),
                   data = ctx$mod_dat, prior = s$prior,
                   sample_prior = s$sample_prior,
                   chains = chains, iter = iter, cores = cores,
                   seed = ctx$SEED, backend = BARG_BACKEND,
                   refresh = 0, silent = 2)
  attr(fit, "barg_name")    <- spec
  attr(fit, "barg_minutes") <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  fit
}

# ---- per-fit convergence diagnostics -------------------------------------
barg_diagnostics <- function(fit, spec = attr(fit, "barg_name")) {
  s  <- posterior::summarise_draws(posterior::as_draws_df(fit),
                                   "rhat", "ess_bulk", "ess_tail")
  s  <- s[!is.na(s$rhat) & !startsWith(s$variable, "lp"), ]
  np <- brms::nuts_params(fit)
  m  <- attr(fit, "barg_minutes")
  data.frame(fit = spec, n_par = nrow(s),
             max_rhat = max(s$rhat),
             min_ess_bulk = min(s$ess_bulk), min_ess_tail = min(s$ess_tail),
             divergent = sum(np$Value[np$Parameter == "divergent__"]),
             max_treedepth = sum(np$Value[np$Parameter == "treedepth__"] >= 10),
             minutes = if (is.null(m)) NA_real_ else m,
             row.names = NULL)
}

# ---- the manifest --------------------------------------------------------
# Same shape as the manifest barg_fits.R used to write, so barg_report.qmd and
# paper/supplementary.qmd read it unchanged.  What has gone is the machinery
# that reconciled fits made in different sessions under different R versions:
# one tar_make() produces every fit in one run, so r_version, brms and cmdstan
# are single values and env_by_fit is uniform by construction.
barg_manifest <- function(diagnostics, seed, chains, iter, cores) {
  env1 <- c(r_version = R.version.string,
            brms      = as.character(utils::packageVersion("brms")),
            cmdstan   = cmdstanr::cmdstan_version())
  env_by_fit <- matrix(env1, nrow = nrow(diagnostics), ncol = 3, byrow = TRUE,
                       dimnames = list(diagnostics$fit, names(env1)))
  list(diagnostics = diagnostics,
       fitted_at   = Sys.time(),
       chains = chains, iter = iter, warmup = iter / 2, cores = cores,
       ndraws = chains * iter / 2,
       seed = seed, backend = BARG_BACKEND,
       cmdstan   = unname(env1[["cmdstan"]]),
       brms      = unname(env1[["brms"]]),
       r_version = unname(env1[["r_version"]]),
       env_by_fit = env_by_fit,
       files = paste0(diagnostics$fit, ".rds"),
       minutes = stats::setNames(diagnostics$minutes, diagnostics$fit))
}
