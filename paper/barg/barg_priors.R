# ==========================================================================
# barg_priors.R -- the prior specifications for every fit in the BARG report.
#
# Sourced into the model and report contexts by barg_context.R; the report
# specifications without refitting anything).  Requires barg_data.R first.
#
# Every class that appears in get_prior() is given an explicit prior.  None
# is left at a brms default, both because BARG Step 1.C asks for the prior to
# be reported in full and because sample_prior = "only" needs every prior to
# be proper.
# ==========================================================================
suppressPackageStartupMessages(library(brms))

# --------------------------------------------------------------------------
# Intercept priors.  Set from the measurement scales, not from the sample:
# the range an archaeologist would have called possible for a retouched
# flake tool before seeing these 165 pieces, mapped through the link.
# Each is deliberately far wider than the observed spread of the response.
# --------------------------------------------------------------------------
intercept_prior <- c(
  Thickness = "normal(3.2, 0.7)",   # log mm; 95% ~ 6-100 mm
  GMsize    = "normal(3.7, 0.6)",   # log mm; 95% ~ 12-134 mm
  GIUR      = "normal(0, 1.5)",     # logit; 95% ~ 0.05-0.95, GIUR is 0-1 by construction
  RLI       = "normal(0, 1.5)",     # logit; same
  NScar     = "normal(3, 1.0)",     # log count; 95% ~ 3-143 scars
  EdgeAngle = "normal(65, 20)",     # degrees; 95% ~ 26-104, abrupt retouch is >= 60-65
  RG        = "normal(3, 2)")       # generations; the recording protocol counts 1-5

# Auxiliary-parameter priors.  sigma is scaled to the link-scale SD of its
# own response, for the same reason the slopes are: a half-t with scale 2 is
# a different statement about edge angle (SD 8.1 degrees) than about log
# thickness (SD 0.30).  phi and shape are given proper gamma priors; the brms
# defaults gamma(0.01, 0.01) and inv_gamma(0.4, 0.3) put mass arbitrarily
# close to zero, which makes the prior predictive distribution degenerate.
aux_prior <- function(r, coi = "beta(1, 1)") {
  s <- sprintf("student_t(3, 0, %.6f)", link_sd[[r]])
  switch(resp_fam[[r]],
    lognormal   = set_prior(s, class = "sigma", resp = r),
    gaussian    = set_prior(s, class = "sigma", resp = r),
    negbinomial = set_prior("gamma(2, 0.1)", class = "shape", resp = r),
    zoib        = c(set_prior("gamma(2, 0.1)", class = "phi", resp = r),
                    set_prior("beta(1, 1)",    class = "zoi", resp = r),
                    set_prior(coi,             class = "coi", resp = r)))
}

# --------------------------------------------------------------------------
# build_prior(b, sd, cor, coi): assemble a full prior from four interchangeable
# pieces.  `b` and `sd` are functions of the response name returning a
# brms prior string; `cor` is a single string for the 7 x 7 LKJ; `coi` is a
# single string for the conditional-one-inflation probability of the two
# zero-one-inflated beta responses, and is the only piece that touches a
# parameter no decision in the report depends on.
# --------------------------------------------------------------------------
build_prior <- function(b, sd, cor = "lkj(1)", coi = "beta(1, 1)") {
  do.call(c, c(
    lapply(resps, function(r) c(
      set_prior(b(r),                 class = "b",         resp = r),
      set_prior(intercept_prior[[r]], class = "Intercept", resp = r),
      set_prior(sd(r),                class = "sd",        resp = r, group = "Locality"),
      aux_prior(r, coi))),
    list(set_prior(cor, class = "cor", group = "Locality"))))
}

# ---- the six prior specifications of the sensitivity analysis ------------
b_link   <- function(r) sprintf("normal(0, %.6f)", link_sd[[r]])
b_flat1  <- function(r) "normal(0, 1)"
b_wide   <- function(r) sprintf("normal(0, %.6f)", 2.5 * link_sd[[r]])
sd_t3    <- function(r) "student_t(3, 0, 2)"
sd_exp   <- function(r) "exponential(1)"
sd_norm  <- function(r) "normal(0, 1)"

prior_specs <- list(
  REF = list(prior = build_prior(b_link,  sd_t3,   "lkj(1)"),
             label = "b: normal(0, SD_link);  sd: student_t(3, 0, 2);  cor: lkj(1)",
             note  = "reference"),
  S1  = list(prior = build_prior(b_flat1, sd_t3,   "lkj(1)"),
             label = "b: normal(0, 1)",
             note  = "the original specification, kept for comparison"),
  S2  = list(prior = build_prior(b_wide,  sd_t3,   "lkj(1)"),
             label = "b: normal(0, 2.5 x SD_link)",
             note  = "slopes two and a half times wider"),
  S3  = list(prior = build_prior(b_link,  sd_exp,  "lkj(1)"),
             label = "sd: exponential(1)",
             note  = "locality SD; 26 clusters make this prior influential"),
  S4  = list(prior = build_prior(b_link,  sd_norm, "lkj(1)"),
             label = "sd: normal(0, 1) truncated at zero",
             note  = "locality SD, a lighter tail than the half-t"),
  S5  = list(prior = build_prior(b_link,  sd_t3,   "lkj(2)"),
             label = "cor: lkj(2)",
             note  = "26 clusters and 21 correlations; lkj(2) shrinks towards zero"),
  # S6 differs from the others in kind.  S1 to S5 change how much the data are
  # allowed to say about a quantity the report decides on; S6 removes a
  # parameter the data cannot inform at all.  GIUR and the retouched perimeter
  # have no zeros, so `coi` -- the probability that an inflated value is a one
  # rather than a zero -- is estimating a proportion whose denominator is
  # entirely ones.  Fixing it at 1 makes the family the one-inflated beta the
  # data actually describe, and it removes the zero-inflation half that nothing
  # supports.  See the demonstration in the report: because the zoib likelihood
  # factorises, this cannot move any slope, phi or ICC.
  S6  = list(prior = build_prior(b_link,  sd_t3,   "lkj(1)", coi = "constant(1)"),
             label = "coi: constant(1)",
             note  = "one-inflated beta; there are no zeros for coi to describe"))

# The reduced model has no population-level slopes at all, so it takes the
# reference prior minus the class-b lines.
prior_noland <- do.call(c, c(
  lapply(resps, function(r) c(
    set_prior(intercept_prior[[r]], class = "Intercept", resp = r),
    set_prior(sd_t3(r),             class = "sd",        resp = r, group = "Locality"),
    aux_prior(r))),
  list(set_prior("lkj(1)", class = "cor", group = "Locality"))))
