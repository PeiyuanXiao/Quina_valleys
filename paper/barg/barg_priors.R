# The prior specifications for every fit.  Requires barg_data.R first.
#
# Every class that appears in get_prior() is given an explicit prior, none
# left at a brms default: BARG Step 1.C asks for the prior in full, and
# sample_prior = "only" needs every prior to be proper.
suppressPackageStartupMessages(library(brms))

# Intercept priors, set from the measurement scales rather than the sample:
# the range an archaeologist would have called possible for a retouched flake
# tool before seeing these 165 pieces, mapped through the link.
intercept_prior <- c(
  Thickness = "normal(3.2, 0.7)",   # log mm; 95% ~ 6-100 mm
  GMsize    = "normal(3.7, 0.6)",   # log mm; 95% ~ 12-134 mm
  GIUR      = "normal(0, 1.5)",     # logit; 95% ~ 0.05-0.95, GIUR is 0-1 by construction
  RLI       = "normal(0, 1.5)",     # logit; same
  NScar     = "normal(3, 1.0)",     # log count; 95% ~ 3-143 scars
  EdgeAngle = "normal(65, 20)",     # degrees; 95% ~ 26-104, abrupt retouch is >= 60-65
  RG        = "normal(3, 2)")       # generations; the recording protocol counts 1-5

# sigma is scaled to the link-scale SD of its own response, for the same
# reason the slopes are: a half-t with scale 2 says something different about
# edge angle (SD 8.1 degrees) than about log thickness (SD 0.30).  phi and
# shape take proper gammas; the brms defaults put mass arbitrarily close to
# zero and make the prior predictive distribution degenerate.
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

# A full prior from four interchangeable pieces: `b` and `sd` are functions of
# the response name, `cor` a string for the 7 x 7 LKJ, `coi` a string for the
# conditional one-inflation probability of the two zoib responses.
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
  # S6 differs in kind: S1-S5 change how much the data may say, S6 removes a
  # parameter the data cannot inform.  GIUR and the retouched perimeter have
  # no zeros, so fixing coi at 1 gives the one-inflated beta the data actually
  # describe.  The zoib likelihood factorises, so this moves no slope, phi or
  # ICC; the report demonstrates it.
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
