# ==========================================================================
# barg_quantities.R -- every number the report prints, derived from cached
# fits.  Sourced by barg_report.qmd and by barg_figures.R.  Fits nothing.
#
# Requires barg_data.R to have been sourced first.
# ==========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(posterior)
})

# ---- interval and point-summary helpers ----------------------------------
# The report is quantile-based throughout (median + 95% equal-tailed
# interval).  The density-based pair (mode + 95% HDI) is carried alongside
# because the reference BARG document reports both and the extra cost is nil.
eti <- function(x, p = c(0.025, 0.975)) unname(quantile(x, p))

hdi <- function(x, mass = 0.95) {
  x <- sort(x); n <- length(x); k <- floor(mass * n)
  if (k < 1L) return(c(NA_real_, NA_real_))
  w <- x[(k + 1L):n] - x[1L:(n - k)]
  i <- which.min(w)
  c(x[i], x[i + k])
}

pmode <- function(x) {                     # kernel-density mode
  d <- stats::density(x, n = 2048, adjust = 1)
  d$x[which.max(d$y)]
}

mcse_q <- function(x, p) vapply(p, function(pp) posterior::mcse_quantile(x, pp), numeric(1))

# summarise one draw vector the way every table in the report does
draw_summary <- function(x) {
  q <- eti(x); h <- hdi(x); m <- mcse_q(x, c(0.025, 0.5, 0.975))
  data.frame(Median = median(x), CrI_lo = q[1], CrI_hi = q[2],
             Mode = pmode(x), HDI_lo = h[1], HDI_hi = h[2],
             MCSE_lo = m[1], MCSE_med = m[2], MCSE_hi = m[3],
             SD = sd(x))
}

# Rhat / ESS for an arbitrary derived quantity: rebuild the chain structure
# from the .chain / .iteration columns of the source draws_df.
derived_diag <- function(x, dr) {
  d <- data.frame(value = x, .chain = dr$.chain,
                  .iteration = dr$.iteration, .draw = dr$.draw)
  s <- posterior::summarise_draws(posterior::as_draws_df(d), "rhat", "ess_bulk", "ess_tail")
  data.frame(rhat = s$rhat, ess_bulk = s$ess_bulk, ess_tail = s$ess_tail)
}

# ---- the three-way decision rule of Kruschke (2018) ----------------------
# reject   : the 95% interval lies wholly outside the ROPE, or less than
#            2.5% of the posterior mass is inside it
# accept   : the 95% interval lies wholly inside the ROPE, or more than
#            95% of the posterior mass is inside it
# undecided: neither
rope_verdict <- function(lo, hi, pct, half) {
  ifelse((lo >  half | hi < -half) | pct <  2.5, "reject the null value",
  ifelse((lo > -half & hi <  half) | pct > 95.0, "accept the null value",
         "undecided"))
}

# ---- residual variance on the link scale, for the ICC --------------------
# gaussian and lognormal: sigma^2 on the (log) scale.
# negative binomial: the log-scale variance of a gamma-Poisson evaluated at
#   the intercept, log(1 + 1/mu + 1/shape).
# zoib: the beta component's logit-scale variance, trigamma(mu phi) +
#   trigamma((1-mu) phi).  This ignores the 0/1 inflation entirely; see the
#   note on the zoib parameterisation in the report.
resid_var <- function(dr, r, fam) {
  b0 <- dr[[paste0("b_", r, "_Intercept")]]
  switch(fam,
    gaussian    = dr[[paste0("sigma_", r)]]^2,
    lognormal   = dr[[paste0("sigma_", r)]]^2,
    negbinomial = log(1 + 1 / exp(b0) + 1 / dr[[paste0("shape_", r)]]),
    zoib        = { mu <- plogis(b0); ph <- dr[[paste0("phi_", r)]]
                    trigamma(mu * ph) + trigamma((1 - mu) * ph) })
}

icc_draws <- function(dr, r, fam) {
  vloc <- dr[[paste0("sd_Locality__", r, "_Intercept")]]^2
  vloc / (vloc + resid_var(dr, r, fam))
}

# ==========================================================================
# 1.  coefficient table
# ==========================================================================
# Response-scale translation.  On a log link a slope is a multiplicative
# factor, reported as a percentage change per unit of the predictor; on a
# logit link it is an odds ratio for the beta component; on the identity
# link it is already in the response's own units.
to_response_scale <- function(b, fam) {
  switch(fam,
    lognormal   = (exp(b) - 1) * 100,
    negbinomial = (exp(b) - 1) * 100,
    zoib        = (exp(b) - 1) * 100,
    gaussian    = b)
}
scale_unit <- function(r) unname(resp_unit[[r]])

coef_table <- function(fit, which_preds = preds) {
  dr <- posterior::as_draws_df(fit)
  sm <- posterior::summarise_draws(dr, "rhat", "ess_bulk", "ess_tail")
  bind_rows(lapply(resps, function(r) bind_rows(lapply(which_preds, function(pp) {
    v <- paste0("b_", r, "_", pp)
    if (!v %in% names(dr)) return(NULL)
    x    <- dr[[v]]
    half <- ROPE_SD * link_sd[[r]]
    s    <- draw_summary(x)
    dg   <- sm[sm$variable == v, ]
    pin  <- 100 * mean(abs(x) < half)
    minr <- max(abs(s$CrI_lo), abs(s$CrI_hi))
    data.frame(
      Response = r, Family = resp_fam[[r]], Link = unname(link_lab[[resp_fam[[r]]]]),
      Predictor = pp, s,
      Std_median = s$Median / link_sd[[r]],
      Std_lo = s$CrI_lo / link_sd[[r]], Std_hi = s$CrI_hi / link_sd[[r]],
      ROPE_half = half,
      pct_in_ROPE = pin,
      Verdict = rope_verdict(s$CrI_lo, s$CrI_hi, pin, half),
      Min_ROPE = minr,
      Min_ROPE_resp = to_response_scale(minr, resp_fam[[r]]),
      Min_ROPE_unit = scale_unit(r),
      Resp_median = to_response_scale(s$Median, resp_fam[[r]]),
      Resp_lo = to_response_scale(s$CrI_lo, resp_fam[[r]]),
      Resp_hi = to_response_scale(s$CrI_hi, resp_fam[[r]]),
      Dir_certainty = max(mean(x > 0), mean(x < 0)),
      P_positive = mean(x > 0),
      rhat = dg$rhat, ess_bulk = dg$ess_bulk, ess_tail = dg$ess_tail,
      row.names = NULL)
  }))))
}

# ==========================================================================
# 2.  ICC table, with diagnostics for the derived quantity (BARG 2.B / 2.C)
# ==========================================================================
icc_table <- function(fit) {
  dr <- posterior::as_draws_df(fit)
  bind_rows(lapply(resps, function(r) {
    icc  <- icc_draws(dr, r, resp_fam[[r]])
    sdl  <- dr[[paste0("sd_Locality__", r, "_Intercept")]]
    s    <- draw_summary(icc); d <- derived_diag(icc, dr)
    ss   <- draw_summary(sdl)
    data.frame(Response = r, Family = resp_fam[[r]], s, d,
               P_gt_rope = mean(icc > ICC_ROPE),
               Verdict = ifelse(s$CrI_lo > ICC_ROPE, "locality variation present",
                         ifelse(s$CrI_hi < ICC_ROPE, "locality variation negligible",
                                "undecided")),
               sd_median = ss$Median, sd_lo = ss$CrI_lo, sd_hi = ss$CrI_hi,
               row.names = NULL)
  }))
}

# ==========================================================================
# 3.  the 21 locality-intercept correlations (BARG 1.B / 3.B)
# ==========================================================================
cor_table <- function(fit) {
  dr <- posterior::as_draws_df(fit)
  sm <- posterior::summarise_draws(dr, "rhat", "ess_bulk", "ess_tail")
  vs <- grep("^cor_Locality__", names(dr), value = TRUE)
  bind_rows(lapply(vs, function(v) {
    parts <- strsplit(sub("^cor_Locality__", "", v), "_Intercept__")[[1]]
    a <- parts[1]; b <- sub("_Intercept$", "", parts[2])
    x <- dr[[v]]; s <- draw_summary(x); dg <- sm[sm$variable == v, ]
    data.frame(A = a, B = b, s, P_positive = mean(x > 0),
               rhat = dg$rhat, ess_bulk = dg$ess_bulk, ess_tail = dg$ess_tail,
               row.names = NULL)
  }))
}

# ==========================================================================
# 4.  the ROPE curve: posterior mass inside a ROPE, as its half-width varies
# ==========================================================================
rope_curve <- function(fit, grid = seq(0, 0.6, by = 0.005)) {
  dr <- posterior::as_draws_df(fit)
  bind_rows(lapply(resps, function(r) bind_rows(lapply(preds, function(pp) {
    v <- paste0("b_", r, "_", pp); if (!v %in% names(dr)) return(NULL)
    z <- abs(dr[[v]]) / link_sd[[r]]         # standardised, so one grid serves all
    data.frame(Response = r, Predictor = pp, half = grid,
               pct = 100 * vapply(grid, function(g) mean(z < g), numeric(1)),
               row.names = NULL)
  }))))
}

# ==========================================================================
# 5.  prior / posterior width, the diagnostic behind the prior correction
# ==========================================================================
# prior_sd is taken from the sample_prior = "only" fit where one is supplied,
# and otherwise from the analytic SD of the normal slope prior.
prior_post_sd <- function(fit, prior_fit = NULL, b_sd = link_sd) {
  dr <- posterior::as_draws_df(fit)
  pr <- if (!is.null(prior_fit)) posterior::as_draws_df(prior_fit) else NULL
  bind_rows(lapply(resps, function(r) bind_rows(lapply(preds, function(pp) {
    v <- paste0("b_", r, "_", pp); if (!v %in% names(dr)) return(NULL)
    psd <- if (!is.null(pr) && v %in% names(pr)) sd(pr[[v]]) else unname(b_sd[[r]])
    data.frame(Response = r, Predictor = pp,
               prior_sd = psd, post_sd = sd(dr[[v]]),
               ratio = sd(dr[[v]]) / psd, row.names = NULL)
  }))))
}

# analytic share of a normal(0, s) slope prior that falls inside its own ROPE
prior_in_rope <- function(s) 100 * (2 * pnorm(ROPE_SD * link_sd / s) - 1)

# ==========================================================================
# 6.  posterior-predictive test statistics (BARG 3.A)
# ==========================================================================
ppc_stat <- function(fit, resp, stat, nd = 1000, label = "") {
  y    <- fit$data[[resp]]
  yrep <- brms::posterior_predict(fit, resp = resp, ndraws = nd)
  if (is.list(yrep)) yrep <- yrep[[resp]]
  obs  <- stat(y); rep <- apply(yrep, 1, stat)
  data.frame(Response = resp, Statistic = label, observed = obs,
             rep_median = median(rep),
             rep_lo = unname(quantile(rep, 0.025)),
             rep_hi = unname(quantile(rep, 0.975)),
             p_upper = mean(rep >= obs), row.names = NULL)
}

# ==========================================================================
# 7.  the basin x gradient extension (ref_basinx)
# ==========================================================================
# The reference model fits one height slope and one distance slope shared by
# both basins; ref_basinx fits one of each per basin.  Binchuan is the
# reference level of the factor, so the plain `zHeight` coefficient IS the
# Binchuan slope and the interaction is the Huangping-minus-Binchuan
# difference.  The Huangping slope is their sum, formed draw by draw so that
# it carries the covariance of the two rather than adding their intervals.
#
# Everything is divided by link_sd, the same standardised axis the reference
# slopes are reported on, so the ROPE is the same +/- ROPE_SD band.
basin_slopes <- function(fit) {
  dr <- posterior::as_draws_df(fit)
  one <- function(x, r, g, what) {
    z <- x / link_sd[[r]]; q <- eti(z); p <- 100 * mean(abs(z) < ROPE_SD)
    data.frame(Response = r, Gradient = g, Quantity = what,
               Median = median(z), CrI_lo = q[1], CrI_hi = q[2],
               pct_in_ROPE = p, P_positive = mean(z > 0),
               excl_zero = q[1] > 0 | q[2] < 0,
               Verdict = rope_verdict(q[1], q[2], p, ROPE_SD), row.names = NULL)
  }
  bind_rows(lapply(resps, function(r) bind_rows(lapply(
    c("zHeight", "zDistance"), function(g) {
      b_bin <- dr[[paste0("b_", r, "_", g)]]
      b_dif <- dr[[paste0("b_", r, "_BasinHuangping:", g)]]
      bind_rows(one(b_bin,         r, g, "Binchuan"),
                one(b_bin + b_dif, r, g, "Huangping"),
                one(b_dif,         r, g, "difference"))
    }))))
}

# P(every Huangping slope on one gradient is positive), computed jointly.
# Fourteen medians that all lean one way look like strong evidence; the joint
# probability is the honest version of that reading, and it is much smaller.
basin_joint_positive <- function(fit, gradient = "zHeight") {
  dr <- posterior::as_draws_df(fit)
  M <- vapply(resps, function(r)
    (dr[[paste0("b_", r, "_", gradient)]] +
     dr[[paste0("b_", r, "_BasinHuangping:", gradient)]]) / link_sd[[r]],
    numeric(nrow(dr)))
  mean(apply(M, 1, function(z) all(z > 0)))
}
