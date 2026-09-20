# Posterior simulation-based calibration for the reference fit: the check the
# convergence diagnostics cannot supply, on the 28 quantities the two research
# questions rest on (21 landscape slopes, 7 locality ICCs).
#
# Posterior rather than classical SBC, after Sailynoja, Schmitt, Burkner and
# Vehtari (2026): the intercept priors are deliberately wide, so datasets
# simulated from them would include assemblages no lithic analyst would
# recognise, and calibration verified over that range says little about the
# region the posterior occupies.
#
# The construction has to be set up this way, and getting it wrong is silent.
# SBC is uniform only when the distribution theta is drawn from is the one the
# refit uses as its prior.  Posterior SBC draws theta from p(theta | y_obs),
# so the rank must be taken in
#
#     p(theta | y_obs, y_sim)  proportional to
#         p(theta | y_obs) . p(y_sim | theta)
#
# -- each refit conditions on the observed data AND the simulated data, the
# two frames stacked, the reference prior untouched.  Refitting to the
# simulated data alone is neither check: theta then comes from the
# concentrated posterior while the refit's prior is the wide original, and the
# ranks are not uniform even for a perfectly calibrated sampler.
suppressPackageStartupMessages({
  library(dplyr); library(posterior)
})

# ---- how much sampling one SBC refit needs -------------------------------
# Far less than the reference fit, whose 20,000 draws support the report's
# three-decimal quantiles: a rank among L thinned draws is an integer in 0..L,
# and L = 200 resolves the ECDF of N ranks far finer than N itself does.
BARG_SBC_CHAINS <- 4L
BARG_SBC_ITER   <- 2000L
BARG_SBC_L      <- 200L

# ---- the quantities under test -------------------------------------------
# One row per quantity, in the order the report's figures use.
barg_sbc_quantities <- function(ctx) {
  slopes <- expand.grid(Response = ctx$resps, Predictor = ctx$preds,
                        stringsAsFactors = FALSE)
  bind_rows(
    data.frame(kind = "slope", slopes,
               quantity = paste0("b_", slopes$Response, "_", slopes$Predictor),
               row.names = NULL),
    data.frame(kind = "icc", Response = ctx$resps, Predictor = NA_character_,
               quantity = paste0("icc_", ctx$resps), row.names = NULL))
}

# ---- the drawn value of every quantity, at one posterior draw -------------
# The ICCs are derived by the same icc_draws() the report uses, so a coding
# error in that derivation is inside what the check can catch.
barg_sbc_truth <- function(ctx, dr, draw_id) {
  d1 <- dr[dr$.draw == draw_id, , drop = FALSE]
  q  <- barg_sbc_quantities(ctx)
  vapply(seq_len(nrow(q)), function(k) {
    if (q$kind[k] == "slope")
      as.numeric(d1[[paste0("b_", q$Response[k], "_", q$Predictor[k])]])
    else
      as.numeric(ctx$icc_draws(d1, q$Response[k], ctx$resp_fam[[q$Response[k]]]))
  }, numeric(1))
}

# ---- the same quantities, as a posterior from a refit ---------------------
barg_sbc_posterior <- function(ctx, fit) {
  dr <- posterior::as_draws_df(fit)
  q  <- barg_sbc_quantities(ctx)
  lapply(seq_len(nrow(q)), function(k) {
    if (q$kind[k] == "slope")
      as.numeric(dr[[paste0("b_", q$Response[k], "_", q$Predictor[k])]])
    else
      as.numeric(ctx$icc_draws(dr, q$Response[k], ctx$resp_fam[[q$Response[k]]]))
  })
}

# ---- one simulated dataset ------------------------------------------------
# The design is held fixed and only the seven responses are replaced: the
# question is whether the model recovers its own parameters from data this
# design could have produced.  posterior_predict() at a single draw uses that
# draw's locality intercepts rather than resampling them, which is what
# conditioning on theta requires.  The negative binomial response is cast back
# to integer because brms returns a numeric matrix.
barg_sbc_simulate <- function(ctx, fit, draw_id) {
  new <- fit$data
  for (r in ctx$resps) {
    y <- as.numeric(brms::posterior_predict(fit, resp = r,
                                            draw_ids = draw_id)[1L, ])
    if (identical(ctx$resp_fam[[r]], "negbinomial")) y <- as.integer(round(y))
    new[[r]] <- y
  }
  new
}

# ---- the frame each refit actually sees ----------------------------------
# Observed rows and simulated rows stacked: how p(theta | y_obs, y_sim) is
# obtained from a sampler that only knows how to apply the reference prior.
# Both halves carry the same localities, so the 26 intercepts are shared
# across the 330 rows, as the construction requires.
barg_sbc_augment <- function(ctx, fit, sim) rbind(fit$data, sim)

# ---- the rank of the drawn value within the refit posterior ---------------
# Thinned to L draws first, so that the rank is an integer in 0..L whatever
# the refit's iteration count.
barg_sbc_rank <- function(post, truth, L = BARG_SBC_L) {
  idx <- round(seq(1, length(post), length.out = L))
  sum(post[idx] < truth)
}

# ---- refitting a simulated dataset ---------------------------------------
# The compiled Stan program stored in the reference fit cannot be reused: it
# calls an internal function the current cmdstanr removed, so
# update(recompile = FALSE) on it fails.  The first refit of a batch is a
# fresh brm() and the rest update() that in-session fit, which does reuse its
# program -- one compilation per batch, not one per simulation.  The
# specification is rebuilt from barg_spec(ctx, "ref"), the same call
# _targets.R makes, so it is the reference model rather than a reconstruction.
barg_sbc_fit_fresh <- function(ctx, dat, seed, chains, iter, cores) {
  s <- barg_spec(ctx, "ref")
  brms::brm(barg_formula(s$rhs, s$fams), data = dat, prior = s$prior,
            sample_prior = s$sample_prior,
            chains = chains, iter = iter, cores = cores, seed = seed,
            backend = BARG_BACKEND, refresh = 0, silent = 2)
}

# ---- one simulation ------------------------------------------------------
# Returns the ranks and the fit.  The batch keeps the first fit as the base
# for every later update() and discards the others, so at most two refits are
# in memory at once.
barg_sbc_one <- function(ctx, fit_ref, dr_ref, draw_id, seed, base = NULL,
                         chains = BARG_SBC_CHAINS, iter = BARG_SBC_ITER,
                         cores = chains, L = BARG_SBC_L) {
  q     <- barg_sbc_quantities(ctx)
  truth <- barg_sbc_truth(ctx, dr_ref, draw_id)
  sim   <- barg_sbc_simulate(ctx, fit_ref, draw_id)
  aug   <- barg_sbc_augment(ctx, fit_ref, sim)

  t0  <- Sys.time()
  ref <- if (is.null(base))
    barg_sbc_fit_fresh(ctx, aug, seed, chains, iter, cores)
  else
    stats::update(base, newdata = aug, chains = chains, iter = iter,
                  cores = cores, seed = seed, recompile = FALSE,
                  refresh = 0, silent = 2)
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

  post <- barg_sbc_posterior(ctx, ref)

  # The refit's own convergence is recorded rather than acted on: dropping bad
  # refits silently would make a calibration failure look like a clean result.
  s  <- posterior::summarise_draws(posterior::as_draws_df(ref), "rhat", "ess_bulk")
  s  <- s[!is.na(s$rhat) & !startsWith(s$variable, "lp"), ]
  np <- brms::nuts_params(ref)
  dv <- sum(np$Value[np$Parameter == "divergent__"])

  ranks <- data.frame(
    sim = draw_id, q[, c("kind", "Response", "Predictor", "quantity")],
    truth = truth,
    rank = vapply(seq_along(post), function(k)
      barg_sbc_rank(post[[k]], truth[k], L), numeric(1)),
    L = L,
    post_median = vapply(post, stats::median, numeric(1)),
    max_rhat = max(s$rhat), min_ess = min(s$ess_bulk), divergent = dv,
    minutes = mins, row.names = NULL)

  list(ranks = ranks, fit = ref)
}

# ---- which posterior draws to use as ground truth ------------------------
# Spread evenly over the reference posterior rather than taken from its head.
barg_sbc_draw_ids <- function(ndraws, n_sim) round(seq(1, ndraws, length.out = n_sim))

# ---- one batch -----------------------------------------------------------
# Simulations are dealt round-robin rather than in blocks, so that whatever
# completes is still spread over the whole reference posterior.  The
# environment is recorded on the returned frame because the recompilation
# above means these refits need not match the eleven reference fits.
barg_sbc_batch <- function(ctx, fit_ref, batch, n_sim, n_batch,
                           chains = BARG_SBC_CHAINS, iter = BARG_SBC_ITER,
                           cores = chains, L = BARG_SBC_L) {
  dr_ref <- posterior::as_draws_df(fit_ref)
  ids    <- barg_sbc_draw_ids(posterior::ndraws(dr_ref), n_sim)
  mine   <- split(seq_along(ids), rep(seq_len(n_batch), length.out = length(ids)))[[batch]]

  base <- NULL
  out  <- vector("list", length(mine))
  for (j in seq_along(mine)) {
    k   <- mine[j]
    one <- barg_sbc_one(ctx, fit_ref, dr_ref, ids[k],
                        seed = ctx$SEED + 100000L + k, base = base,
                        chains = chains, iter = iter, cores = cores, L = L)
    out[[j]] <- one$ranks
    # the first refit of the batch becomes the compiled base for the rest;
    # every later one is dropped as soon as its ranks are out of it
    if (is.null(base)) base <- one$fit
    one <- NULL
    gc()
  }
  res <- bind_rows(out)
  res$batch    <- batch
  res$chains   <- chains
  res$iter     <- iter
  res$cmdstanr <- as.character(utils::packageVersion("cmdstanr"))
  res$cmdstan  <- cmdstanr::cmdstan_version()
  res$brms     <- as.character(utils::packageVersion("brms"))
  res
}

# ---- the uniformity check ------------------------------------------------
# Under a calibrated posterior the N ranks of a quantity are uniform on 0..L,
# so the ECDF of the fractional ranks should follow the diagonal.  The band is
# SIMULTANEOUS over the whole ECDF, not pointwise, and is obtained by
# simulation rather than from the Kolmogorov distribution because the ranks
# are discrete.  The supremum is taken from the order statistics: for sorted u
# the largest gap can only occur at a data point, so it is exact.
barg_sbc_sup <- function(u) {
  u <- sort(u); n <- length(u); i <- seq_len(n)
  max(max(i / n - u), max(u - (i - 1) / n))
}

# The null ranks go through exactly the transform the observed ranks get,
# (rank + 0.5) / (L + 1).  Simulating uniforms on any other grid offsets the
# null ECDF by half a step and leaves the band about 2.5% too narrow.
barg_sbc_band <- function(n, L = BARG_SBC_L, alpha = 0.05, nsim = 20000L,
                          seed = 2226L) {
  set.seed(seed)
  sup <- replicate(nsim, barg_sbc_sup(
    (sample.int(L + 1L, n, replace = TRUE) - 1L + 0.5) / (L + 1)))
  unname(stats::quantile(sup, 1 - alpha))
}

# ---- per-quantity uniformity ---------------------------------------------
# Two summaries, because they fail differently: the supremum of the ECDF
# difference catches a shift or a squeeze, the chi-square on binned ranks a
# pile-up at the extremes that leaves the middle of the ECDF alone.
barg_sbc_uniformity <- function(ranks, alpha = 0.05, nbin = 20L) {
  band <- barg_sbc_band(length(unique(ranks$sim)), unique(ranks$L)[1], alpha)
  ranks |>
    group_by(kind, Response, Predictor, quantity) |>
    reframe({
      u   <- (rank + 0.5) / (L + 1)
      sup <- barg_sbc_sup(u)
      o   <- table(cut(u, breaks = seq(0, 1, length.out = nbin + 1L),
                       include.lowest = TRUE))
      e   <- length(u) / nbin
      x2  <- sum((as.numeric(o) - e)^2 / e)
      data.frame(n_sim = length(u), sup = sup, band = band,
                 outside = sup > band,
                 chisq = x2, chisq_df = nbin - 1L,
                 chisq_p = stats::pchisq(x2, nbin - 1L, lower.tail = FALSE))
    }) |>
    ungroup()
}

# ---- the ECDF-difference curves the figure draws -------------------------
barg_sbc_ecdf <- function(ranks, grid = seq(0, 1, length.out = 201)) {
  ranks |>
    group_by(kind, Response, Predictor, quantity) |>
    reframe({
      u <- (rank + 0.5) / (L + 1)
      data.frame(x = grid,
                 diff = vapply(grid, function(g) mean(u <= g), numeric(1)) - grid)
    }) |>
    ungroup()
}

# ---- the figure ----------------------------------------------------------
# A 7 x 4 grid, three slope columns and one ICC column, so that the 28
# quantities appear in the same layout as the main figure.  Each panel is the
# ECDF difference with its simultaneous band.
#
# The drawing constants come off ctx: fig_theme, save_fig and the palette live
# in barg_theme.R, which is sourced into a private environment that a function
# defined here at the top level would not see.  It also keeps the expensive
# rank targets independent of the report context.
barg_sbc_figure <- function(ctx, ranks, dir = ctx$FIGDIR) {
  ec   <- barg_sbc_ecdf(ranks)
  un   <- barg_sbc_uniformity(ranks)
  band <- un$band[1]

  col_lab <- c(unname(ctx$pred_short), "Locality ICC")
  lab_of  <- function(kind, pred)
    ifelse(kind == "icc", "Locality ICC", unname(ctx$pred_short[pred]))
  add_facets <- function(d)
    dplyr::mutate(d,
      Col = factor(lab_of(kind, Predictor), levels = col_lab),
      Row = factor(unname(ctx$resp_lab[Response]), levels = unname(ctx$resp_lab)))

  ec <- add_facets(ec)
  un <- add_facets(un)
  bad <- dplyr::semi_join(ec, dplyr::filter(un, outside), by = c("Row", "Col"))

  # The y range is set from the band, not from the curves: otherwise the
  # ribbon fills the panel to its edges and reads as a background tint.
  ylim <- band * 1.25
  p <- ggplot2::ggplot(ec, ggplot2::aes(x, diff)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -band, ymax = band),
                         fill = "#EDEFF2", colour = NA) +
    ggplot2::geom_hline(yintercept = c(-band, band), colour = "#B9BFC7",
                        linewidth = 0.3, linetype = "22") +
    ggplot2::geom_hline(yintercept = 0, colour = ctx$GREY, linewidth = 0.25) +
    ggplot2::geom_step(linewidth = 0.4, colour = ctx$SLATE) +
    ggplot2::geom_step(data = bad, linewidth = 0.5, colour = ctx$POS) +
    ggplot2::facet_grid(Row ~ Col, switch = "y") +
    ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1)) +
    ggplot2::coord_cartesian(ylim = c(-ylim, ylim)) +
    # Axis labels avoid "rank" and "ECDF": the readership is archaeological,
    # and the report's own text describes the quantity this way.
    ggplot2::labs(x = "Position of the generating value within its refitted posterior",
                  y = "Departure from an even spread") +
    ctx$fig_theme +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 7.5),
      strip.text.x = ggplot2::element_text(size = 7.5),
      axis.text = ggplot2::element_text(size = 6.5),
      panel.spacing = ggplot2::unit(3, "pt"))

  ctx$save_fig("sbc_ecdf.png", p, 8.5, 7.4, dir = dir)
}
