# ==========================================================================
# barg_sbc.R -- posterior simulation-based calibration for the reference fit.
#
# Convergence diagnostics establish that the sampler explored the posterior of
# the model that was coded.  They cannot establish that the posterior it
# returns is calibrated, and the two defects this analysis found in itself
# (s1's unscaled slope prior, s6's coi estimating an outcome that cannot
# occur) were both invisible to R-hat, ESS and the divergence count.  This
# file supplies the check that speaks to calibration instead.
#
# Why POSTERIOR SBC rather than the classical kind.  Classical SBC draws its
# parameters from the prior.  The intercept priors here are deliberately wide
# -- 6 to 100 mm of thickness, 26 to 104 degrees of edge angle before the data
# are seen -- so datasets simulated from them would include assemblages no
# lithic analyst would recognise, and calibration verified over that range
# says little about the narrow region the posterior actually occupies.
# Sailynoja, Schmitt, Burkner and Vehtari (2026) condition the simulation on
# the observed data instead.  That checks calibration where the answers live.
#
# HOW THE CONSTRUCTION HAS TO BE SET UP, because getting it wrong is easy and
# silent.  SBC is uniform only when the distribution the parameters are drawn
# from is the same distribution the refit uses as its prior.  Posterior SBC
# draws theta from p(theta | y_obs), so p(theta | y_obs) is the prior of the
# check, and the posterior the rank is taken in must therefore be
#
#     p(theta | y_obs, y_sim)  proportional to
#         p(theta) p(y_obs | theta) p(y_sim | theta)
#       = p(theta | y_obs) . p(y_sim | theta)
#
# In other words each refit conditions on the observed data AND the simulated
# data: the two frames are stacked and the reference prior is left untouched.
#
# Refitting to the simulated data ALONE under the reference prior is neither
# check.  Theta then comes from the concentrated posterior while the refit's
# prior is the wide original, the two do not match, and the ranks are not
# uniform even for a perfectly calibrated sampler.  A first run here did
# exactly that and produced 13 of the 28 quantities outside a 95% band, with
# the rank SD at 0.278 against the uniform 0.289 and several quantities'
# mean rank displaced from 0.5 -- the signature of the mismatch, not of the
# model.  The stacking below is what makes the check valid.
#
# What is checked.  The 28 quantities the two research questions rest on: the
# 21 landscape slopes (Q2) and the seven locality ICCs (Q1).  Nuisance
# parameters are not checked -- no research question is about them, and
# Modrak, Moon, Kim, et al. (2025) show that the choice of test quantity is
# what governs an SBC check's sensitivity, so the quantities the report
# actually reports are the ones worth spending refits on.
#
# What it cannot detect.  SBC checks the model as coded against itself.  Two
# known defects are therefore outside its reach by construction: GMsize
# contains Thickness in the real data while the coded model gives the seven
# responses independent residuals, and the simulated datasets inherit that
# independence; and the discreteness of retouch generations is a property of
# the measurement, not of the coded likelihood.  Both are stated in the
# report's limits section, and neither is a calibration failure.
#
# Nothing here reads or writes a cache.  _targets.R calls barg_sbc_batch()
# once per batch of simulations and stores the ranks; the batches exist so
# that an interrupted run resumes at the last completed batch rather than at
# the beginning, and so that the compiled Stan program is amortised over the
# simulations inside a batch.
# ==========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(posterior)
})

# ---- how much sampling one SBC refit needs -------------------------------
# The reference fit runs 4 chains of 10,000 for 20,000 post-warmup draws
# because the report quotes three-decimal quantiles of its posterior and the
# Monte Carlo standard error has to support them (@sec-digits).  A rank needs
# nothing like that.  The rank of the drawn value among L thinned draws is an
# integer in 0..L, and L = 200 resolves the ECDF of N ranks far finer than N
# itself does.  4 chains of 2,000 give 4,000 post-warmup draws, thinned to
# 200, which is why an SBC refit costs about a fifth of a reference fit.
BARG_SBC_CHAINS <- 4L
BARG_SBC_ITER   <- 2000L
BARG_SBC_L      <- 200L

# ---- the quantities under test -------------------------------------------
# One row per quantity, in the order the report's figures use.  ICC is placed
# last per response so that the figure reads as three slope columns and one
# ICC column.
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
# The slopes are read straight off the draw.  The ICCs are derived from it by
# the same icc_draws() the report uses, so a coding error in that derivation
# is inside what the check can catch rather than outside it.  A one-row
# draws_df behaves like the full one, so icc_draws() needs no special case.
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
# The design is held fixed: the same 165 specimens in the same 26 localities
# with the same basin, height and distance, and only the seven responses
# replaced.  That is the point of the exercise -- the question is whether the
# model recovers its own parameters from data this design could have produced,
# not whether a different design would resolve them better.
#
# posterior_predict() at a single draw uses that draw's locality intercepts
# rather than resampling them, which is what conditioning on theta requires:
# the drawn parameter vector includes the group-level effects, and the
# simulated data have to come from the same vector whose rank is then checked.
# The negative binomial response is cast back to integer because brms returns
# a numeric matrix and the family requires counts.
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
# The observed rows and the simulated rows stacked, which is how
# p(theta | y_obs, y_sim) is obtained from a sampler that only knows how to
# apply the reference prior (see the header).  Both halves carry the same
# localities and the same basin, height and distance, so the 26 locality
# intercepts are shared across the 330 rows exactly as the construction
# requires: y_obs and y_sim are conditionally independent given theta, and
# theta includes those intercepts.
barg_sbc_augment <- function(ctx, fit, sim) rbind(fit$data, sim)

# ---- the rank of the drawn value within the refit posterior ---------------
# Thinned to L draws first, so that the rank is an integer in 0..L whatever
# the refit's iteration count, and so that neighbouring draws do not inflate
# the resolution beyond what the sampler independently supports.
barg_sbc_rank <- function(post, truth, L = BARG_SBC_L) {
  idx <- round(seq(1, length(post), length.out = L))
  sum(post[idx] < truth)
}

# ---- refitting a simulated dataset ---------------------------------------
# The compiled Stan program of the reference fit cannot be reused.  That fit
# was produced under cmdstanr 0.9.0.9002 and the CmdStanModel object stored
# inside it calls an internal function the 0.9.0 release removed, so
# update(recompile = FALSE) on it fails outright.  The first refit of a batch
# is therefore a fresh brm() that compiles the same brms-generated Stan
# program under the cmdstanr in use, and the rest of the batch update()s that
# in-session fit, which does reuse its compiled program.  One compilation per
# batch, not one per simulation.
#
# The specification is rebuilt from barg_spec(ctx, "ref"), the same call
# _targets.R makes for the reference fit, so the formula, the four families
# and the reference prior are the ones the report estimates from rather than a
# reconstruction of them.
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

  # The refit's own convergence is recorded rather than acted on.  A rank from
  # a refit that did not converge is not evidence about calibration, so the
  # report needs to be able to say how many such refits there were; dropping
  # them silently would make a calibration failure look like a clean result.
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
# Spread evenly over the reference posterior rather than taken from its head,
# so that the N parameter vectors are as close to independent as the chain
# allows and between them cover the region the posterior occupies.
barg_sbc_draw_ids <- function(ndraws, n_sim) round(seq(1, ndraws, length.out = n_sim))

# ---- one batch -----------------------------------------------------------
# Simulations are dealt round-robin across the batches rather than in blocks,
# so that a batch that fails or is still running has not taken a contiguous
# stretch of the reference posterior with it: whatever completes is still
# spread over the whole posterior and the partial result is interpretable.
#
# The environment is recorded on the returned frame because these refits do
# not run under the environment the eleven reference fits ran under -- the
# stored compiled model forces a recompilation here (see above) -- and the
# report has to be able to say so rather than imply one uniform run.
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

# ==========================================================================
# The uniformity check
# ==========================================================================
# Under a calibrated posterior the N ranks of a quantity are uniform on
# 0..L, so the ECDF of the fractional ranks should follow the diagonal and
# the difference from it should be flat at zero.  The band below is
# SIMULTANEOUS over the whole ECDF, not pointwise: a pointwise band is
# crossed somewhere with probability far above its nominal level and invites
# exactly the over-reading this report avoids elsewhere.
#
# It is obtained by simulation rather than from the Kolmogorov distribution
# because the ranks are discrete.  With L = 200 and N in the low hundreds the
# discreteness is not negligible, and simulating uniform ranks on the same
# grid gives the exact null distribution of the supremum at no meaningful
# cost.
# The supremum itself is taken from the order statistics rather than off a
# grid: for sorted u the largest gap between the ECDF and the diagonal can
# only occur at a data point, so this is the exact supremum and not an
# approximation to it, and it costs one sort instead of a pass over a grid.
# The figure still draws the difference on a grid, because a curve needs one.
barg_sbc_sup <- function(u) {
  u <- sort(u); n <- length(u); i <- seq_len(n)
  max(max(i / n - u), max(u - (i - 1) / n))
}

# The null ranks are put through exactly the transform the observed ranks get,
# (rank + 0.5) / (L + 1) with rank in 0..L.  Simulating uniforms on any other
# grid -- k / (L + 1), say -- offsets the null ECDF from the observed one by
# half a step and leaves the band about 2.5% too narrow at L = 200, which shows
# up as calibrated quantities crossing it slightly too often.
barg_sbc_band <- function(n, L = BARG_SBC_L, alpha = 0.05, nsim = 20000L,
                          seed = 2226L) {
  set.seed(seed)
  sup <- replicate(nsim, barg_sbc_sup(
    (sample.int(L + 1L, n, replace = TRUE) - 1L + 0.5) / (L + 1)))
  unname(stats::quantile(sup, 1 - alpha))
}

# ---- per-quantity uniformity ---------------------------------------------
# Two summaries, because they fail in different ways.  The supremum of the
# ECDF difference catches a systematic shift or a squeeze -- the posterior
# sitting too low, or being too narrow -- and is the statistic the band is
# built for.  The chi-square on binned ranks catches a pile-up at the extremes
# that leaves the middle of the ECDF alone, which is what a posterior with the
# right location and the wrong tails produces.
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

# ==========================================================================
# The figure
# ==========================================================================
# A 7 x 4 grid: three slope columns and one ICC column, one row per response,
# so that the 28 quantities under test appear in the same layout as
# @fig-main, whose panel A is the ICC column and whose panel B is the three
# slope columns.  Each panel is the ECDF difference with its simultaneous
# band; a curve inside the band is a quantity whose posterior is calibrated
# as far as N simulations can tell.
#
# The drawing constants come off ctx rather than out of this file's own
# environment.  fig_theme, save_fig and the palette are defined in
# barg_theme.R, which barg_context.R sources into a private environment; a
# function defined here at the top level would not see them.  Taking them from
# ctx is the same route barg_sbc_one() takes to icc_draws(), and it keeps the
# expensive rank targets independent of the report context, so that editing a
# figure cannot invalidate a day of refits.
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

  # The y range is set from the band rather than from the curves.  When every
  # curve sits well inside the band the ribbon would otherwise fill the panel
  # to its edges and read as a background tint, leaving the reader unable to
  # see where the threshold is; widening the axis past the band and drawing its
  # edge explicitly keeps "inside the band" a visible statement rather than an
  # assertion in the caption.
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
    # Axis labels avoid "rank" and "ECDF": this figure is read by archaeologists
    # rather than by statisticians, and the report's own text describes the same
    # quantity as the position of the generating value within its refitted
    # posterior, and the departure of those positions from an even spread.
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
