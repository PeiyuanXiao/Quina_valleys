# ==========================================================================
# barg_figures.R -- every figure in paper/barg/barg_report.qmd.
#
# Sourced into the report context by barg_context.R, so that resp_lab,
# fig_theme, PAL and the rest of the drawing constants resolve without being
# passed in.  Nothing here reads a cache or fits anything: the fits arrive as
# arguments from _targets.R, and each entry point returns the paths it wrote
# so that the figure target can declare them with format = "file".
#
#   barg_ppc_stats()  the targeted posterior predictive statistics (BARG 3.A),
#                     a table the report prints and this file also plots
#   barg_figures()    every PNG, written to paper/barg/figures/ at 300 dpi
#
# The main figure is the two-panel display the manuscript carries as its
# Figure 9. It is defined in barg_main_figure.R, which the manuscript's own
# context loads too, so the PNG written here and the figure the manuscript
# draws for itself come from one definition. Everything else here is
# supplementary.
# ==========================================================================
suppressPackageStartupMessages({
  library(brms); library(posterior); library(dplyr)
  library(tidyr); library(ggplot2); library(patchwork); library(bayesplot)
})

SENS <- c(REF = "ref", S1 = "s1", S2 = "s2", S3 = "s3", S4 = "s4", S5 = "s5",
          S6 = "s6")

resp_f <- function(x) factor(unname(resp_lab[x]), levels = unname(resp_lab))
pred_f <- function(x) factor(unname(pred_short[x]), levels = unname(pred_short))

# ---- the targeted statistics of BARG 3.A ---------------------------------
stat_defs <- list(
  list(r = "RG",        f = function(y) mean(y < 0),  lab = "share below zero"),
  list(r = "RG",        f = function(y) min(y),       lab = "minimum"),
  list(r = "RG",        f = function(y) sd(y),        lab = "SD"),
  list(r = "GIUR",      f = function(y) mean(y == 1), lab = "share exactly 1"),
  list(r = "GIUR",      f = function(y) mean(y == 0), lab = "share exactly 0"),
  list(r = "RLI",       f = function(y) mean(y == 1), lab = "share exactly 1"),
  list(r = "RLI",       f = function(y) mean(y == 0), lab = "share exactly 0"),
  list(r = "NScar",     f = function(y) var(y) / mean(y), lab = "variance / mean"),
  list(r = "NScar",     f = function(y) max(y),       lab = "maximum"),
  list(r = "Thickness", f = function(y) max(y),       lab = "maximum"),
  list(r = "GMsize",    f = function(y) max(y),       lab = "maximum"),
  list(r = "EdgeAngle", f = function(y) min(y),       lab = "minimum"))

barg_ppc_stats <- function(fit_ref, nd = 1000) {
  bind_rows(lapply(stat_defs, function(s)
    ppc_stat(fit_ref, s$r, s$f, nd = nd, label = s$lab)))
}

# ---- sensitivity draws, one fit at a time --------------------------------
# The overlaid ECDFs compare seven fits. Reducing each to the quantities the
# figures need in its own target keeps exactly one large brmsfit in memory at
# a time -- eleven full fits do not fit in 16 GB together -- and caches the
# reduction, so redrawing the panels never reopens a fit.
#
# Draws are thinned for the ECDF: a step function of 20000 points is
# indistinguishable from one of 4000 and the figure files are far smaller.
barg_sens_one <- function(fit, spec, thin = 5) {
  key_slopes <- bind_rows(
    expand.grid(Response = "EdgeAngle", Predictor = preds, stringsAsFactors = FALSE),
    expand.grid(Response = resps, Predictor = "zHeight", stringsAsFactors = FALSE)) |>
    distinct()

  d    <- posterior::as_draws_df(fit)
  keep <- seq(1, nrow(d), by = thin)

  icc <- bind_rows(lapply(resps, function(r)
    data.frame(Spec = spec, kind = "icc", Response = r,
               Predictor = NA_character_,
               x = icc_draws(d, r, resp_fam[[r]])[keep])))

  sl <- bind_rows(lapply(seq_len(nrow(key_slopes)), function(i) {
    r <- key_slopes$Response[i]; pp <- key_slopes$Predictor[i]
    data.frame(Spec = spec, kind = "slope", Response = r, Predictor = pp,
               x = d[[paste0("b_", r, "_", pp)]][keep] / link_sd[[r]])
  }))

  rm(d); gc()
  bind_rows(icc, sl)
}

# ==========================================================================
# barg_figures(fits, ppc_stat_tbl, sens_long, ...)
#
# fits          named list holding ref, prior_ref and noland -- and only those
#               three, so that this target never holds more than three large
#               brmsfit objects at once
# ppc_stat_tbl  the table barg_ppc_stats() returns
# sens_long     the seven reductions barg_sens_one() returns, stacked
# ndraws        overlay draws in the predictive checks
# stat_ndraws   predictive draws behind the grouped-statistic checks
# ==========================================================================
barg_figures <- function(fits, ppc_stat_tbl, sens_long, ndraws = 100,
                         stat_ndraws = 1000, dir = FIGDIR) {

  fit_ref    <- fits[["ref"]]
  fit_prior  <- fits[["prior_ref"]]
  fit_noland <- fits[["noland"]]
  out <- character(0)
  keep_fig <- function(p) { out <<- c(out, p); invisible(p) }

  bayesplot::color_scheme_set(c(rep("#BFC4CB", 3), rep(INK, 3)))

  # ========================================================================
  # MAIN FIGURE
  # ========================================================================
  keep_fig(save_fig("barg_main.png",
                    barg_main_figure(fit_ref, fit_prior, fit_noland),
                    BARG_MAIN_SIZE[["width"]], BARG_MAIN_SIZE[["height"]],
                    dir = dir))

  # ========================================================================
  # PREDICTIVE CHECKS
  # ========================================================================
  # Display windows for the prior predictive check: the range a lithic analyst
  # would call physically possible, not the range of the sample.
  prior_window <- list(Thickness = c(0, 150), GMsize = c(0, 200), GIUR = c(0, 1),
                       RLI = c(0, 1), NScar = c(0, 200), EdgeAngle = c(0, 180),
                       RG = c(-2, 10))

  ppc_panel <- function(fit, r, window = NULL, nd = ndraws,
                        type = "dens_overlay") {
    p <- brms::pp_check(fit, resp = r, ndraws = nd, type = type) +
      labs(title = sprintf("%s  (%s)", resp_lab[[r]], fam_lab[[resp_fam[[r]]]]),
           x = NULL, y = NULL) +
      fig_theme +
      theme(legend.position = "none", plot.title = element_text(size = 8, face = "bold"))
    if (!is.null(window)) p <- p + coord_cartesian(xlim = window)
    p
  }

  # The prior predictive draws span orders of magnitude on the log-link
  # responses and pile up on the boundaries of the two proportions, which makes
  # a density overlay unreadable and, for the proportions, misleading.  The
  # empirical cumulative distribution handles heavy tails, point masses and
  # bounded support in one display, and it is the same quantile-based view the
  # rest of the report uses.
  keep_fig(save_fig("ppc_prior.png",
           wrap_plots(lapply(resps, function(r)
                        ppc_panel(fit_prior, r, prior_window[[r]],
                                  type = "ecdf_overlay")), ncol = 3) +
             plot_annotation(caption = paste(
               "Cumulative distribution of each prior predictive dataset (grey)",
               "against the observed one (dark).",
               "Axes are clipped to the physically possible range of each measure.")) &
             theme(plot.caption = element_text(size = 7, colour = GREY, hjust = 0)),
           8.5, 6.2, dir = dir))

  keep_fig(save_fig("ppc_posterior.png",
           wrap_plots(lapply(resps, function(r) ppc_panel(fit_ref, r)), ncol = 3),
           8.5, 6.0, dir = dir))

  grouped_panel <- function(r, grp) {
    brms::pp_check(fit_ref, resp = r, type = "stat_grouped", group = grp,
                   stat = "median", ndraws = stat_ndraws) +
      labs(title = resp_lab[[r]], x = NULL, y = NULL) + fig_theme +
      theme(legend.position = "none", plot.title = element_text(size = 8, face = "bold"),
            axis.text = element_text(size = 5.5), strip.text = element_text(size = 5.5))
  }
  keep_fig(save_fig("ppc_group_basin.png",
           wrap_plots(lapply(resps, function(r) grouped_panel(r, "Basin")), ncol = 3),
           8.5, 6.0, dir = dir))
  keep_fig(save_fig("ppc_group_locality.png",
           wrap_plots(lapply(resps, function(r) grouped_panel(r, "Locality")), ncol = 3),
           9.5, 8.0, dir = dir))

  p_stats <- ppc_stat_tbl |>
    mutate(lab = paste0(unname(resp_lab[Response]), "\n", Statistic),
           lab = factor(lab, levels = rev(unique(lab))),
           inside = observed >= rep_lo & observed <= rep_hi) |>
    ggplot(aes(y = lab)) +
    geom_linerange(aes(xmin = rep_lo, xmax = rep_hi), linewidth = 0.5, colour = "#9AA0A6") +
    geom_point(aes(x = rep_median), shape = 21, size = 1.6, fill = "white", colour = INK) +
    geom_point(aes(x = observed, colour = inside), shape = 18, size = 2.6) +
    scale_colour_manual(values = c("TRUE" = "#6FB98E", "FALSE" = POS),
                        labels = c("TRUE" = "inside the 95% predictive interval",
                                   "FALSE" = "outside")) +
    facet_wrap(~ lab, scales = "free", ncol = 3, strip.position = "left") +
    labs(x = NULL, y = NULL,
         caption = paste("Diamond: the observed statistic.  Open circle and bar: the",
                         "median and 95% interval of the posterior predictive draws.")) +
    fig_theme +
    theme(axis.text.y = element_blank(), strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = 6.5, face = "plain"),
          legend.position = "bottom", plot.caption = element_text(size = 7, colour = GREY,
                                                                  hjust = 0))
  keep_fig(save_fig("ppc_stats.png", p_stats, 8.5, 4.6, dir = dir))

  # ========================================================================
  # PRIOR AGAINST POSTERIOR, for every slope
  # ========================================================================
  dr_post  <- posterior::as_draws_df(fit_ref)
  dr_prior <- posterior::as_draws_df(fit_prior)
  pp_long <- bind_rows(lapply(resps, function(r) bind_rows(lapply(preds, function(pp) {
    v <- paste0("b_", r, "_", pp)
    bind_rows(data.frame(Response = r, Predictor = pp, which = "posterior",
                         x = dr_post[[v]] / link_sd[[r]]),
              data.frame(Response = r, Predictor = pp, which = "prior",
                         x = dr_prior[[v]] / link_sd[[r]]))
  }))))

  p_pp <- pp_long |>
    mutate(Resp = resp_f(Response), Pred = pred_f(Predictor)) |>
    ggplot(aes(x, colour = which, fill = which)) +
    annotate("rect", xmin = -ROPE_SD, xmax = ROPE_SD, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.6) +
    geom_density(alpha = 0.25, linewidth = 0.35, adjust = 1.2) +
    scale_colour_manual(values = c(prior = "#9AA0A6", posterior = INK)) +
    scale_fill_manual(values = c(prior = "#C9CDD3", posterior = PAL[2])) +
    coord_cartesian(xlim = c(-3.2, 3.2)) +
    facet_grid(Resp ~ Pred, scales = "free_y", switch = "y") +
    labs(x = expression(beta * " / SD of the response on its link scale"), y = NULL) +
    fig_theme +
    theme(axis.text.y = element_blank(), legend.position = "bottom",
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = 6.5),
          strip.placement = "outside", strip.background.y = element_blank(),
          panel.spacing = unit(3, "pt"))
  keep_fig(save_fig("prior_posterior.png", p_pp, 7.5, 8.0, dir = dir))

  # ========================================================================
  # ROPE CURVES
  # ========================================================================
  rc <- rope_curve(fit_ref) |> mutate(Resp = resp_f(Response), Pred = pred_f(Predictor))
  p_rope <- ggplot(rc, aes(half, pct, colour = Resp)) +
    geom_vline(xintercept = ROPE_SD, linetype = "dashed", linewidth = 0.3, colour = GREY) +
    geom_hline(yintercept = c(2.5, 95), linetype = "dotted", linewidth = 0.3, colour = "#9AA0A6") +
    geom_line(linewidth = 0.45) +
    scale_colour_manual(values = setNames(c(PAL, "#7A8087"), unname(resp_lab))) +
    scale_x_continuous(breaks = seq(0, 0.6, 0.1)) +
    facet_wrap(~ Pred, nrow = 1) +
    labs(x = "ROPE half-width (SD of the response on its link scale)",
         y = "posterior mass inside the ROPE (%)",
         caption = paste("Dashed line: the 0.1 SD half-width used in the text.",
                         "Dotted lines: the 2.5% and 95% decision thresholds.")) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    fig_theme +
    theme(legend.position = "bottom", legend.text = element_text(size = 7),
          legend.key.width = unit(0.5, "cm"), legend.key.height = unit(0.32, "cm"),
          legend.margin = margin(t = -2), legend.spacing.y = unit(1, "pt"),
          plot.caption = element_text(size = 7, colour = GREY, hjust = 0))
  keep_fig(save_fig("rope_curves.png", p_rope, 7.5, 3.4, dir = dir))

  # ========================================================================
  # LOCALITY INTERCEPTS
  # ========================================================================
  # Localities are in one fixed order in every panel -- largest assemblage at
  # the top -- so that the panels can be read against each other and so that the
  # effect of partial pooling is visible: the further down the axis, the fewer
  # specimens, the harder the offset is shrunk and the wider its interval.
  n_by_loc <- sort(table(mod_dat$Locality), decreasing = TRUE)
  loc_lev  <- rev(names(n_by_loc))

  re <- brms::ranef(fit_ref)$Locality
  cat_df <- bind_rows(lapply(resps, function(r) {
    k <- paste0(r, "_Intercept")
    data.frame(Locality = rownames(re), Response = r,
               est = re[, "Estimate", k], lo = re[, "Q2.5", k], hi = re[, "Q97.5", k])
  })) |>
    mutate(Resp = resp_f(Response),
           Loc = factor(sprintf("%s (%d)", Locality, n_by_loc[Locality]),
                        levels = sprintf("%s (%d)", loc_lev, n_by_loc[loc_lev])))

  p_cat <- ggplot(cat_df, aes(est, Loc)) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = GREY) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.3, colour = "#9AA0A6") +
    geom_point(shape = 21, size = 1.1, fill = PAL[2], colour = INK, stroke = 0.3) +
    facet_wrap(~ Resp, scales = "free_x", nrow = 2) +
    labs(x = "locality offset on the link scale", y = NULL,
         caption = paste("Localities in one fixed order, largest assemblage first;",
                         "the specimen count is in brackets.")) +
    fig_theme + theme(axis.text.y = element_text(size = 5),
                      axis.text.x = element_text(size = 6),
                      plot.caption = element_text(size = 7, colour = GREY, hjust = 0))
  keep_fig(save_fig("locality_intercepts.png", p_cat, 9.5, 6.0, dir = dir))

  # ========================================================================
  # CORRELATION MATRIX
  # ========================================================================
  ct <- cor_table(fit_ref)
  # Only the lower triangle is drawn: the matrix is symmetric and its 21
  # estimated parameters are exactly the off-diagonal entries.  Each pair is
  # placed by the position of its two responses in the standard response order,
  # the later one on the vertical axis.
  cm <- ct |>
    mutate(row = pmax(match(A, resps), match(B, resps)),
           col = pmin(match(A, resps), match(B, resps))) |>
    transmute(X = factor(unname(resp_lab[resps[col]]),
                         levels = unname(resp_lab[resps[-length(resps)]])),
              Y = factor(unname(resp_lab[resps[row]]),
                         levels = rev(unname(resp_lab[resps[-1]]))),
              Median, CrI_lo, CrI_hi,
              decided = CrI_lo > 0 | CrI_hi < 0)

  p_cor <- ggplot(cm, aes(X, Y, fill = Median)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = formatC(Median, format = "f", digits = 2),
                  fontface = ifelse(decided, "bold", "plain")),
              size = 2.4, colour = INK) +
    scale_fill_gradient2(low = NEG, mid = MID, high = POS, midpoint = 0,
                         limits = c(-1, 1), name = "posterior\nmedian") +
    scale_x_discrete(expand = c(0, 0), drop = FALSE) +
    scale_y_discrete(expand = c(0, 0), drop = FALSE) +
    labs(x = NULL, y = NULL,
         caption = paste("Bold: the 95% equal-tailed interval excludes zero.",
                         "The matrix is symmetric; its 21 estimated off-diagonal",
                         "entries are shown once each.")) +
    fig_theme +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7), legend.position = "right",
          legend.title = element_text(size = 7.5),
          legend.key.width = unit(0.28, "cm"), legend.key.height = unit(0.9, "cm"),
          plot.caption = element_text(size = 7, colour = GREY, hjust = 0))
  keep_fig(save_fig("correlation_matrix.png", p_cor, 6.4, 4.4, dir = dir))

  # ========================================================================
  # CONVERGENCE
  # ========================================================================
  sm <- posterior::summarise_draws(dr_post, "rhat", "ess_bulk", "ess_tail")
  sm <- sm[!is.na(sm$rhat) & !startsWith(sm$variable, "lp"), ]
  icc_d <- icc_table(fit_ref)
  diag_df <- bind_rows(
    sm |> transmute(variable, rhat, ess_bulk, ess_tail,
                    Class = case_when(startsWith(variable, "b_") &
                                        grepl("Intercept$", variable) ~ "intercepts",
                                      startsWith(variable, "b_")   ~ "slopes",
                                      startsWith(variable, "sd_")  ~ "locality SD",
                                      startsWith(variable, "cor_") ~ "correlations",
                                      startsWith(variable, "r_")   ~ "locality offsets",
                                      TRUE ~ "auxiliary")),
    icc_d |> transmute(variable = paste0("ICC_", Response), rhat, ess_bulk, ess_tail,
                       Class = "ICC (derived)"))
  diag_df$Class <- factor(diag_df$Class,
    levels = c("slopes", "intercepts", "locality SD", "correlations",
               "locality offsets", "auxiliary", "ICC (derived)"))

  p_rhat <- ggplot(diag_df, aes(rhat, Class, colour = Class)) +
    geom_vline(xintercept = 1.01, linetype = "dashed", linewidth = 0.3, colour = GREY) +
    geom_jitter(height = 0.18, size = 1.1, alpha = 0.75) +
    scale_colour_manual(values = setNames(c(PAL[1:6], INK), levels(diag_df$Class)),
                        guide = "none") +
    labs(x = expression(hat(R)), y = NULL, tag = "A") + fig_theme

  p_ess <- ggplot(diag_df, aes(ess_bulk, Class, colour = Class)) +
    geom_vline(xintercept = 10000, linetype = "dashed", linewidth = 0.3, colour = GREY) +
    geom_jitter(height = 0.18, size = 1.1, alpha = 0.75) +
    scale_colour_manual(values = setNames(c(PAL[1:6], INK), levels(diag_df$Class)),
                        guide = "none") +
    scale_x_continuous(labels = scales::comma) +
    labs(x = "bulk effective sample size", y = NULL, tag = "B") +
    fig_theme + theme(axis.text.y = element_blank())

  keep_fig(save_fig("convergence.png",
           (p_rhat + p_ess + plot_layout(widths = c(1, 1.15))) +
             plot_annotation(caption = paste(
               "Dashed lines: Rhat = 1.01, and the ESS of 10000 that Kruschke (2021)",
               "recommends for HDI limits.")) &
             theme(plot.caption = element_text(size = 7, colour = GREY, hjust = 0)),
           7.5, 2.8, dir = dir))

  # ========================================================================
  # SENSITIVITY: OVERLAID CUMULATIVE DISTRIBUTIONS
  # ========================================================================
  # sens_long arrives already reduced, one target per fit, so no large brmsfit
  # is opened here at all.
  icc_ecdf <- sens_long[sens_long$kind == "icc", ] |>
    mutate(Spec = factor(Spec, levels = SENS_LEVELS), Resp = resp_f(Response))
  p_se_icc <- ggplot(icc_ecdf, aes(x, colour = Spec)) +
    annotate("rect", xmin = 0, xmax = ICC_ROPE, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    stat_ecdf(geom = "step", linewidth = 0.4) +
    scale_colour_manual(values = SENS_COLS) +
    coord_cartesian(xlim = c(0, 0.75)) +
    facet_wrap(~ Resp, ncol = 4) +
    guides(colour = guide_legend(nrow = 1)) +
    labs(x = "ICC", y = "cumulative posterior probability") +
    fig_theme +
    theme(legend.position = "bottom", legend.margin = margin(t = -4),
          legend.key.width = unit(0.45, "cm"))
  keep_fig(save_fig("sensitivity_icc.png", p_se_icc, 8.0, 4.0, dir = dir))

  sl_ecdf <- sens_long[sens_long$kind == "slope", ] |>
    mutate(Spec = factor(Spec, levels = SENS_LEVELS),
           panel = paste0(unname(resp_lab[Response]), "\n", unname(pred_short[Predictor])),
           panel = factor(panel, levels = unique(panel)))

  p_se_sl <- ggplot(sl_ecdf, aes(x, colour = Spec)) +
    annotate("rect", xmin = -ROPE_SD, xmax = ROPE_SD, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    stat_ecdf(geom = "step", linewidth = 0.4) +
    scale_colour_manual(values = SENS_COLS) +
    coord_cartesian(xlim = c(-1.1, 1.1)) +
    facet_wrap(~ panel, ncol = 3) +
    guides(colour = guide_legend(nrow = 1)) +
    labs(x = expression(beta * " / SD of the response on its link scale"),
         y = "cumulative posterior probability") +
    fig_theme +
    theme(legend.position = "bottom", legend.margin = margin(t = -4),
          legend.key.width = unit(0.45, "cm"),
          strip.text = element_text(size = 6.5, lineheight = 1.05))
  keep_fig(save_fig("sensitivity_slopes.png", p_se_sl, 7.5, 5.2, dir = dir))

  message("\nall figures written to ", dir)
  out
}
