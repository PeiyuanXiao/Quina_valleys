# ==========================================================================
# barg_main_figure.R -- the two-panel landscape figure, built from the cached
# fits and returned as a plot object rather than written to disk.
#
#   A  the intercept correlation ratio for each response, with and without
#      the landscape terms  (Q1: is there anything to explain?)
#   B  the twenty-one landscape slopes on one standardised axis
#      (Q2: do the landscape terms explain it?)
#
# Two callers draw it from this one definition, so that the report and the
# manuscript cannot drift apart:
#   paper/barg/barg_figures.R  saves it as barg_main.png for the BARG report
#   paper/manuscript.qmd       draws it inline as Figure 9
#
# Sourcing:  the file defines functions and nothing else, so it goes into the
# report context barg_context.R builds, where barg_data.R, barg_quantities.R
# and barg_theme.R are already in scope.  A private environment is needed
# rather than the caller's, because barg_data.R defines objects (mod_dat,
# ROPE_SD, variables, squeeze) whose names paper/_analysis.R also uses for its
# own versions of the same things:
#
#   ctx <- barg_context_report(scraper_xlsx, site_xlsx)
#   ctx$barg_main_figure(fit_ref, fit_prior, fit_noland)
#
# Nothing here reads a cache or fits anything.  The three fits are arguments,
# supplied by _targets.R; run tar_make() to build them.
# ==========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(posterior)
})
# The cached fits are brmsfit objects. Loading the brms namespace registers the
# methods posterior::as_draws_df() dispatches on, without attaching brms and
# masking anything in the environment of whoever is drawing the figure.
if (!requireNamespace("brms", quietly = TRUE))
  stop("brms is needed to read the cached fits", call. = FALSE)

# ---- the figure -----------------------------------------------------------
# fit_ref     the reference model, the three landscape terms included
# fit_prior   the same model sampled from the prior only, for the
#             prior/posterior width diagnostic behind the grey slopes
# fit_noland  the intercept-only model, the triangles in panel A
barg_main_figure <- function(fit_ref, fit_prior, fit_noland) {

  pred_f <- function(x) factor(unname(pred_short[x]), levels = unname(pred_short))

  # One size for every strip in the figure.  Below the theme's 8.5, because the
  # three facets of B are only about 1.3 in wide and the longest predictor name
  # is clipped at the theme size.
  STRIP_PT <- 7.6

  # ---- A: the locality share of the variance, with and without the terms --
  # The two models differ by the three landscape terms and by nothing else, so
  # the legend names them as that one contrast rather than by model type.
  M_WITH <- "With landscape terms"; M_WITHOUT <- "Without landscape terms"

  # Filled or hollow is the Q1 verdict, in the words main-text Table 4 uses, so
  # that the figure and the table read against each other.  The two legend rows
  # each vary one thing only -- the first two filled symbols differing in shape,
  # the second two circles differing in fill -- so that neither row can be
  # mistaken for the other's code.
  D_YES <- "Locality variation present"; D_NO <- "Undecided"

  icc_ref <- icc_table(fit_ref)   |> mutate(Model = M_WITH)
  icc_nol <- icc_table(fit_noland)|> mutate(Model = M_WITHOUT)

  ord <- icc_ref$Response[order(icc_ref$Median)]               # ascending, so the
  lev <- unname(resp_lab[ord])                                 # largest sits on top

  icc_both <- bind_rows(icc_ref, icc_nol) |>
    mutate(Resp = factor(unname(resp_lab[Response]), levels = lev),
           Model = factor(Model, levels = c(M_WITH, M_WITHOUT)),
           Decided = factor(ifelse(CrI_lo > ICC_ROPE, D_YES, D_NO),
                            levels = c(D_YES, D_NO)),
           Panel = "Between-locality variation")

  # The panel is faceted on a constant, purely so that it carries the same grey
  # strip as the three facets of B and the two panels align along their tops.
  p_a <- ggplot(icc_both, aes(Median, Resp)) +
    annotate("rect", xmin = 0, xmax = ICC_ROPE, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    geom_line(aes(group = Resp), colour = "#9AA0A6", linewidth = 0.3) +
    geom_linerange(data = filter(icc_both, Model == M_WITH),
                   aes(xmin = CrI_lo, xmax = CrI_hi), linewidth = 0.35, colour = SLATE) +
    geom_point(aes(shape = Model, fill = Decided), size = 1.9, stroke = 0.4,
               colour = SLATE) +
    scale_shape_manual(values = setNames(c(21, 24), c(M_WITH, M_WITHOUT)),
                       guide = guide_legend(order = 1,
                                            override.aes = list(fill = SLATE))) +
    scale_fill_manual(values = setNames(c(SLATE, "white"), c(D_YES, D_NO)),
                      drop = FALSE,
                      guide = guide_legend(order = 2,
                                           override.aes = list(shape = 21))) +
    scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1), expand = c(0.01, 0)) +
    facet_wrap(~ Panel) +
    labs(x = "Intraclass correlation coefficient", y = NULL, tag = "A") +
    fig_theme +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",   # the two rows share a left edge
          legend.margin = margin(t = -4, b = 0), legend.spacing.y = unit(3, "pt"),
          legend.key.height = unit(11, "pt"), legend.key.width = unit(11, "pt"),
          legend.text = element_text(size = 8, margin = margin(l = 1, r = 7)),
          strip.text = element_text(size = STRIP_PT),
          panel.grid.major.y = element_line(color = "#EDEEF0", linewidth = 0.3),
          panel.grid.major.x = element_blank())

  # ---- B: the twenty-one slopes on one standardised axis ------------------
  # A slope whose posterior is still close to its prior has not been informed
  # by the data and must not be read as an estimate.  Any such slope is drawn
  # grey and hollow; the threshold is a posterior/prior SD ratio above 0.5.
  ppsd <- prior_post_sd(fit_ref, fit_prior)
  prior_dom <- ppsd |> filter(ratio > 0.5) |> transmute(Response, Predictor, dom = TRUE)
  message("prior-dominated slopes (post/prior SD > 0.5): ",
          if (nrow(prior_dom)) paste(prior_dom$Response, prior_dom$Predictor,
                                     collapse = "; ") else "none")

  cf_ref <- coef_table(fit_ref) |>
    left_join(prior_dom, by = c("Response", "Predictor")) |>
    mutate(Resp = factor(unname(resp_lab[Response]), levels = lev),
           Pred = pred_f(Predictor),
           dom = !is.na(dom),
           Sign = factor(ifelse(dom, "prior-dominated",
                         ifelse(Std_median >= 0, "positive", "negative")),
                         levels = c("positive", "negative", "prior-dominated")))

  xlim_b <- max(abs(c(cf_ref$Std_lo, cf_ref$Std_hi))) * 1.06

  # The colour is the sign of the posterior median and nothing more, so the
  # legend says median.  Its keys are the levels the panel actually draws: the
  # grey key appears only in a run where some slope is prior-dominated, which
  # the reference fit has none of.
  SIGN_LAB <- c(positive = "Positive median", negative = "Negative median",
                `prior-dominated` = "Prior-dominated")
  sign_lev <- levels(droplevels(cf_ref$Sign))
  sign_guide <- guide_legend()

  p_b <- ggplot(cf_ref, aes(Std_median, Resp)) +
    annotate("rect", xmin = -ROPE_SD, xmax = ROPE_SD, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "#8A9099") +
    geom_linerange(aes(xmin = Std_lo, xmax = Std_hi, colour = Sign), linewidth = 0.35,
                   show.legend = FALSE) +
    geom_point(aes(fill = Sign, colour = Sign), shape = 21, size = 1.9, stroke = 0.4) +
    scale_colour_manual(values = c(positive = POS, negative = NEG,
                                   `prior-dominated` = "#9AA0A6"),
                        breaks = sign_lev, labels = unname(SIGN_LAB[sign_lev]),
                        guide = sign_guide) +
    scale_fill_manual(values = c(positive = POS, negative = NEG,
                                 `prior-dominated` = "white"),
                      breaks = sign_lev, labels = unname(SIGN_LAB[sign_lev]),
                      guide = sign_guide) +
    scale_x_continuous(limits = c(-xlim_b, xlim_b), breaks = scales::pretty_breaks(4)) +
    facet_wrap(~ Pred, nrow = 1) +
    labs(x = expression("Standardised slope (" * beta * " / SD)"),
         y = NULL, tag = "B") +
    fig_theme +
    theme(axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(t = -4, b = 0),
          legend.key.height = unit(9, "pt"), legend.key.width = unit(11, "pt"),
          legend.text = element_text(size = 8, margin = margin(l = 1, r = 7)),
          strip.text = element_text(size = STRIP_PT),
          panel.spacing.x = unit(7, "pt"),
          panel.grid.major.y = element_line(color = "#EDEEF0", linewidth = 0.3),
          panel.grid.major.x = element_blank())

  p_a + p_b + plot_layout(widths = c(1, 1.6))
}

# The size the figure is drawn at, in inches: the report saves the PNG at this
# size and the manuscript chunk sets fig-width / fig-height to match.
BARG_MAIN_SIZE <- c(width = 7.5, height = 3.4)
