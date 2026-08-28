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
# Sourcing:  the file defines functions and nothing else, so it can be sourced
# into a private environment.  The manuscript does exactly that, because
# barg_data.R defines objects (mod_dat, ROPE_SD, variables, squeeze) whose
# names _analysis.R also uses for its own versions of the same things:
#
#   e <- new.env(parent = globalenv())
#   for (s in c("barg_data.R", "barg_quantities.R", "barg_theme.R",
#               "barg_main_figure.R")) source(here("paper", "barg", s), local = e)
#   e$barg_main_figure(e$barg_fit("ref"), e$barg_fit("prior_ref"),
#                      e$barg_fit("noland"))
#
# barg_data.R, barg_quantities.R and barg_theme.R must be in scope; nothing
# here fits or refits anything.
# ==========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(posterior)
})
# The cached fits are brmsfit objects. Loading the brms namespace registers the
# methods posterior::as_draws_df() dispatches on, without attaching brms and
# masking anything in the environment of whoever is drawing the figure.
if (!requireNamespace("brms", quietly = TRUE))
  stop("brms is needed to read the cached fits", call. = FALSE)

# ---- the cache ------------------------------------------------------------
# Reads a fit by name from paper/barg/fits/ (or fits_quick/ under BARG_QUICK).
# Never samples: a missing fit is an error, because refitting is barg_fits.R's
# job and must not happen in the middle of a document render.
barg_fit <- function(name, dir = barg_fitdir()) {
  f <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(f))
    stop("cached fit not found: ", f,
         "\nRun  Rscript paper/barg/barg_fits.R  to build the cache.",
         call. = FALSE)
  readRDS(f)
}

barg_fitdir <- function() {
  here::here("paper", "barg",
             if (nzchar(Sys.getenv("BARG_QUICK"))) "fits_quick" else "fits")
}

# ---- the figure -----------------------------------------------------------
# fit_ref     the reference model, the three landscape terms included
# fit_prior   the same model sampled from the prior only, for the
#             prior/posterior width diagnostic behind the grey slopes
# fit_noland  the intercept-only model, the triangles in panel A
barg_main_figure <- function(fit_ref, fit_prior, fit_noland) {

  pred_f <- function(x) factor(unname(pred_short[x]), levels = unname(pred_short))

  # ---- A: the locality share of the variance, with and without the terms --
  icc_ref <- icc_table(fit_ref)   |> mutate(Model = "with landscape terms")
  icc_nol <- icc_table(fit_noland)|> mutate(Model = "intercept only")

  ord <- icc_ref$Response[order(icc_ref$Median)]               # ascending, so the
  lev <- unname(resp_lab[ord])                                 # largest sits on top

  icc_both <- bind_rows(icc_ref, icc_nol) |>
    mutate(Resp = factor(unname(resp_lab[Response]), levels = lev),
           Model = factor(Model, levels = c("with landscape terms", "intercept only")),
           Decided = CrI_lo > ICC_ROPE)

  p_a <- ggplot(icc_both, aes(Median, Resp)) +
    annotate("rect", xmin = 0, xmax = ICC_ROPE, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    geom_line(aes(group = Resp), colour = "#9AA0A6", linewidth = 0.3) +
    geom_linerange(data = filter(icc_both, Model == "with landscape terms"),
                   aes(xmin = CrI_lo, xmax = CrI_hi), linewidth = 0.35, colour = INK) +
    geom_point(aes(shape = Model, fill = Decided), size = 1.9, stroke = 0.4, colour = INK) +
    scale_shape_manual(values = c("with landscape terms" = 21, "intercept only" = 24)) +
    scale_fill_manual(values = c(`TRUE` = INK, `FALSE` = "white"), guide = "none") +
    scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1), expand = c(0.01, 0)) +
    labs(x = "Locality share of link-scale variance (ICC)", y = NULL, tag = "A",
         subtitle = "shaded: ICC below 0.05, treated as negligible") +
    fig_theme +
    theme(legend.position = "bottom", legend.margin = margin(t = -4),
          plot.subtitle = element_text(size = 6.8, colour = GREY),
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

  p_b <- ggplot(cf_ref, aes(Std_median, Resp)) +
    annotate("rect", xmin = -ROPE_SD, xmax = ROPE_SD, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    geom_vline(xintercept = c(-ROPE_SD, ROPE_SD), linewidth = 0.25,
               linetype = "dotted", colour = "#9AA0A6") +
    geom_vline(xintercept = 0, linewidth = 0.35, colour = GREY) +
    geom_linerange(aes(xmin = Std_lo, xmax = Std_hi, colour = Sign), linewidth = 0.35) +
    geom_point(aes(fill = Sign), shape = 21, size = 1.8, stroke = 0.35, colour = INK) +
    scale_colour_manual(values = c(positive = POS, negative = NEG,
                                   `prior-dominated` = "#9AA0A6"), guide = "none") +
    scale_fill_manual(values = c(positive = POS, negative = NEG,
                                 `prior-dominated` = "white"), guide = "none") +
    scale_x_continuous(limits = c(-xlim_b, xlim_b), breaks = scales::pretty_breaks(4)) +
    facet_wrap(~ Pred, nrow = 1) +
    labs(x = expression("Slope " * beta * " / SD of the response on its link scale"),
         y = NULL, tag = "B") +
    fig_theme +
    theme(axis.text.y = element_blank(), axis.text.x = element_text(size = 7),
          strip.text = element_text(size = 7.2, lineheight = 1.05),
          panel.spacing.x = unit(4, "pt"),
          panel.grid.major.y = element_line(color = "#EDEEF0", linewidth = 0.3),
          panel.grid.major.x = element_blank())

  p_a + p_b + plot_layout(widths = c(1, 1.6))
}

# The size the figure is drawn at, in inches: the report saves the PNG at this
# size and the manuscript chunk sets fig-width / fig-height to match.
BARG_MAIN_SIZE <- c(width = 7.5, height = 3.4)
