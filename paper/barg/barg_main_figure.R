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
# barg_data.R, barg_quantities.R and barg_theme.R must be in scope. Nothing
# here fits or refits anything on its own; ensure_barg_fits() delegates to
# barg_fits.R only when a document needs a fit the cache does not hold.
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
# Never samples: a missing fit is an error. Refitting is barg_fits.R's job,
# triggered once per document render by ensure_barg_fits() when the cache does
# not hold the fits the document needs.
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

# ---- the cache builder ---------------------------------------------------
# ensure_barg_fits() is called from the setup chunk of both documents, before
# any barg_fit(). If a fit the document needs is not in the cache, it runs
# barg_fits.R in a fresh R process, so that a first-time render builds the
# cache instead of failing. Running it as a subprocess keeps the render's own
# environment clean and lets barg_fits.R print its [fit ...] progress to the
# console. BARG_NOREFIT=1 forbids sampling during a render, and BARG_QUICK=1
# makes barg_fits.R write a reduced-iteration smoke-test cache to fits_quick/
# instead. Both environment variables are inherited by the subprocess, so
# nothing needs to be propagated by hand.
barg_cache_file <- function(name, dir) {
  file.path(dir, if (name == "manifest") "manifest.rds" else paste0(name, ".rds"))
}

ensure_barg_fits <- function(required = c("ref", "prior_ref", "noland", "manifest"),
                             dir = barg_fitdir()) {
  missing <- required[
    !file.exists(vapply(required, barg_cache_file, character(1), dir = dir))]
  if (!length(missing)) {
    message("[barg-cache] all required fits are present in ", dir)
    return(invisible(dir))
  }
  if (nzchar(Sys.getenv("BARG_NOREFIT")))
    stop("[barg-cache] fits missing from ", dir, ": ",
         paste(missing, collapse = ", "),
         ". BARG_NOREFIT is set, so the render will not build them.\n",
         "Build the cache once, offline, with  Rscript paper/barg/barg_fits.R\n",
         "and render again, or repeat the render with BARG_QUICK=1 for a\n",
         "reduced-iteration smoke test.",
         call. = FALSE)
  if (!requireNamespace("cmdstanr", quietly = TRUE))
    stop("[barg-cache] barg_fits.R needs the cmdstanr package, which is not\n",
         "installed here. Install cmdstanr and the CmdStan toolchain to rebuild\n",
         "the cache, or render on a machine where they are installed.\n",
         "  remotes::install_github(\"stan-dev/cmdstanr\")",
         call. = FALSE)
  if (nzchar(Sys.getenv("BARG_QUICK")))
    message("[barg-cache] building missing fits (", paste(missing, collapse = ", "),
            ") under BARG_QUICK: a reduced-iteration smoke test, minutes.")
  else
    message("[barg-cache] building missing fits (", paste(missing, collapse = ", "),
            ") by running  Rscript paper/barg/barg_fits.R.\n",
            "This is the full run: eleven MCMC fits with 20,000 post-warmup draws\n",
            "each, and it will take hours. Set BARG_QUICK=1 for a minutes-long\n",
            "smoke test, or BARG_NOREFIT=1 to fail fast instead.")
  script <- file.path(here::here("paper", "barg", "barg_fits.R"))
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  status <- suppressWarnings(system2(rscript, script, wait = TRUE))
  if (is.na(status) || status != 0)
    stop("[barg-cache] barg_fits.R failed (exit status ", status,
         "). See its output above; the render stops rather than continue with\n",
         "an incomplete cache.",
         call. = FALSE)
  still <- required[
    !file.exists(vapply(required, barg_cache_file, character(1), dir = dir))]
  if (length(still))
    stop("[barg-cache] barg_fits.R finished but the cache still lacks: ",
         paste(still, collapse = ", "), ".\n",
         "Check that the run wrote to the folder this document reads (",
         dir, ").",
         call. = FALSE)
  message("[barg-cache] fits ready in ", dir)
  invisible(dir)
}

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

  icc_ref <- icc_table(fit_ref)   |> mutate(Model = M_WITH)
  icc_nol <- icc_table(fit_noland)|> mutate(Model = M_WITHOUT)

  ord <- icc_ref$Response[order(icc_ref$Median)]               # ascending, so the
  lev <- unname(resp_lab[ord])                                 # largest sits on top

  icc_both <- bind_rows(icc_ref, icc_nol) |>
    mutate(Resp = factor(unname(resp_lab[Response]), levels = lev),
           Model = factor(Model, levels = c(M_WITH, M_WITHOUT)),
           Decided = CrI_lo > ICC_ROPE,
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
    scale_shape_manual(values = setNames(c(21, 24), c(M_WITH, M_WITHOUT))) +
    scale_fill_manual(values = c(`TRUE` = SLATE, `FALSE` = "white"), guide = "none") +
    scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1), expand = c(0.01, 0)) +
    facet_wrap(~ Panel) +
    labs(x = "Intraclass correlation coefficient", y = NULL, tag = "A") +
    fig_theme +
    theme(legend.position = "bottom", legend.margin = margin(t = -4),
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

  p_b <- ggplot(cf_ref, aes(Std_median, Resp)) +
    annotate("rect", xmin = -ROPE_SD, xmax = ROPE_SD, ymin = -Inf, ymax = Inf,
             fill = "#DCDDE0", alpha = 0.55) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "#8A9099") +
    geom_linerange(aes(xmin = Std_lo, xmax = Std_hi, colour = Sign), linewidth = 0.35) +
    geom_point(aes(fill = Sign, colour = Sign), shape = 21, size = 1.9, stroke = 0.4) +
    scale_colour_manual(values = c(positive = POS, negative = NEG,
                                   `prior-dominated` = "#9AA0A6"), guide = "none") +
    scale_fill_manual(values = c(positive = POS, negative = NEG,
                                 `prior-dominated` = "white"), guide = "none") +
    scale_x_continuous(limits = c(-xlim_b, xlim_b), breaks = scales::pretty_breaks(4)) +
    facet_wrap(~ Pred, nrow = 1) +
    labs(x = expression("Standardised slope (" * beta * " / SD)"),
         y = NULL, tag = "B") +
    fig_theme +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = STRIP_PT),
          panel.spacing.x = unit(7, "pt"),
          panel.grid.major.y = element_line(color = "#EDEEF0", linewidth = 0.3),
          panel.grid.major.x = element_blank())

  p_a + p_b + plot_layout(widths = c(1, 1.6))
}

# The size the figure is drawn at, in inches: the report saves the PNG at this
# size and the manuscript chunk sets fig-width / fig-height to match.
BARG_MAIN_SIZE <- c(width = 7.5, height = 3.4)
