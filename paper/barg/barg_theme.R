# ==========================================================================
# barg_theme.R -- the manuscript figure style, copied verbatim from the
# fig_theme the manuscript uses, so that every figure in the BARG report sits
# beside the manuscript's own figures without a visible seam.
# ==========================================================================
suppressPackageStartupMessages(library(ggplot2))

fig_theme <- theme_minimal(base_size = 9) +
  theme(panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.5),
        axis.ticks = element_line(color = "#202124", linewidth = 0.3),
        axis.ticks.length = unit(2, "pt"),
        axis.title = element_text(size = 9),
        axis.text = element_text(color = "#303238", size = 8),
        strip.text = element_text(face = "bold", color = "#202124", size = 8.5),
        strip.background = element_rect(fill = "#E8E8E8", color = NA),
        legend.title = element_blank(), legend.text = element_text(size = 8.5),
        legend.key = element_blank(),
        plot.tag = element_text(face = "bold", size = 11),
        plot.background = element_rect(color = NA, fill = "white"),
        panel.background = element_rect(color = NA, fill = "white"))

PAL  <- c("#E07C90", "#6BA8CE", "#E6C25C", "#9B87C4", "#6FB98E", "#E69F00")
INK  <- "#202124"; GREY <- "#5A5F66"; TXT <- "#303238"
POS  <- "#E07C90"; NEG <- "#6BA8CE"; MID <- "#F4F4F5"

# the six prior specifications, in a fixed order and with fixed colours, so
# that every sensitivity display reads the same way
SENS_LEVELS <- c("REF", "S1", "S2", "S3", "S4", "S5")
SENS_COLS   <- setNames(c(INK, PAL[1], PAL[2], PAL[3], PAL[5], PAL[4]), SENS_LEVELS)

FIGDIR <- here::here("paper", "barg", "figures")
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(name, plot, width, height) {
  f <- file.path(FIGDIR, name)
  ggsave(f, plot, width = width, height = height, dpi = 300, bg = "white")
  message("written: ", f)
  invisible(f)
}
