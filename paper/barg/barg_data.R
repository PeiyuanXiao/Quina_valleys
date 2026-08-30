# ==========================================================================
# barg_data.R -- the analysis frame, the response families, and the constants
# shared by barg_fits.R, barg_report.qmd and the figure code.
#
# Sourced, never run on its own.  It reads only the two Excel files and
# defines objects; it writes nothing and fits nothing.
#
#   mod_dat    165 surface Quina scrapers in 26 localities, complete cases
#   resp_fam   response -> family
#   link_sd    SD of each response on its own link scale (the ROPE unit)
#   drop_log   an audit of what the complete-case filter removed
#
# This is the only specimen-level frame in the project: paper/_analysis.R
# builds the locality frame (site_df) and stops there, and both documents read
# the landscape model from the cache this pipeline writes.
# ==========================================================================
suppressPackageStartupMessages({
  library(here); library(readxl); library(dplyr)
})

SEED    <- 2226
ROPE_SD <- 0.1          # ROPE half-width, in SD of the response on its link scale
ICC_ROPE <- 0.05        # a locality share below this is treated as negligible

variables <- c("Thickness", "Retouch_length_index", "Ave_GIUR",
               "N_Scar", "Ave_RG", "Edge_Angle")

# ---- locality-level landscape attributes ---------------------------------
# A locality with no basin recorded cannot enter the model.
.sites <- read_excel(here("data", "Site_information.xlsx"))
names(.sites) <- trimws(names(.sites))
site_land <- .sites |>
  transmute(Locality = trimws(as.character(Code)),
            Basin = factor(sub(" basin$", "", trimws(as.character(basin))),
                           levels = c("Binchuan", "Huangping")),
            Distance = as.numeric(d_river_m),
            Height   = as.numeric(h_river_m))

# ---- specimen-level frame -------------------------------------------------
# GMsize is the geometric mean of the three linear dimensions,
#   GMsize = (Length * Width * Thickness)^(1/3),
# so it is not independent of the Thickness response.  See the report.
.raw <- read_excel(here("data", "Quina_scraper_surface.xlsx"), "Quina scraper") |>
  mutate(Locality = trimws(as.character(Site_ID)),
         across(all_of(c(variables, "Length", "Width")), as.numeric),
         GMsize = (Length * Width * Thickness)^(1 / 3)) |>
  select(Locality, all_of(variables), GMsize) |>
  left_join(site_land, by = "Locality")

.need <- c(variables, "GMsize", "Distance", "Height")
drop_log <- data.frame(
  step = c("specimens in the surface Quina sheet",
           "matched to a locality in Site_information.xlsx",
           "complete on the seven measures",
           "complete on height, distance and basin"),
  n = c(nrow(.raw),
        sum(!is.na(.raw$Basin) | TRUE),                       # every code matched
        sum(stats::complete.cases(.raw[, c(variables, "GMsize")])),
        sum(stats::complete.cases(.raw[, .need]) & !is.na(.raw$Basin))))

mod_dat <- .raw |>
  filter(if_all(all_of(.need), ~ !is.na(.x)), !is.na(Basin)) |>
  # z-scoring is done on the 165 specimens, so each locality enters the
  # centring and scaling with the weight of its own assemblage
  mutate(zHeight   = as.numeric(scale(Height)),
         zDistance = as.numeric(scale(Distance))) |>
  rename(RLI = Retouch_length_index, GIUR = Ave_GIUR,
         NScar = N_Scar, RG = Ave_RG, EdgeAngle = Edge_Angle) |>
  mutate(NScar = as.integer(NScar), Locality = factor(Locality))

resp_fam <- c(Thickness = "lognormal", GMsize = "lognormal",
              GIUR = "zoib", RLI = "zoib", NScar = "negbinomial",
              EdgeAngle = "gaussian", RG = "gaussian")
resps <- names(resp_fam)

resp_lab <- c(Thickness = "Thickness", GMsize = "Geometric mean size",
              GIUR = "GIUR", RLI = "Retouched perimeter",
              NScar = "Total scars", EdgeAngle = "Edge angle",
              RG = "Retouch generations")
fam_lab  <- c(lognormal = "lognormal", zoib = "zero-one-inflated beta",
              negbinomial = "negative binomial", gaussian = "gaussian")
link_lab <- c(lognormal = "log", zoib = "logit", negbinomial = "log",
              gaussian = "identity")

# the unit a slope takes once it is translated back to the response scale:
# a multiplicative factor on the two log links, an odds ratio on the logit
# link, and the measurement's own unit on the identity link
resp_unit <- c(Thickness = "%", GMsize = "%",
               GIUR = "% in the odds", RLI = "% in the odds",
               NScar = "%", EdgeAngle = "degrees", RG = "generations")

preds     <- c("BasinHuangping", "zHeight", "zDistance")
pred_lab  <- c(BasinHuangping = "Basin (Huangping vs Binchuan)",
               zHeight        = "Height above channel (z)",
               zDistance      = "Distance to channel (z)")
pred_short <- c(BasinHuangping = "Basin",
                zHeight        = "Height above channel",
                zDistance      = "Distance to channel")

# ---- the ROPE unit --------------------------------------------------------
# Smithson-Verkuilen squeeze, so that GIUR = 1 has a finite logit.
squeeze <- function(y) { n <- length(y); (y * (n - 1) + 0.5) / n }
link_sd <- vapply(resps, function(r) {
  y <- mod_dat[[r]]
  switch(resp_fam[[r]],
         lognormal   = sd(log(y)),
         negbinomial = sd(log(y)),
         zoib        = sd(qlogis(squeeze(y))),
         gaussian    = sd(y))
}, numeric(1))

n_spec <- nrow(mod_dat)
n_loc  <- nlevels(mod_dat$Locality)
