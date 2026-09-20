# A synthetic dataset with the same structure as the real one, for verifying
# that the pipeline runs without the specimen measurements.  Reads the fit_ref
# target, writes paper/barg/dummy_data.csv; run from the project root with
# Rscript paper/barg/barg_dummy_data.R.
#
# The responses are one posterior predictive draw, so they carry the right
# ranges, locality structure and dependence while reproducing no individual
# specimen; the locality attributes are jittered.  Nothing in the report is
# computed from this file.
suppressPackageStartupMessages({
  library(here); library(brms); library(dplyr)
})
source(here("paper", "barg", "barg_data.R"))

fit <- targets::tar_read(fit_ref)
set.seed(SEED)

pp <- posterior_predict(fit, ndraws = 1)
one <- function(r) as.vector(if (is.list(pp)) pp[[r]] else pp[, , r])

loc_key <- data.frame(Locality = levels(mod_dat$Locality)) |>
  mutate(FakeID = sprintf("L%02d", sample(seq_len(n_loc))))

dummy <- mod_dat |>
  transmute(Locality, Basin,
            Height   = round(Height   * runif(n(), 0.85, 1.15)),
            Distance = round(Distance * runif(n(), 0.85, 1.15))) |>
  left_join(loc_key, by = "Locality") |>
  group_by(Locality) |>                     # keep the attributes constant per locality
  mutate(Height = first(Height), Distance = first(Distance)) |>
  ungroup() |>
  transmute(Site_ID = FakeID, Basin, h_river_m = Height, d_river_m = Distance,
            Thickness            = round(one("Thickness"), 2),
            GMsize               = round(one("GMsize"), 2),
            Ave_GIUR             = round(one("GIUR"), 4),
            Retouch_length_index = round(one("RLI"), 4),
            N_Scar               = as.integer(one("NScar")),
            Edge_Angle           = round(one("EdgeAngle"), 2),
            Ave_RG               = round(one("RG"), 2)) |>
  arrange(Site_ID)

# reproduce the one missing GIUR that the complete-case filter removes
dummy$Ave_GIUR[sample(nrow(dummy), 1)] <- NA_real_

out <- here("paper", "barg", "dummy_data.csv")
write.csv(dummy, out, row.names = FALSE, na = "")
message("written: ", out, "  (", nrow(dummy), " rows, ",
        length(unique(dummy$Site_ID)), " localities)")
