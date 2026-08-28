# ==========================================================================
# barg_dummy_data.R -- a synthetic dataset with the same structure as the
# real one, for readers who need to verify that the pipeline runs without
# having access to the specimen measurements.
#
# Reads:   paper/barg/fits/ref.rds  (the fitted reference model)
# Writes:  paper/barg/dummy_data.csv
#
# Run from the project root:  Rscript paper/barg/barg_dummy_data.R
#
# The responses are one posterior predictive draw from the fitted model, so
# they carry the right ranges, the right locality structure and the right
# dependence between measures, and reproduce no individual specimen.  The
# locality attributes (basin, height, distance) are jittered so that the real
# find spots cannot be recovered from the file.  Nothing in the report is
# computed from this file; it exists only so that the scripts can be run.
# ==========================================================================
suppressPackageStartupMessages({
  library(here); library(brms); library(dplyr)
})
source(here("paper", "barg", "barg_data.R"))

fit <- readRDS(here("paper", "barg", "fits", "ref.rds"))
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
