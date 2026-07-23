# retouch_product_attributes.R
# Descriptive summary of the metric and reduction attributes recorded on the
# two classes of Quina retouch product from the surface collections: the Quina
# scrapers and the resharpening flakes detached in maintaining them. Mirror of
# the `tbl-retouch-products` chunk in paper/manuscript.qmd, so the two must
# give the same numbers.
#
# Each attribute is summarised as mean +/- SD with the median, quartiles and
# range alongside, since several of them (mass above all) are strongly skewed.
# n is per attribute, not per assemblage: the platform attributes are recorded
# only on the specimens that retain the platform of the blank.
#
# Pipeline:
#   1. Load both sheets of the surface assemblage.
#   2. Build one row per attribute, under a panel heading per artefact class.
#   3. Print the table and write it to CSV.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheets "Quina scraper", "Resharpening flake")
#
# Output:
#   - output/scraper_analysis/retouch_product_attributes.csv

# ==============================================================================
# Setup
# ==============================================================================

required_packages <- c("readxl", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readxl)
library(dplyr)
library(here)

sc_path    <- here("data", "Quina_scraper_surface.xlsx")
output_dir <- here("output", "scraper_analysis")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

f   <- function(x, d = 2) formatC(x, format = "f", digits = d)
IND <- strrep(intToUtf8(160), 3)   # indent for the rows under a panel heading

# ==============================================================================
# Table rows
# ==============================================================================

# one row per attribute; d = decimals for mean/SD, ds for the order statistics
row_num <- function(label, x, d = 1, ds = d) {
  x  <- as.numeric(x); x <- x[is.finite(x)]
  qs <- unname(quantile(x, c(.25, .75)))
  data.frame(Attribute = paste0(IND, label),
             mn  = f(mean(x), d), sd = f(sd(x), d),
             med = f(median(x), ds),
             iqr = paste0(f(qs[1], ds), "–", f(qs[2], ds)))
}
# panel heading, keeping the two classes of retouch product apart
row_head <- function(label, n_total) {
  data.frame(Attribute = sprintf("%s (n = %d)", label, n_total),
             mn = "", sd = "", med = "", iqr = "")
}

q   <- read_excel(sc_path, sheet = "Quina scraper")
rfl <- read_excel(sc_path, sheet = "Resharpening flake")

retouch_products_tbl <- bind_rows(
  row_head("Quina scrapers", nrow(q)),
  row_num("Length (mm)",    q$Length,    1),
  row_num("Width (mm)",     q$Width,     1),
  row_num("Thickness (mm)", q$Thickness, 1),
  row_num("Mass (g)",       q$Mass,      1),
  row_num("Platform depth (mm)", q$Platform_depth, 1),
  row_num("Platform width (mm)", q$Platform_width, 1),
  row_num("IPA (°)",             q$IPA,            1),
  row_num("Retouch generations", q$Ave_RG,    2),
  row_num("Retouch scars",       q$N_Scar,    1, ds = 0),
  row_num("GIUR",                q$Ave_GIUR,  2),
  row_num("Edge angle (°)",      q$Edge_Angle, 1),
  row_num("Retouched perimeter", q$Retouch_length_index, 2),
  row_head("Resharpening flakes", nrow(rfl)),
  row_num("Length (mm)",    rfl$Length,    1),
  row_num("Width (mm)",     rfl$Width,     1),
  row_num("Thickness (mm)", rfl$Thickness, 1),
  row_num("Mass (g)",       rfl$Mass,      1),
  row_num("Platform depth (mm)", rfl$Platform_depth, 1),
  row_num("Platform width (mm)", rfl$Platform_width, 1),
  row_num("IPA (°)",             rfl$IPA,            1),
  row_num("EPA (°)",             rfl$EPA,            1))

names(retouch_products_tbl) <- c("Attribute", "Mean", "SD", "Median", "Q1_Q3")

cat("\nMetric and reduction attributes of the surface Quina retouch products:\n\n")
print(retouch_products_tbl, row.names = FALSE, right = FALSE)

out_csv <- file.path(output_dir, "retouch_product_attributes.csv")
write.csv(retouch_products_tbl, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat("\nWritten:", out_csv, "\n")
