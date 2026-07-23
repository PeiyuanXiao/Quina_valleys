# bordes_typology.R
# Typological composition of the surface-collected Quina scrapers: the number
# of retouched edges, and the attribution of each specimen within Bordes' type
# list. Mirror of the `tbl-bordes-typology` chunk in paper/manuscript.qmd, so
# the two must give the same numbers.
#
# Every specimen carries Quina retouch, so the point of the table is the spread
# of forms within one technological concept rather than a division into groups:
# the types are therefore reported under Bordes' own families, with the family
# subtotal alongside each set of types.
#
# Two tidying steps are applied to the recorded attributions:
#   - internal double spaces are closed up;
#   - "Double convex-straight" and "Double straight-convex" are merged, these
#     being one type in Bordes' list recorded with the two edges either way round.
#
# Input:
#   - data/Quina_scraper_surface.xlsx (sheet "Quina scraper")
#
# Output:
#   - output/scraper_analysis/bordes_typology.csv

# ==============================================================================
# Setup
# ==============================================================================

required_packages <- c("readxl", "dplyr", "tibble")
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
library(tibble)
library(here)

sc_path    <- here("data", "Quina_scraper_surface.xlsx")
output_dir <- here("output", "scraper_analysis")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

f   <- function(x, d = 2) formatC(x, format = "f", digits = d)
IND <- strrep(" ", 3)   # indent for the type rows under a family heading

q <- read_excel(sc_path, sheet = "Quina scraper")

# ==============================================================================
# Number of retouched edges
# ==============================================================================

styp <- table(factor(trimws(q$Sub_typology), levels = c("Single", "Double", "Multi")))
cat("\nRetouched edges (n =", nrow(q), "):\n")
print(data.frame(edges = names(styp), n = as.integer(styp),
                 pct = f(100 * as.integer(styp) / nrow(q), 1)), row.names = FALSE)

# ==============================================================================
# Bordes attributions
# ==============================================================================

bordes <- gsub("\\s+", " ", trimws(as.character(q$Bordes_typology)))
bordes[bordes == "Double convex-straight scraper"] <- "Double straight-convex scraper"

# the type list in Bordes' order: recorded label -> family, type number, short label
bordes_key <- tribble(
  ~family,                          ~no,  ~short,            ~recorded,
  "Simple scrapers",                "9",  "straight",        "Single straight scraper",
  "Simple scrapers",                "10", "convex",          "Single convex scraper",
  "Simple scrapers",                "11", "concave",         "Single concave scraper",
  "Double scrapers",                "12", "straight",        "Double straight scraper",
  "Double scrapers",                "13", "straight–convex", "Double straight-convex scraper",
  "Double scrapers",                "15", "biconvex",        "Double convex scraper",
  "Double scrapers",                "16", "biconcave",       "Double concave scraper",
  "Double scrapers",                "17", "convex–concave",  "Double convex-concave scraper",
  "Convergent and déjeté scrapers", "18", "straight",        "Straight convergent scraper",
  "Convergent and déjeté scrapers", "19", "convex",          "Convex convergent scraper",
  "Convergent and déjeté scrapers", "20", "concave",         "Concave convergent scraper",
  "Convergent and déjeté scrapers", "21", "déjeté",          "Déjeté scraper",
  "Transverse scrapers",            "22", "straight",        "Straight transverse scraper",
  "Transverse scrapers",            "23", "convex",          "Convex transverse scraper",
  "Transverse scrapers",            "24", "concave",         "Concave transverse scraper",
  "Other forms",                    "25", "inverse",         "Inverse scraper",
  "Other forms",                    "29", "alternate",       "Alternate scraper",
  "Other forms",                    "28", "bifacial",        "Bifacial scraper",
  "Other forms",                    "—",  "triple",          "Triple scraper",
  "Other forms",                    "8",  "limace",          "Limace")
bordes_fam <- tribble(
  ~family,                          ~no_range,
  "Simple scrapers",                "9–11",
  "Double scrapers",                "12–17",
  "Convergent and déjeté scrapers", "18–21",
  "Transverse scrapers",            "22–24",
  "Other forms",                    "")
# a label recorded outside the key would drop specimens from the table
stopifnot(setequal(unique(bordes), bordes_key$recorded))

bordes_key$n <- vapply(bordes_key$recorded, function(l) sum(bordes == l), integer(1))
pc <- function(x) f(100 * x / length(bordes), 1)

bordes_tbl <- bind_rows(lapply(bordes_fam$family, function(fm) {
  k <- bordes_key[bordes_key$family == fm, ]
  bind_rows(
    data.frame(Type = fm, no = bordes_fam$no_range[bordes_fam$family == fm],
               n = sum(k$n), pct = pc(sum(k$n))),
    data.frame(Type = paste0(IND, k$short), no = k$no, n = k$n, pct = pc(k$n)))
})) |>
  bind_rows(data.frame(Type = "Total", no = "", n = length(bordes), pct = f(100, 1)))
names(bordes_tbl) <- c("Bordes_type", "No", "n", "pct")

cat("\nBordes attributions:\n\n")
print(bordes_tbl, row.names = FALSE, right = FALSE)

out_csv <- file.path(output_dir, "bordes_typology.csv")
write.csv(bordes_tbl, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat("\nWritten:", out_csv, "\n")
