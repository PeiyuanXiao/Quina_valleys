## Raw-material selection: composition + Jacobs' electivity index (D)
##
## Availability reference : Raw_mat_basin.xlsx           (Sheet1,           Lithology)
## Used assemblage        : SC Quina scraper  = Quina_scraper_surface.xlsx (sheet "Quina scraper", Raw_material)
##
## Category harmonisation : basin "Quartz sandstone" + "Coarse sandstone" -> "Sandstone"
##                          (so the basin lithologies and the tool raw materials are comparable)
##
## Jacobs' (1974) electivity index:  D = (r - p) / (r + p - 2 * r * p)
##   r = proportion of a raw material among the *used* tools
##   p = proportion of that raw material *available* in the basin
##   D ranges from -1 (complete avoidance) to +1 (complete selection); 0 = used in proportion to availability.

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2")
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
library(tidyr)
library(ggplot2)

basin_path <- "H:/Quina_valleys/data/Raw_mat_basin.xlsx"
sc_path    <- "H:/Quina_valleys/data/Quina_scraper_surface.xlsx"
output_dir <- "H:/Quina_valleys/output"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- shared raw-material levels and colours (Okabe-Ito palette) ----
material_levels <- c("Trachyte", "Sandstone", "Quartz", "Mudstone",
                     "Andesite")

material_colors <- c(
  Trachyte  = "#D55E00",
  Sandstone = "#E69F00",
  Quartz    = "#56B4E9",
  Mudstone  = "#0072B2",
  Andesite  = "#009E73"
)

group_levels <- c("Basin (available)", "SC Quina scraper")

## ---- shared plot theme ----
base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#454649",
                                 margin = margin(b = 8)),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

## ---- load and harmonise raw-material labels ----
read_material <- function(path, sheet, column) {
  values <- read_excel(path, sheet = sheet)[[column]]
  trimws(as.character(values))
}

basin_raw <- read_material(basin_path, "Sheet1", "Lithology")
sc_raw    <- read_material(sc_path, "Quina scraper", "Raw_material")

## Merge the two basin sandstone classes so they match the tools' "Sandstone".
basin_raw <- dplyr::recode(
  basin_raw,
  "Quartz sandstone" = "Sandstone",
  "Coarse sandstone" = "Sandstone"
)

## Guard against unexpected categories (typos, new materials) silently dropping out.
unknown_materials <- setdiff(
  unique(c(basin_raw, sc_raw)),
  c(material_levels, NA, "", "NA")
)
if (length(unknown_materials) > 0) {
  stop(
    "Raw-material categories not listed in `material_levels`: ",
    paste(unknown_materials, collapse = ", "),
    ". Add them to `material_levels`/`material_colors` before continuing."
  )
}

material_data <- bind_rows(
  tibble(Group = "Basin (available)", Material = basin_raw),
  tibble(Group = "SC Quina scraper",  Material = sc_raw)
) |>
  filter(!is.na(Material), !Material %in% c("", "NA")) |>
  mutate(
    Group    = factor(Group, levels = group_levels),
    Material = factor(Material, levels = material_levels)
  )

## ---- composition: counts and within-group percentages ----
composition <- material_data |>
  count(Group, Material, name = "n") |>
  complete(Group, Material, fill = list(n = 0)) |>
  group_by(Group) |>
  mutate(
    total   = sum(n),
    percent = n / total * 100
  ) |>
  ungroup()

cat("Raw-material composition (counts and within-group %):\n")
composition |>
  arrange(Group, Material) |>
  print(n = Inf)

write.csv(
  composition,
  file.path(output_dir, "raw_material_composition.csv"),
  row.names = FALSE
)

## ---- 100% stacked bar chart of composition ----
group_totals <- material_data |> count(Group, name = "n_total")
group_axis_labels <- setNames(
  sprintf("%s\n(n = %d)", group_totals$Group, group_totals$n_total),
  as.character(group_totals$Group)
)

stacked_plot <- ggplot(
  composition,
  aes(x = Group, y = percent, fill = Material)
) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(
    data = subset(composition, percent >= 5),
    aes(label = sprintf("%.1f%%", percent)),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = material_colors, drop = FALSE) +
  scale_x_discrete(labels = group_axis_labels) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Raw-material composition",
    x = NULL,
    y = "Percentage",
    fill = "Raw material"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

ggsave(
  filename = file.path(output_dir, "raw_material_stacked_bar.png"),
  plot = stacked_plot,
  width = 5.5,
  height = 5.4,
  dpi = 300
)

print(stacked_plot)

## ---- Jacobs' electivity index (D) ----
availability <- composition |>
  filter(Group == "Basin (available)") |>
  transmute(Material, p_avail = percent / 100)

jacobs_D <- function(r, p) {
  d <- (r - p) / (r + p - 2 * r * p)
  d[r == 0 & p == 0] <- NA_real_   # material absent from both basin and tools
  d
}

electivity <- composition |>
  filter(Group != "Basin (available)") |>
  transmute(Assemblage = droplevels(Group), Material, r_used = percent / 100) |>
  left_join(availability, by = "Material") |>
  mutate(
    p_avail = tidyr::replace_na(p_avail, 0),
    D       = jacobs_D(r_used, p_avail)
  ) |>
  filter(!is.na(D)) |>
  mutate(
    Selection = factor(
      ifelse(D >= 0, "Selected (preferred)", "Avoided"),
      levels = c("Selected (preferred)", "Avoided")
    )
  ) |>
  arrange(Assemblage, desc(D))

cat("\nJacobs' electivity index (D):\n")
print(electivity, n = Inf)

write.csv(
  electivity,
  file.path(output_dir, "jacobs_electivity_index.csv"),
  row.names = FALSE
)

## ---- electivity visualization (diverging bars) ----
electivity_plot <- ggplot(
  electivity,
  aes(x = D, y = Material, fill = Selection)
) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, color = "#202124", linewidth = 0.5) +
  geom_text(
    aes(
      label = sprintf("%+.2f", D),
      hjust = ifelse(D >= 0, -0.15, 1.15)
    ),
    size = 3, color = "#303238"
  ) +
  scale_fill_manual(
    values = c("Selected (preferred)" = "#009E73", "Avoided" = "#D55E00")
  ) +
  scale_x_continuous(limits = c(-1.35, 1.35), breaks = seq(-1, 1, 0.5)) +
  scale_y_discrete(limits = rev(material_levels), drop = FALSE) +
  labs(
    title = "Jacobs' electivity index (D) for raw-material selection",
    subtitle = "D > 0 = preferred relative to basin availability;  D < 0 = avoided",
    x = "Jacobs' D",
    y = "Raw material",
    fill = NULL
  ) +
  base_theme +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

ggsave(
  filename = file.path(output_dir, "jacobs_electivity_index.png"),
  plot = electivity_plot,
  width = 6.5,
  height = 5.0,
  dpi = 300
)

print(electivity_plot)
