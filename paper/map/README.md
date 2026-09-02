# Map scripts for Figure 1

These R scripts build every component of **Figure 1** of the manuscript (the study-area map). They run as a set: the first builds the spatial cache, the others draw the map panels, and the last exports them at final placed size for manual assembly into `figures/study_area.png`.

All scripts use `here::here()` to resolve paths, so they must be run from the project root (the folder containing `Quina_valleys.Rproj`).

---

## Scripts

| Script | Role | Output | Dependencies |
|--------|------|--------|--------------|
| `setup.R` | Builds the spatial cache: DEM, hillshade, channel network, admin boundaries | `data/cache/*.tif`, `data/cache/*.gpkg` | Network connection; WhiteboxTools on first run (~70 MB) |
| `terra_map_2D.R` | Local plan map (base layer for panel A). Defines the shaded-relief model, site symbols, hypsometric wash | `output/maps/map_quina_sites_{sheet,landscape}.png/pdf` | `data/cache/dem.tif`, `data/cache/rivers_dem.gpx` |
| `terra_map_2D_regional.R` | Wider regional plan map showing the Jinsha River (panel B) | `output/figures/fig01_panels/panel_B_map_regional_jinsha.png/pdf` | Builds its own regional cache; does **not** require `setup.R` |
| `terra_map_3D_hyps.R` | 3-D block model (panel B). Uses the same hypsometric colours as the 2-D sheets | `output/maps/terrain_3d_hyps.png` | `data/cache/dem.tif`; `terra_map_2D.R` (for the drape texture in "landscape" mode) |
| `traverse_profile_figure.R` | Topographic profile along the traverse through all 27 sites (panel C) | `output/figures/fig_traverse_profile.png/pdf` | `data/cache/dem.tif`, `data/Site_information.xlsx` |
| `locator_globe_figure.R` | Locator inset: orthographic globe centred on the study area | `output/figures/fig01_panels/locator_globe.png/pdf` | rnaturalearth only — standalone, no cache needed |
| `fig01_export_panels.R` | Exports panels A and C at their final placed sizes for manual composition. Re-sources `terra_map_2D.R` and `traverse_profile_figure.R` | `output/figures/fig01_panels/panel_A_map*.png/pdf,<br>panel_C_profile*.png/pdf` | `data/cache/` (via the re-sourced scripts) |

---

## What each panel becomes

- **Panel A** — plan map: produced by `terra_map_2D.R`, exported at final size by `fig01_export_panels.R`.
- **Panel B** — two options, chosen in composition:
  - The 3-D block (`output/maps/terrain_3d_hyps.png`), from `terra_map_3D_hyps.R`.
  - The regional map with the Jinsha (`output/figures/fig01_panels/panel_B_map_regional_jinsha.png`), from `terra_map_2D_regional.R`.
- **Panel C** — traverse profile: from `traverse_profile_figure.R`, exported at final size by `fig01_export_panels.R`.
- **Locator inset** — globe inset: from `locator_globe_figure.R`, written straight to the panel folder.

The final `figures/study_area.png` is assembled by hand from these exported panels in Illustrator.

---

## Run order

1. `setup.R` — builds `data/cache/` once. Everything after this needs no network.

2. (optional) `terra_map_2D_regional.R` — the regional Jinsha map. Builds its own cache; skip if not needed.

3. `terra_map_3D_hyps.R` — the 3-D block (panel B / block option).

4. `traverse_profile_figure.R` — the topographic profile (panel C).

5. `locator_globe_figure.R` — the locator globe inset.

6. `fig01_export_panels.R` — exports panels A and C at final placed size, with the traverse route drawn on the map. If a palette argument is needed, pass it as command-line args.

To rebuild only one panel, delete its output from `output/` and run the corresponding script directly — except panels A and C, which must go through `fig01_export_panels.R` to be exported at the correct placed size.

---

## How long each script takes

| Script | First run | After cache exists |
|--------|-----------|--------------------|
| `setup.R` | ~30–60 min (downloads SRTM DEM; installs WhiteboxTools 70 MB; extracts channel network via WhiteboxTools) | ~1 min (everything cached) |
| `terra_map_2D.R` | ~10 min @ 600 dpi | ~5 min (re-renders only) |
| `terra_map_2D_regional.R` | ~20 min (downloads regional DEM, solves drainage with Whitebox breach-and-route) | ~5 min (cache reused) |
| `terra_map_3D_hyps.R` | ~5 min (preview, 80 samples) / ~45 min (publication, 400 samples) | ~5 min (preview reused) / ~45 min (repaint at full res) |
| `traverse_profile_figure.R` | ~3 min | ~2 min |
| `locator_globe_figure.R` | ~2 min (downloads coastlines on first run) | ~1 min |
| `fig01_export_panels.R` | ~12 min (re-runs `terra_map_2D.R` + `traverse_profile_figure.R` in-process, then exports both panels at 600 dpi) | ~10 min |

The 3-D block render at publication resolution is the most CPU-intensive step; the rest are limited by download speed and raster size, not compute.

---

## Environment variables and switches

- The map scripts take no environment variables of their own beyond those listed below. The Bayesian fits in `paper/barg/` are built by `targets::tar_make()` from the project root and are unrelated to these scripts.
- `terra_map_3D_hyps.R` accepts optional command-line arguments: `Rscript terra_map_3D_hyps.R <variant> <warm_mix> <albedo_gain> <key_intensity> <aspect_mix> <albedo_contrast> <shadow_darken> <slab_m>`. With a variant tag, it renders a fast preview (80 samples).
- `terra_map_2D.R` is sourced by `fig01_export_panels.R` with environment variables `PALETTE_MODE` ("sheet" or "landscape"), `MAP_EXT` (for the regional frame), `SKIP_EXPORT` (TRUE to skip the file write), and `SHOW_LEGEND`/`SHOW_GRID` (for bare-map mode).
