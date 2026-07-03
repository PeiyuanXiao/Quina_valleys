from __future__ import annotations

import csv
import json
import math
from collections import Counter
from collections import deque
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter


PROJ = Path("H:/Quina_valleys")
SCAN = PROJ / "data" / "geology_scan.jpg"
OUT = PROJ / "data" / "derived" / "geology_extraction"

# Inner neat-line corners and graticule values from scripts/05_geology_map_scan.R.
LEFT_X, RIGHT_X = 1176, 5059
TOP_Y, BOTTOM_Y = 19, 5728
LON_W, LON_E = 100.0, 101.0
LAT_N, LAT_S = 26 + 40 / 60, 25 + 20 / 60

# A deliberately broad, indicative key. The source sheet needs printed unit codes
# for authoritative lithology/age labels; color alone cannot reliably separate all
# units on this faded scan.
INDICATIVE = {
    "Quaternary alluvium/terraces": "#D8C66E",
    "Permian Emeishan basalt / green units": "#8C9A55",
    "Triassic / purple-brown units": "#AC8473",
    "Cretaceous-Paleogene red beds": "#CFAB8F",
    "Carboniferous-Permian carbonate / grey units": "#A6AE96",
    "Precambrian basement / brown units": "#7A5E3F",
    "Water or very pale blue areas": "#B9C7BD",
}


def hex_to_rgb(value: str) -> tuple[int, int, int]:
    value = value.lstrip("#")
    return tuple(int(value[i : i + 2], 16) for i in (0, 2, 4))


def rgb_to_hex(rgb: np.ndarray) -> str:
    vals = np.clip(np.rint(rgb), 0, 255).astype(int)
    return "#{:02X}{:02X}{:02X}".format(vals[0], vals[1], vals[2])


def assign_nearest_group(rgb: np.ndarray) -> str:
    palette = {k: np.array(hex_to_rgb(v), dtype=float) for k, v in INDICATIVE.items()}
    return min(palette, key=lambda k: float(np.sum((rgb - palette[k]) ** 2)))


def kmeans(sample: np.ndarray, k: int, seed: int = 1, iters: int = 30) -> np.ndarray:
    rng = np.random.default_rng(seed)
    centers = sample[rng.choice(sample.shape[0], k, replace=False)].astype(np.float64)
    labels = np.zeros(sample.shape[0], dtype=np.int32)
    for _ in range(iters):
        d = ((sample[:, None, :] - centers[None, :, :]) ** 2).sum(axis=2)
        new_labels = d.argmin(axis=1).astype(np.int32)
        if np.array_equal(labels, new_labels):
            break
        labels = new_labels
        for i in range(k):
            m = labels == i
            if np.any(m):
                centers[i] = sample[m].mean(axis=0)
            else:
                centers[i] = sample[rng.integers(0, sample.shape[0])]
    return centers


def classify_rgb(arr: np.ndarray, centers: np.ndarray, keep: np.ndarray) -> np.ndarray:
    flat = arr.reshape(-1, 3).astype(np.float32)
    keep_flat = keep.reshape(-1)
    out = np.full(flat.shape[0], 255, dtype=np.uint8)
    idx = np.flatnonzero(keep_flat)
    chunk = 350_000
    for start in range(0, idx.size, chunk):
        part_idx = idx[start : start + chunk]
        part = flat[part_idx]
        d = ((part[:, None, :] - centers[None, :, :]) ** 2).sum(axis=2)
        out[part_idx] = d.argmin(axis=1).astype(np.uint8)
    return out.reshape(arr.shape[:2])


def heal_and_smooth(class_img: np.ndarray) -> np.ndarray:
    arr = class_img.copy()
    shifts = [
        (-1, 0),
        (1, 0),
        (0, -1),
        (0, 1),
        (-1, -1),
        (-1, 1),
        (1, -1),
        (1, 1),
        (-2, 0),
        (2, 0),
        (0, -2),
        (0, 2),
    ]

    def shifted(a: np.ndarray, dr: int, dc: int) -> np.ndarray:
        out = np.full_like(a, 255)
        rs_src = slice(max(0, -dr), a.shape[0] - max(0, dr))
        cs_src = slice(max(0, -dc), a.shape[1] - max(0, dc))
        rs_dst = slice(max(0, dr), a.shape[0] - max(0, -dr))
        cs_dst = slice(max(0, dc), a.shape[1] - max(0, -dc))
        out[rs_dst, cs_dst] = a[rs_src, cs_src]
        return out

    # Grow neighboring fill classes into text/line/white pixels. This is less
    # elegant than a true nearest-neighbor distance transform, but avoids heavy
    # geospatial/image dependencies and works well for thin printed linework.
    for _ in range(90):
        missing = arr == 255
        if not missing.any():
            break
        before = int(missing.sum())
        for dr, dc in shifts:
            nb = shifted(arr, dr, dc)
            take = (arr == 255) & (nb != 255)
            arr[take] = nb[take]
        if int((arr == 255).sum()) == before:
            break

    img = Image.fromarray(arr.astype(np.uint8), mode="L")

    for size in (7, 7, 5, 3):
        img = img.filter(ImageFilter.ModeFilter(size=size))
    return np.array(img)


def block_mode(arr: np.ndarray, factor: int) -> np.ndarray:
    h, w = arr.shape
    hh = h // factor
    ww = w // factor
    trimmed = arr[: hh * factor, : ww * factor]
    out = np.empty((hh, ww), dtype=np.uint8)
    for r in range(hh):
        rows = trimmed[r * factor : (r + 1) * factor]
        for c in range(ww):
            vals = rows[:, c * factor : (c + 1) * factor].reshape(-1)
            vals = vals[vals != 255]
            out[r, c] = Counter(vals.tolist()).most_common(1)[0][0] if vals.size else 255
    return out


def sieve_small_components(grid: np.ndarray, min_cells: int, max_passes: int = 4) -> np.ndarray:
    out = grid.copy()
    h, w = out.shape
    neighbors = [(-1, 0), (1, 0), (0, -1), (0, 1)]

    for _ in range(max_passes):
        seen = np.zeros((h, w), dtype=bool)
        changed = False
        for r0 in range(h):
            for c0 in range(w):
                cls = int(out[r0, c0])
                if cls == 255 or seen[r0, c0]:
                    continue
                q = deque([(r0, c0)])
                seen[r0, c0] = True
                cells: list[tuple[int, int]] = []
                border: list[int] = []
                while q:
                    r, c = q.popleft()
                    cells.append((r, c))
                    for dr, dc in neighbors:
                        rr, cc = r + dr, c + dc
                        if rr < 0 or rr >= h or cc < 0 or cc >= w:
                            continue
                        nb = int(out[rr, cc])
                        if nb == cls and not seen[rr, cc]:
                            seen[rr, cc] = True
                            q.append((rr, cc))
                        elif nb != cls and nb != 255:
                            border.append(nb)
                if len(cells) < min_cells and border:
                    fill = Counter(border).most_common(1)[0][0]
                    for r, c in cells:
                        out[r, c] = fill
                    changed = True
        if not changed:
            break
    return out


def trace_boundary_rings(cells: list[tuple[int, int]], cell_set: set[tuple[int, int]]) -> list[list[tuple[int, int]]]:
    edge_starts: dict[tuple[int, int], list[tuple[int, int]]] = {}

    def add_edge(a: tuple[int, int], b: tuple[int, int]) -> None:
        edge_starts.setdefault(a, []).append(b)

    for r, c in cells:
        if (r - 1, c) not in cell_set:
            add_edge((c, r), (c + 1, r))
        if (r, c + 1) not in cell_set:
            add_edge((c + 1, r), (c + 1, r + 1))
        if (r + 1, c) not in cell_set:
            add_edge((c + 1, r + 1), (c, r + 1))
        if (r, c - 1) not in cell_set:
            add_edge((c, r + 1), (c, r))

    rings: list[list[tuple[int, int]]] = []
    while edge_starts:
        start = next(iter(edge_starts))
        ring = [start]
        cur = start
        guard = 0
        while True:
            guard += 1
            if guard > 1_000_000:
                break
            nexts = edge_starts.get(cur)
            if not nexts:
                break
            nxt = nexts.pop()
            if not nexts:
                edge_starts.pop(cur, None)
            ring.append(nxt)
            cur = nxt
            if cur == start:
                break
        if len(ring) > 4 and ring[-1] == ring[0]:
            rings.append(simplify_grid_ring(ring))
    return rings


def simplify_grid_ring(ring: list[tuple[int, int]]) -> list[tuple[int, int]]:
    if len(ring) <= 4:
        return ring
    closed = ring[-1] == ring[0]
    pts = ring[:-1] if closed else ring
    simplified: list[tuple[int, int]] = []
    n = len(pts)
    for i, p in enumerate(pts):
        prev = pts[(i - 1) % n]
        nxt = pts[(i + 1) % n]
        if (prev[0] == p[0] == nxt[0]) or (prev[1] == p[1] == nxt[1]):
            continue
        simplified.append(p)
    if simplified and simplified[0] != simplified[-1]:
        simplified.append(simplified[0])
    return simplified


def ring_to_lonlat(
    ring: list[tuple[int, int]],
    xmin: float,
    xmax: float,
    ymin: float,
    ymax: float,
    w: int,
    h: int,
) -> list[list[float]]:
    dx = (xmax - xmin) / w
    dy = (ymax - ymin) / h
    return [[xmin + x * dx, ymax - y * dy] for x, y in ring]


def planar_ring_area(coords: list[list[float]]) -> float:
    area = 0.0
    for (x1, y1), (x2, y2) in zip(coords, coords[1:]):
        area += x1 * y2 - x2 * y1
    return area / 2


def polygonize_grid(
    path: Path,
    grid: np.ndarray,
    legend: dict[int, dict[str, str]],
    xmin: float,
    xmax: float,
    ymin: float,
    ymax: float,
    min_cells: int = 4,
) -> dict[str, int]:
    grid = sieve_small_components(grid, min_cells=min_cells)
    h, w = grid.shape
    seen = np.zeros((h, w), dtype=bool)
    neighbors = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    dx = (xmax - xmin) / w
    dy = (ymax - ymin) / h
    cell_area_km2 = dx * 111.32 * math.cos(math.radians((ymax + ymin) / 2)) * dy * 110.57

    feature_count = 0
    component_count = 0
    with path.open("w", encoding="utf-8") as f:
        f.write('{"type":"FeatureCollection","name":"geology_polygons","features":[\n')
        first = True
        for r0 in range(h):
            for c0 in range(w):
                cls = int(grid[r0, c0])
                if cls == 255 or seen[r0, c0]:
                    continue
                q = deque([(r0, c0)])
                seen[r0, c0] = True
                cells: list[tuple[int, int]] = []
                while q:
                    r, c = q.popleft()
                    cells.append((r, c))
                    for dr, dc in neighbors:
                        rr, cc = r + dr, c + dc
                        if 0 <= rr < h and 0 <= cc < w and not seen[rr, cc] and int(grid[rr, cc]) == cls:
                            seen[rr, cc] = True
                            q.append((rr, cc))

                if len(cells) < min_cells:
                    continue
                component_count += 1
                cell_set = set(cells)
                rings = trace_boundary_rings(cells, cell_set)
                lonlat_rings = [ring_to_lonlat(ring, xmin, xmax, ymin, ymax, w, h) for ring in rings]
                lonlat_rings = [ring for ring in lonlat_rings if len(ring) >= 4]
                if not lonlat_rings:
                    continue
                lonlat_rings.sort(key=lambda coords: abs(planar_ring_area(coords)), reverse=True)

                props = {
                    "unit_id": legend[cls].get("unit_id", f"cluster_{cls + 1:02d}"),
                    "class_id": cls + 1,
                    "color": legend[cls]["color"],
                    "indicative_group": legend[cls]["indicative_group"],
                    "cells": len(cells),
                    "area_km2": round(len(cells) * cell_area_km2, 4),
                }
                feat = {
                    "type": "Feature",
                    "properties": props,
                    "geometry": {"type": "Polygon", "coordinates": lonlat_rings},
                }
                if not first:
                    f.write(",\n")
                json.dump(feat, f, ensure_ascii=False, separators=(",", ":"))
                first = False
                feature_count += 1
        f.write("\n]}\n")
    return {"features": feature_count, "components": component_count}


def write_worldfile(path: Path, xmin: float, xmax: float, ymin: float, ymax: float, w: int, h: int) -> None:
    dx = (xmax - xmin) / w
    dy = (ymin - ymax) / h
    x_center = xmin + dx / 2
    y_center = ymax + dy / 2
    path.write_text(
        f"{dx:.12f}\n0.0\n0.0\n{dy:.12f}\n{x_center:.12f}\n{y_center:.12f}\n",
        encoding="ascii",
    )


def write_prj(path: Path) -> None:
    path.write_text("EPSG:4326\n", encoding="ascii")


def write_geojson_grid(
    path: Path,
    grid: np.ndarray,
    legend: dict[int, dict[str, str]],
    xmin: float,
    xmax: float,
    ymin: float,
    ymax: float,
) -> int:
    h, w = grid.shape
    dx = (xmax - xmin) / w
    dy = (ymax - ymin) / h
    count = 0
    with path.open("w", encoding="utf-8") as f:
        f.write('{"type":"FeatureCollection","name":"geology_units_grid","features":[\n')
        first = True
        for r in range(h):
            c = 0
            while c < w:
                unit = int(grid[r, c])
                c2 = c + 1
                while c2 < w and int(grid[r, c2]) == unit:
                    c2 += 1
                if unit != 255:
                    x1 = xmin + c * dx
                    x2 = xmin + c2 * dx
                    y2 = ymax - r * dy
                    y1 = ymax - (r + 1) * dy
                    props = {
                        "unit_id": legend[unit].get("unit_id", f"cluster_{unit + 1:02d}"),
                        "class_id": unit + 1,
                        "color": legend[unit]["color"],
                        "indicative_group": legend[unit]["indicative_group"],
                    }
                    geom = {
                        "type": "Polygon",
                        "coordinates": [[[x1, y1], [x2, y1], [x2, y2], [x1, y2], [x1, y1]]],
                    }
                    feat = {"type": "Feature", "properties": props, "geometry": geom}
                    if not first:
                        f.write(",\n")
                    json.dump(feat, f, ensure_ascii=False, separators=(",", ":"))
                    first = False
                    count += 1
                c = c2
        f.write("\n]}\n")
    return count


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)

    img = Image.open(SCAN).convert("RGB")
    cropped = img.crop((LEFT_X, TOP_Y, RIGHT_X, BOTTOM_Y))
    cropped.save(OUT / "01_main_map_crop.png")

    factor = max(1, round(cropped.width / 1900))
    work_size = (cropped.width // factor, cropped.height // factor)
    work = cropped.resize(work_size, Image.Resampling.BILINEAR)
    arr = np.asarray(work).astype(np.float32)

    mx = arr.max(axis=2)
    mn = arr.min(axis=2)
    sat = np.where(mx == 0, 0, (mx - mn) / mx)
    dark_ink = ((mx < 95) & (sat < 0.36)) | (mx < 58)
    red_ink = (arr[:, :, 0] > 150) & ((arr[:, :, 0] - arr[:, :, 1]) > 48) & ((arr[:, :, 0] - arr[:, :, 2]) > 42)
    ink = dark_ink | red_ink
    inkrow = ink.mean(axis=1)

    top_row = 0
    for rr in range(max(1, arr.shape[0] - 25)):
        if np.all(inkrow[rr : rr + 25] < 0.34):
            top_row = rr
            break
    if top_row > 0:
        arr = arr[top_row:, :, :]

    lat_span = LAT_N - LAT_S
    lat_max = LAT_N - (top_row / work_size[1]) * lat_span
    lat_min = LAT_S
    lon_min = LON_W
    lon_max = LON_E

    mx = arr.max(axis=2)
    mn = arr.min(axis=2)
    sat = np.where(mx == 0, 0, (mx - mn) / mx)
    dark_ink = ((mx < 95) & (sat < 0.36)) | (mx < 58)
    red_ink = (arr[:, :, 0] > 150) & ((arr[:, :, 0] - arr[:, :, 1]) > 48) & ((arr[:, :, 0] - arr[:, :, 2]) > 42)
    ink = dark_ink | red_ink
    white = (mx > 238) & (sat < 0.08)
    keep = ~(ink | white)

    flat_keep = arr[keep].reshape(-1, 3)
    rng = np.random.default_rng(1)
    sample_n = min(80_000, flat_keep.shape[0])
    sample = flat_keep[rng.choice(flat_keep.shape[0], sample_n, replace=False)]
    centers = kmeans(sample, k=44, seed=1, iters=30)

    classified = classify_rgb(arr, centers, keep)
    clean_class = heal_and_smooth(classified)

    present = sorted(int(v) for v in np.unique(clean_class) if int(v) != 255)
    counts = {int(v): int((clean_class == v).sum()) for v in present}
    total = sum(counts.values())
    order = sorted(present, key=lambda v: counts[v], reverse=True)
    remap = {old: new for new, old in enumerate(order)}
    remapped = np.full(clean_class.shape, 255, dtype=np.uint8)
    new_centers = []
    for old in order:
        new_id = remap[old]
        remapped[clean_class == old] = new_id
        rgb = arr[clean_class == old].mean(axis=0)
        new_centers.append(rgb)
    centers_sorted = np.vstack(new_centers)

    clean_rgb = np.zeros((*remapped.shape, 3), dtype=np.uint8)
    for i, rgb in enumerate(centers_sorted):
        clean_rgb[remapped == i] = np.clip(np.rint(rgb), 0, 255).astype(np.uint8)
    Image.fromarray(clean_rgb, mode="RGB").save(OUT / "02_classified_clean_rgb.png")

    class_vis = remapped.copy()
    class_vis[class_vis == 255] = 0
    Image.fromarray(class_vis, mode="L").save(OUT / "03_classified_unit_ids.png")
    write_worldfile(OUT / "03_classified_unit_ids.pgw", lon_min, lon_max, lat_min, lat_max, remapped.shape[1], remapped.shape[0])
    write_prj(OUT / "03_classified_unit_ids.prj")

    # Coarse RLE rectangles keep the GeoJSON reasonably small while preserving coverage.
    grid_factor = 6
    grid = block_mode(remapped, grid_factor)

    legend: dict[int, dict[str, str]] = {}
    pixel_area_km2 = (
        ((lon_max - lon_min) / remapped.shape[1]) * 111.32 * math.cos(math.radians((lat_max + lat_min) / 2))
        * ((lat_max - lat_min) / remapped.shape[0]) * 110.57
    )
    with (OUT / "legend_units.csv").open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=["class_id", "unit_id", "color", "indicative_group", "pixels", "area_km2", "percent"],
        )
        writer.writeheader()
        for i, rgb in enumerate(centers_sorted):
            npx = int((remapped == i).sum())
            color = rgb_to_hex(rgb)
            group = assign_nearest_group(rgb.astype(float))
            legend[i] = {"color": color, "indicative_group": group}
            writer.writerow(
                {
                    "class_id": i + 1,
                    "unit_id": f"cluster_{i + 1:02d}",
                    "color": color,
                    "indicative_group": group,
                    "pixels": npx,
                    "area_km2": f"{npx * pixel_area_km2:.3f}",
                    "percent": f"{100 * npx / total:.3f}",
                }
            )

    with (OUT / "unit_interpretation_template.csv").open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "class_id",
                "unit_id",
                "source_color",
                "suggested_color_family",
                "area_km2",
                "percent",
                "map_code",
                "formal_unit_name",
                "lithology",
                "confidence",
                "notes",
            ],
        )
        writer.writeheader()
        for i, rgb in enumerate(centers_sorted):
            npx = int((remapped == i).sum())
            color = rgb_to_hex(rgb)
            group = assign_nearest_group(rgb.astype(float))
            writer.writerow(
                {
                    "class_id": i + 1,
                    "unit_id": f"cluster_{i + 1:02d}",
                    "source_color": color,
                    "suggested_color_family": group,
                    "area_km2": f"{npx * pixel_area_km2:.3f}",
                    "percent": f"{100 * npx / total:.3f}",
                    "map_code": "",
                    "formal_unit_name": "",
                    "lithology": "",
                    "confidence": "unverified",
                    "notes": "Fill from printed unit labels/legend before analytical use.",
                }
            )

    n_features = write_geojson_grid(
        OUT / "geology_units_grid_epsg4326.geojson",
        grid,
        legend,
        lon_min,
        lon_max,
        lat_min,
        lat_max,
    )
    polygon_stats = polygonize_grid(
        OUT / "geology_units_polygons_epsg4326.geojson",
        grid,
        legend,
        lon_min,
        lon_max,
        lat_min,
        lat_max,
        min_cells=5,
    )

    group_names = list(INDICATIVE.keys())
    group_colors = {name: INDICATIVE[name] for name in group_names}
    group_id = {name: i for i, name in enumerate(group_names)}
    lut = np.full(256, 255, dtype=np.uint8)
    for i in range(len(centers_sorted)):
        lut[i] = group_id[legend[i]["indicative_group"]]
    broad = lut[remapped]

    broad_rgb = np.zeros((*broad.shape, 3), dtype=np.uint8)
    for name, gid in group_id.items():
        broad_rgb[broad == gid] = np.array(hex_to_rgb(group_colors[name]), dtype=np.uint8)
    Image.fromarray(broad_rgb, mode="RGB").save(OUT / "04_broad_groups_rgb.png")
    write_worldfile(OUT / "04_broad_groups_rgb.pgw", lon_min, lon_max, lat_min, lat_max, broad.shape[1], broad.shape[0])
    write_prj(OUT / "04_broad_groups_rgb.prj")

    broad_legend = {
        gid: {
            "unit_id": f"group_{gid + 1:02d}",
            "color": group_colors[name],
            "indicative_group": name,
        }
        for name, gid in group_id.items()
    }
    broad_grid = block_mode(broad, grid_factor)
    broad_features = write_geojson_grid(
        OUT / "geology_broad_groups_grid_epsg4326.geojson",
        broad_grid,
        broad_legend,
        lon_min,
        lon_max,
        lat_min,
        lat_max,
    )
    broad_polygon_stats = polygonize_grid(
        OUT / "geology_broad_groups_polygons_epsg4326.geojson",
        broad_grid,
        broad_legend,
        lon_min,
        lon_max,
        lat_min,
        lat_max,
        min_cells=5,
    )

    with (OUT / "legend_broad_groups.csv").open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=["group_id", "unit_id", "color", "indicative_group", "pixels", "area_km2", "percent"],
        )
        writer.writeheader()
        valid = broad != 255
        valid_total = int(valid.sum())
        for name, gid in group_id.items():
            npx = int((broad == gid).sum())
            writer.writerow(
                {
                    "group_id": gid + 1,
                    "unit_id": f"group_{gid + 1:02d}",
                    "color": group_colors[name],
                    "indicative_group": name,
                    "pixels": npx,
                    "area_km2": f"{npx * pixel_area_km2:.3f}",
                    "percent": f"{100 * npx / valid_total:.3f}",
                }
            )

    # Small legend preview.
    row_h = 24
    preview = Image.new("RGB", (720, row_h * min(len(centers_sorted), 44) + 20), "white")
    draw = ImageDraw.Draw(preview)
    for i, rgb in enumerate(centers_sorted):
        y = 10 + i * row_h
        color = tuple(np.clip(np.rint(rgb), 0, 255).astype(np.uint8).tolist())
        draw.rectangle([10, y, 38, y + 16], fill=color, outline=(80, 80, 80))
        draw.text(
            (48, y),
            f"cluster_{i + 1:02d}  {rgb_to_hex(rgb)}  {legend[i]['indicative_group']}",
            fill=(20, 20, 20),
        )
    preview.save(OUT / "legend_preview.png")

    meta = {
        "source": str(SCAN),
        "scan_size_px": img.size,
        "crop_px": [LEFT_X, TOP_Y, RIGHT_X, BOTTOM_Y],
        "work_size_px": [remapped.shape[1], remapped.shape[0]],
        "downsample_factor": factor,
        "top_rows_trimmed_after_downsample": int(top_row),
        "extent_epsg4326": [lon_min, lat_min, lon_max, lat_max],
        "classes": len(centers_sorted),
        "geojson_features": n_features,
        "broad_geojson_features": broad_features,
        "polygon_features": polygon_stats["features"],
        "broad_polygon_features": broad_polygon_stats["features"],
        "notes": [
            "Classes are color clusters from the faded scan, not authoritative geologic unit codes.",
            "Use printed map labels/legend to rename or merge clusters before analysis.",
            "GeoJSON is a coarse run-length grid of rectangular coverage polygons in EPSG:4326.",
            "Polygon GeoJSON files dissolve adjacent grid cells after a small-component sieve.",
        ],
    }
    (OUT / "metadata.json").write_text(json.dumps(meta, indent=2, ensure_ascii=False), encoding="utf-8")

    print(json.dumps(meta, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
