from __future__ import annotations

import csv
import json
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFont


PROJ = Path("H:/Quina_valleys")
DEM = PROJ / "data" / "cache" / "dem.tif"
GEO_DIR = PROJ / "data" / "derived" / "geology_extraction"
OUT = PROJ / "output"


def load_geotiff_extent(path: Path) -> tuple[Image.Image, tuple[float, float, float, float]]:
    img = Image.open(path)
    tags = img.tag_v2
    scale = tags[33550]
    tie = tags[33922]
    dx = float(scale[0])
    dy = float(scale[1])
    xmin = float(tie[3])
    ymax = float(tie[4])
    xmax = xmin + img.width * dx
    ymin = ymax - img.height * dy
    return img, (xmin, ymin, xmax, ymax)


def terrain_rgb(z: np.ndarray) -> np.ndarray:
    valid = np.isfinite(z)
    lo, hi = np.nanpercentile(z[valid], [2, 98])
    t = np.clip((z - lo) / (hi - lo), 0, 1)
    stops = np.array(
        [
            [58, 92, 84],
            [103, 126, 78],
            [172, 158, 100],
            [188, 172, 135],
            [226, 222, 204],
        ],
        dtype=np.float32,
    )
    x = t * (len(stops) - 1)
    i = np.clip(np.floor(x).astype(int), 0, len(stops) - 2)
    f = (x - i)[..., None]
    rgb = stops[i] * (1 - f) + stops[i + 1] * f
    return np.clip(rgb, 0, 255).astype(np.uint8)


def hillshade(z: np.ndarray, azimuth: float = 315, altitude: float = 40) -> np.ndarray:
    z = z.astype(np.float32)
    z = np.where(np.isfinite(z), z, np.nanmedian(z))
    dy, dx = np.gradient(z)
    slope = np.pi / 2 - np.arctan(np.sqrt(dx * dx + dy * dy))
    aspect = np.arctan2(-dx, dy)
    az = np.deg2rad(azimuth)
    alt = np.deg2rad(altitude)
    shaded = np.sin(alt) * np.sin(slope) + np.cos(alt) * np.cos(slope) * np.cos(az - aspect)
    shaded = np.clip((shaded + 1) / 2, 0, 1)
    return shaded


def lonlat_to_px(
    lon: float,
    lat: float,
    extent: tuple[float, float, float, float],
    width: int,
    height: int,
) -> tuple[int, int]:
    xmin, ymin, xmax, ymax = extent
    x = int(round((lon - xmin) / (xmax - xmin) * width))
    y = int(round((ymax - lat) / (ymax - ymin) * height))
    return x, y


def feature_bbox(coords: list) -> tuple[float, float, float, float]:
    xs: list[float] = []
    ys: list[float] = []
    for ring in coords:
        for x, y in ring:
            xs.append(float(x))
            ys.append(float(y))
    return min(xs), min(ys), max(xs), max(ys)


def bbox_intersects(a: tuple[float, float, float, float], b: tuple[float, float, float, float]) -> bool:
    return not (a[2] < b[0] or a[0] > b[2] or a[3] < b[1] or a[1] > b[3])


def hex_to_rgba(value: str, alpha: int) -> tuple[int, int, int, int]:
    value = value.strip().lstrip("#")
    return (int(value[0:2], 16), int(value[2:4], 16), int(value[4:6], 16), alpha)


def draw_geojson_polygons(
    base: Image.Image,
    geojson_path: Path,
    extent: tuple[float, float, float, float],
    alpha: int,
    outline_alpha: int,
) -> int:
    data = json.loads(geojson_path.read_text(encoding="utf-8"))
    layer = Image.new("RGBA", base.size, (0, 0, 0, 0))
    draw = ImageDraw.Draw(layer)
    count = 0
    for feat in data["features"]:
        geom = feat["geometry"]
        if geom["type"] != "Polygon":
            continue
        rings = geom["coordinates"]
        if not bbox_intersects(feature_bbox(rings), extent):
            continue
        color = hex_to_rgba(feat["properties"]["color"], alpha)
        outline = hex_to_rgba(feat["properties"]["color"], outline_alpha)
        pixel_rings = [
            [lonlat_to_px(float(x), float(y), extent, base.width, base.height) for x, y in ring]
            for ring in rings
        ]
        if not pixel_rings or len(pixel_rings[0]) < 3:
            continue
        draw.polygon(pixel_rings[0], fill=color, outline=outline)
        # Carve holes when rings are present. Most extracted units do not have many
        # holes at the preview scale, but this keeps the renderer honest enough.
        for hole in pixel_rings[1:]:
            if len(hole) >= 3:
                draw.polygon(hole, fill=(0, 0, 0, 0), outline=(0, 0, 0, 0))
        count += 1
    return count if not base.alpha_composite(layer) else count


def add_legend(img: Image.Image, legend_csv: Path, max_items: int = 12) -> None:
    rows = []
    with legend_csv.open("r", encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            rows.append(row)
    rows = sorted(rows, key=lambda r: float(r.get("percent", 0)), reverse=True)[:max_items]
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    w, h = 300, 26 + len(rows) * 20
    x0, y0 = img.width - w - 16, 16
    draw.rounded_rectangle([x0, y0, x0 + w, y0 + h], radius=6, fill=(255, 255, 255, 218), outline=(50, 50, 50, 120))
    draw.text((x0 + 12, y0 + 8), "Extracted geology classes", fill=(30, 30, 30), font=font)
    for i, row in enumerate(rows):
        y = y0 + 30 + i * 20
        fill = hex_to_rgba(row["color"], 255)
        draw.rectangle([x0 + 12, y, x0 + 28, y + 12], fill=fill, outline=(80, 80, 80))
        label = f"{row['unit_id']}  {row['percent']}%"
        draw.text((x0 + 36, y - 1), label, fill=(25, 25, 25), font=font)


def render(layer: str) -> Path:
    dem_img, extent = load_geotiff_extent(DEM)
    z = np.asarray(dem_img, dtype=np.float32)
    rgb = terrain_rgb(z).astype(np.float32)
    hs = hillshade(z)
    rgb = rgb * (0.45 + 0.65 * hs[..., None])
    rgb = np.clip(rgb, 0, 255).astype(np.uint8)
    base = Image.fromarray(rgb, mode="RGB").convert("RGBA")

    if layer == "units":
        geojson = GEO_DIR / "geology_units_polygons_epsg4326.geojson"
        legend = GEO_DIR / "legend_units.csv"
        alpha = 128
    else:
        geojson = GEO_DIR / "geology_broad_groups_polygons_epsg4326.geojson"
        legend = GEO_DIR / "legend_broad_groups.csv"
        alpha = 118

    count = draw_geojson_polygons(base, geojson, extent, alpha=alpha, outline_alpha=70)
    add_legend(base, legend, max_items=12 if layer == "units" else 7)

    draw = ImageDraw.Draw(base)
    draw.rectangle([14, base.height - 42, 526, base.height - 14], fill=(255, 255, 255, 205))
    draw.text(
        (24, base.height - 34),
        f"DEM hillshade + extracted geology ({layer}); drawn polygons in DEM extent: {count}",
        fill=(25, 25, 25),
        font=ImageFont.load_default(),
    )

    OUT.mkdir(parents=True, exist_ok=True)
    out = OUT / f"geology_{layer}_over_dem_preview.png"
    base.convert("RGB").save(out, quality=95)
    return out


def main() -> None:
    print(render("units"))
    print(render("broad_groups"))


if __name__ == "__main__":
    main()
