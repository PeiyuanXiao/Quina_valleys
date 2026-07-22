## 06_geology_classified.R — 6-class lithology map EXTRACTED FROM THE 1:200,000
## scan, rendered to resemble a published geological sheet:
##   * geology = a class raster (values 1-6) -> recolour via geo_cols
##   * RELIEF SHADING via HSL: hillshade modulates the L (lightness) of each
##     pixel's geology colour (not a grey overlay), so colours stay vivid
##   * clean VECTOR unit boundaries (as.polygons -> st_simplify) drawn as thin grey
##   * palette desaturated one notch for a print-geology feel
##
## Class scheme follows reviewer guidance (lithology-based; D is NOT lower Palaeozoic):
##   1 Quaternary alluvium & terraces            (Qh, Qp)            pale yellow
##   2 Permian Emeishan basalt (Pβ)              green
##   3 Mesozoic–Paleogene clastic (T,J,K,E)      pink/tan
##   4 Carbonate rocks (limestone & dolomite)    grey / blue-grey
##   5 Palaeozoic clastic–carbonate (O–D, loc.C) rust / brick — incl. the
##       Devonian/Carboniferous folded belt SW of Binchuan (was wrongly in cl.3)
##   6 Precambrian metamorphic basement          dark brown
## HONESTY: boundaries APPROXIMATE; D/C vs Triassic in the south share colours and
## are partly separated by a hand-set spatial override (see DC_override below).
## Source sheet scale is 1:200,000 (confirmed on the sheet & legend).
## -----------------------------------------------------------------------------

library(sf); library(dplyr); library(readxl)
library(ggplot2); library(terra); library(tidyterra); library(ggspatial)
sf::sf_use_s2(FALSE)
proj_dir   <- here::here()
output_dir <- file.path(proj_dir, "output", "maps")
cache_dir  <- file.path(proj_dir, "data", "cache")

## ===========================================================================
## ADJUSTABLE PARAMETERS
## ===========================================================================
z_exag     <- 1.8                 # hillshade vertical exaggeration
sun_angle  <- 35                  # sun elevation (deg)
sun_dirs   <- c(300, 337, 15)     # multi-light azimuths, averaged
relief     <- 0.42                # 0=flat colour, ~0.5 strong relief (HSL-L gain)
desat      <- 0.78                # palette saturation multiplier (<1 = softer)
simplify_m <- 90                  # boundary simplification tolerance (metres)
sieve_cells<- 90                  # drop geology patches smaller than this (despeckle)
bound_col  <- "grey38"; bound_lwd <- 0.12
show_sites <- TRUE
geo_levels <- c(
  "Quaternary alluvium & terraces",
  "Permian Emeishan basalt (Pβ)",
  "Mesozoic–Paleogene clastic (T,J,K,E)",
  "Carbonate rocks (limestone & dolomite)",
  "Palaeozoic clastic–carbonate (O–D, locally C)",
  "Precambrian metamorphic basement")
geo_cols0 <- c("#EBD79B", "#6F9A57", "#C77B53", "#8FB0BE", "#C98A45", "#9A6FA0")
basin_cols <- c(Binchuan = "#8E2F39", Heqing = "#23506E"); label_col <- "#241F1A"

## ---- HSL helpers (vectorised) ----------------------------------------------
rgb2hsl <- function(r, g, b) {
  mx <- pmax(r,g,b); mn <- pmin(r,g,b); l <- (mx+mn)/2; d <- mx-mn
  s <- ifelse(d == 0, 0, d/(1 - abs(2*l - 1)))
  h <- ifelse(d == 0, 0,
        ifelse(mx == r, ((g-b)/d) %% 6,
        ifelse(mx == g, ((b-r)/d) + 2, ((r-g)/d) + 4))) / 6
  list(h = h %% 1, s = s, l = l)
}
hsl2rgb <- function(h, s, l) {
  c <- (1 - abs(2*l - 1))*s; hp <- h*6; x <- c*(1 - abs(hp %% 2 - 1))
  r <- g <- b <- numeric(length(h))
  i <- hp < 1;           r[i]<-c[i]; g[i]<-x[i]
  i <- hp>=1 & hp<2;     r[i]<-x[i]; g[i]<-c[i]
  i <- hp>=2 & hp<3;     g[i]<-c[i]; b[i]<-x[i]
  i <- hp>=3 & hp<4;     g[i]<-x[i]; b[i]<-c[i]
  i <- hp>=4 & hp<5;     r[i]<-x[i]; b[i]<-c[i]
  i <- hp>=5;            r[i]<-c[i]; b[i]<-x[i]
  m <- l - c/2; list(r = r+m, g = g+m, b = b+m)
}
desat_hex <- function(hex, f) { x <- rgb2hsv(col2rgb(hex)); hsv(x[1], pmin(1, x[2]*f), x[3]) }
geo_cols  <- setNames(vapply(geo_cols0, desat_hex, "", f = desat), geo_levels)

## ===========================================================================
## (A) GEOREFERENCE + CROP
## ===========================================================================
left_x<-1176; right_x<-5059; top_y<-19; bottom_y<-5728
lon_W<-100; lon_E<-101; lat_N<-26+40/60; lat_S<-25+20/60
scan <- rast(file.path(proj_dir, "data", "geology_scan.jpg"))
nc<-ncol(scan); nr<-nrow(scan)
lon_at<-function(x) lon_W+(x-left_x)/(right_x-left_x)*(lon_E-lon_W)
lat_at<-function(y) lat_N+(y-top_y)/(bottom_y-top_y)*(lat_S-lat_N)
ext(scan)<-c(lon_at(0),lon_at(nc),lat_at(nr),lat_at(0)); crs(scan)<-"EPSG:4326"
names(scan)<-c("R","G","B")
dem <- rast(file.path(cache_dir, "dem.tif"))
bb  <- ext(dem); bbx <- as.numeric(as.vector(bb))
sc  <- crop(scan, bb)

## ===========================================================================
## (B) CLASSIFY by reference colours (K=30 cluster means, ground-truthed)
## ===========================================================================
ref <- rbind(
  c(213,193,109,1),c(178,157,91,1),c(207,189,122,1),c(210,188,91,1),c(157,136,73,1),c(191,174,109,1),
  c(130,131,78,2),c(101,94,49,2),c(147,150,95,2),c(110,109,59,2),c(132,117,56,2),c(137,141,87,2),
    c(119,120,71,2),c(155,161,104,2),c(150,159,84,2),
  c(144,108,85,3),c(198,159,134,3),c(163,127,103,3),c(179,144,118,3),c(208,171,145,3),c(141,124,108,3),
  c(128,141,124,4),c(98,103,85,4),c(151,155,134,4),c(116,118,101,4),c(175,178,154,4),
  c(182,108,66,5),c(156,78,49,5),
  c(119,91,72,6),c(110,67,40,6))
R<-values(sc[["R"]]); G<-values(sc[["G"]]); B<-values(sc[["B"]]); n<-length(R)
mxx<-pmax(R,G,B); mnn<-pmin(R,G,B); sat<-ifelse(mxx==0,0,(mxx-mnn)/mxx)
na_mask <- (mxx<115)|(mxx>238&sat<0.08)|(B>R+22&B>G+8&B>95)|is.na(R)
best<-rep(Inf,n); cls<-rep(NA_integer_,n)
for(k in seq_len(nrow(ref))){ d<-(R-ref[k,1])^2+(G-ref[k,2])^2+(B-ref[k,3])^2
  b<-!is.na(d)&d<best; best[b]<-d[b]; cls[b]<-ref[k,4] }
cls[na_mask]<-NA
cr <- sc[[1]]; values(cr)<-cls

## ---- spatial override: SW Devonian–Carboniferous belt (read off the sheet) --
## within this zone, warm pixels classed as Mesozoic clastic (3) are really D/C (5)
DC_override <- st_sfc(st_polygon(list(rbind(
  c(100.310,25.955), c(100.405,25.945), c(100.470,25.890), c(100.495,25.815),
  c(100.455,25.735), c(100.345,25.735), c(100.310,25.820), c(100.310,25.955)))), crs=4326)
dcz <- rasterize(vect(DC_override), cr, field=1, background=0)
v <- values(cr); inzone <- values(dcz)==1
v[!is.na(v) & v==3 & inzone] <- 5            # T/J/K-coloured -> Palaeozoic D/C
values(cr) <- v

## ---- heal masked pixels + modal-smooth into blocks --------------------------
for(i in 1:12){ if(!any(is.na(values(cr)))) break
  cr <- focal(cr, w=5, fun="modal", na.policy="only", na.rm=TRUE) }
cr <- focal(cr, w=7, fun="modal", na.rm=TRUE)
cr <- round(focal(cr, w=5, fun="modal", na.rm=TRUE))
cr <- sieve(cr, threshold = sieve_cells, directions = 8)   # absorb tiny specks/slivers

## ===========================================================================
## (C) VECTORISE -> SIMPLIFY -> re-rasterise (fills & boundaries stay in sync)
## ===========================================================================
polys <- as.polygons(cr); polys_sf <- st_as_sf(polys)
names(polys_sf)[1] <- "cls"
polys_sf <- polys_sf |> st_make_valid() |>
  st_transform(32647) |> st_simplify(dTolerance = simplify_m, preserveTopology = TRUE) |>
  st_make_valid() |> st_transform(4326)
polys_sf <- polys_sf[!st_is_empty(polys_sf), ]
crr <- rasterize(vect(polys_sf), cr, field = "cls")

## ===========================================================================
## (D) HILLSHADE (DEM) -> resample to the geology grid
## ===========================================================================
dem_z<-dem*z_exag
slope_r<-terra::terrain(dem_z,"slope",unit="radians")
aspect_r<-terra::terrain(dem_z,"aspect",unit="radians")
hl<-lapply(sun_dirs,function(d) terra::shade(slope_r,aspect_r,angle=sun_angle,direction=d))
hill<-terra::app(terra::rast(hl),mean)
hr<-as.numeric(stats::quantile(values(hill,mat=FALSE),c(0.02,0.98),na.rm=TRUE))
hill<-(clamp(hill,hr[1],hr[2])-hr[1])/(hr[2]-hr[1])
hill_rs<-resample(hill,crr,method="bilinear")

## ===========================================================================
## (E) RELIEF-SHADE the geology in HSL L-channel  -> shaded RGB raster
## ===========================================================================
clsv <- values(crr)[,1]
hh   <- values(hill_rs)[,1]; hh[is.na(hh)] <- 0.5
basehex <- geo_cols[clsv]                       # NA where clsv NA
rgbm <- col2rgb(ifelse(is.na(basehex), "#000000", basehex))/255
hsl  <- rgb2hsl(rgbm[1,], rgbm[2,], rgbm[3,])
Lfac <- 1 + relief*(2*hh - 1)                   # >1 lit slopes, <1 shaded
Ln   <- pmin(1, pmax(0, hsl$l * Lfac))
o    <- hsl2rgb(hsl$h, hsl$s, Ln)
RR<-o$r*255; GG<-o$g*255; BB<-o$b*255
bad <- is.na(clsv); RR[bad]<-NA; GG[bad]<-NA; BB[bad]<-NA
shaded <- rast(crr, nlyr=3); values(shaded)<-cbind(RR,GG,BB)
shaded <- clamp(shaded,0,255); names(shaded)<-c("r","g","b")
writeRaster(crr, file.path(cache_dir,"geology_classes.tif"), overwrite=TRUE)

## ===========================================================================
## (F) SITES + HULLS
## ===========================================================================
sites <- readxl::read_excel(file.path(proj_dir,"data","Site_information.xlsx"))
names(sites)<-trimws(names(sites))
sites <- sites |> rename(code=Code) |> filter(!code %in% c("PJDD","ZKZ")) |>
  mutate(basin=factor(sub(" basin$","",trimws(basin)),levels=c("Binchuan","Heqing"))) |>
  st_as_sf(coords=c("lon","lat"),crs=4326,remove=FALSE)
anchor <- subset(sites, code %in% c("LT","THC"))
hulls <- sites |> group_by(basin) |> summarise(geometry=st_combine(geometry),.groups="drop") |>
  st_convex_hull() |> st_transform(32647) |> st_buffer(900) |> st_transform(4326)

## off-canvas dummy for a discrete geology legend (fills come from the RGB raster)
key_df <- st_sf(unit=factor(geo_levels,levels=geo_levels),
  geometry=st_sfc(lapply(seq_along(geo_levels),function(i) st_point(c(lon_W-2,lat_S-2))),crs=4326))

## ===========================================================================
## (G) RENDER
## ===========================================================================
p <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = shaded, maxcell = 6e6) +
  geom_sf(data = polys_sf, fill = NA, color = bound_col, linewidth = bound_lwd) +
  geom_sf(data = key_df, aes(fill = unit), shape = 22, size = 0, color = NA) +
  scale_fill_manual(values = geo_cols, breaks = geo_levels, drop = FALSE,
                    name = "Geology (from 1:200k sheet,\nclasses approximate)",
                    guide = guide_legend(order = 1,
                            override.aes = list(size = 4.2, alpha = 1, color = "grey35"))) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = hulls, aes(color = basin), fill = NA, linewidth = 0.55,
          linetype = "22", show.legend = FALSE)

if (show_sites)
  p <- p +
    geom_sf(data = sites, aes(fill = basin), size = 2.7, shape = 21,
            color = "white", stroke = 0.5, alpha = 0.98) +
    geom_sf(data = anchor, shape = 8, size = 5.0, color = "white", stroke = 1.5) +
    geom_sf(data = anchor, shape = 8, size = 3.9, color = label_col, stroke = 0.9) +
    scale_fill_manual(values = basin_cols, name = "Basin",
                      guide = guide_legend(order = 2, override.aes = list(shape = 21, size = 3))) +
    scale_color_manual(values = basin_cols, guide = "none") +
    ggrepel::geom_text_repel(data = sites, aes(geometry = geometry, label = code),
      stat = "sf_coordinates", size = 2.4, fontface = "bold", color = label_col,
      bg.color = "white", bg.r = 0.15, max.overlaps = 30,
      min.segment.length = 0, segment.color = "grey55", segment.size = 0.2)

p <- p +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),
                         height = unit(1.05, "cm"), width = unit(1.05, "cm")) +
  coord_sf(xlim = bbx[1:2], ylim = bbx[3:4], expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Geology of the Binchuan and Heqing basins",
       subtitle = "6 lithological classes from the 1:200,000 sheet (Yunnan Geol. Bureau, 1973); HSL relief shading from SRTM",
       caption = paste0(
         "Geology classified from the georeferenced 1:200,000 sheet — boundaries APPROXIMATE (source coloured by age). ",
         "The SW Devonian–Carboniferous\nfolded belt is separated from Mesozoic clastics by a hand-set zone. ",
         "Stars = LT (Longtan), THC (Tianhua Cave). Lighting/colours editable atop the script.")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        panel.grid = element_line(color = grey(0.88), linewidth = 0.15),
        legend.title = element_text(size = 8.5), legend.text = element_text(size = 8),
        plot.subtitle = element_text(size = 9, color = "grey25"),
        plot.caption  = element_text(size = 7, hjust = 0))

ggsave(file.path(output_dir, "map_geology_scan6.png"), p, width = 9.3, height = 8, dpi = 320)
message("06_geology_classified.R done -> output/map_geology_scan6.(pdf|png)")
