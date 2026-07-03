## 07_illustrator_layers.R — export aligned layers for hand-drawing the geology in
## Illustrator over a clean GREYSCALE HILLSHADE base.
##
## Outputs (all share the EXACT same pixel grid & extent -> they overlay perfectly):
##   output/ai_hillshade_grey.png  greyscale relief base (place at the bottom)
##   output/ai_scan_reference.png  georeferenced 1:200k scan, same frame (trace from this)
##   output/ai_hillshade_grey.tif  georeferenced GeoTIFF (if you georeference in GIS)
## Workflow in AI: place both at the same size; lock the scan as a reference layer,
## trace lithology polygons on a new layer; set that layer to MULTIPLY over the grey
## hillshade -> instant relief shading through your colours (same idea as HSL).
## Lighting knobs match 04/06.
## -----------------------------------------------------------------------------
library(terra)
proj_dir <- "H:/Quina_valleys"; output_dir <- file.path(proj_dir, "output", "maps")
cache_dir <- file.path(proj_dir, "data", "cache")

## -- adjustable lighting --
z_exag <- 1.8; sun_angle <- 35; sun_dirs <- c(300, 337, 15)
upsample <- 1.5            # render finer than the scan grid for crisp tracing

## ---- georeference scan, crop to study area (DEM extent) ---------------------
left_x<-1176; right_x<-5059; top_y<-19; bottom_y<-5728
lon_W<-100; lon_E<-101; lat_N<-26+40/60; lat_S<-25+20/60
scan <- rast(file.path(proj_dir,"data","geology_scan.jpg")); nc<-ncol(scan); nr<-nrow(scan)
lon_at<-function(x) lon_W+(x-left_x)/(right_x-left_x)*(lon_E-lon_W)
lat_at<-function(y) lat_N+(y-top_y)/(bottom_y-top_y)*(lat_S-lat_N)
ext(scan)<-c(lon_at(0),lon_at(nc),lat_at(nr),lat_at(0)); crs(scan)<-"EPSG:4326"; names(scan)<-c("R","G","B")
dem <- rast(file.path(cache_dir,"dem.tif")); bb <- ext(dem)
sc  <- crop(scan, bb)

## common target grid (scan grid, optionally upsampled) so all layers align
templ <- sc[[1]]
if (upsample != 1) templ <- disagg(templ, fact = upsample, method = "near")
sc_r  <- resample(sc, templ, method = "bilinear")

## ---- hillshade from DEM, onto the same grid ---------------------------------
dem_z<-dem*z_exag
slope_r<-terra::terrain(dem_z,"slope",unit="radians")
aspect_r<-terra::terrain(dem_z,"aspect",unit="radians")
hl<-lapply(sun_dirs,function(d) terra::shade(slope_r,aspect_r,angle=sun_angle,direction=d))
hill<-terra::app(terra::rast(hl),mean)
hr<-as.numeric(stats::quantile(values(hill,mat=FALSE),c(0.02,0.98),na.rm=TRUE))
hill<-(clamp(hill,hr[1],hr[2])-hr[1])/(hr[2]-hr[1])
hill_rs<-resample(hill, templ, method="bilinear")
grey255 <- round(clamp(hill_rs,0,1)*255)

## ---- write layers -----------------------------------------------------------
writeRaster(grey255, file.path(output_dir,"ai_hillshade_grey.png"),
            datatype="INT1U", overwrite=TRUE)
writeRaster(grey255, file.path(output_dir,"ai_hillshade_grey.tif"),
            datatype="INT1U", overwrite=TRUE)               # georeferenced copy
writeRaster(clamp(sc_r,0,255), file.path(output_dir,"ai_scan_reference.png"),
            datatype="INT1U", overwrite=TRUE)

cat(sprintf("grid: %d x %d px  (extent lon %.4f..%.4f, lat %.4f..%.4f)\n",
            nrow(templ), ncol(templ), bb[1], bb[2], bb[3], bb[4]))
message("07_illustrator_layers.R done -> output/ai_hillshade_grey.png/.tif, ai_scan_reference.png")
