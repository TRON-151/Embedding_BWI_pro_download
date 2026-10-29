##source("scripts/00_rlibraries.R")
#library(here)
#
## important paths
#dir_list <- list(
#    data = here::here("data") 
##    out_dir = here::here("output"),
##    out_regions = here::here("output", "regions"),
##    model_inputs = here::here("output", "model_inputs")
#)
#
#
#
#for (dir in dir_list){
#    if(!dir.exists(dir)){
#        dir.create(dir, recursive = TRUE)
#        message("created missing dir: ", dir)
#    }else{message("Found '", dir, "' path")}
#}
#
#aoi_utm_0 <- st_transform(st_read(file.path(dir_list$data, "/bbox/aoi_bbox_0.geojson")), 25832)
#
## regions <- names(params)[unlist(params)]
#
#regions <- c("region_1", "region_2")
#download_ALS(regions)
#
#
#
##if (params$build_data_from_base || length(list.files(dir_list$data)) == 0 ) {
##  source("scripts/02_download_data.R")
##}
#
#aoi_0 <- st_transform(st_read(file.path(root, "data/bbox/aoi_bbox_0.geojson")), 25832)
#
#
#
###############################################################################################
###############################################################################################
###############################################################################################
#
#library(sf)
#library(mapview)
#
## ---- Germany boundary ----
## Natural Earth 1:10m country polygons (sf reads the URL directly)
#vector_url <- "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson"
#world_sf <- st_read(vector_url)
#
#germany_sf    <- world_sf[world_sf$ADMIN == "Germany", ]
#germany_utm32 <- st_transform(germany_sf, crs = 25832)
#
## ---- AOI: Germany + 100 km buffer, clipped to land ----
#germany_buf <- st_buffer(germany_utm32, 100000)  # EPSG:25832 is in metres
#
## only the countries touching the buffer (transforming the whole world to UTM gives broken geometries)
#sf_use_s2(FALSE)
#neigh <- world_sf[lengths(st_intersects(world_sf, st_transform(germany_buf, 4326))) > 0, ]
#sf_use_s2(TRUE)
#
#land <- st_union(st_make_valid(st_transform(neigh, 25832)))
#aoi  <- st_intersection(st_geometry(germany_buf), land)
#
## keep only land connected to Germany (drops parts across water, e.g. Danish islands, Sweden)
#aoi_parts <- st_cast(aoi, "POLYGON")
#aoi       <- st_union(aoi_parts[lengths(st_intersects(aoi_parts, germany_utm32)) > 0])
#
#mapview(aoi, col.regions = "red", alpha.regions = 0.3, layer.name = "AOI") +
#    mapview(germany_utm32, alpha.regions = 0, layer.name = "Germany")
#
# ---- Embeddings (geotessera) ----
# TODO: --region-file expects a file path, so write the AOI to disk first
# gt_cli <- file.path(dirname(py_exe()), "geotessera")
# args   <- c("download", "--region-file", shQuote(aoi_file), "--year", 2022, "--output", dir_list$embd_dir)
# system2(gt_cli, args)


#######################################################################################
#######################################################################################
#######################################################################################


# testing
save_raster <- function(x, file, int8 = FALSE) {
  part_file <- paste0(file, ".part")
  if (int8) {
    writeRaster(x, part_file, filetype = "GTiff", datatype = "INT1S", NAflag = -128,
                gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  } else {
    writeRaster(x, part_file, filetype = "GTiff", datatype = "FLT4S",
                gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  }
  file.rename(part_file, file)
}