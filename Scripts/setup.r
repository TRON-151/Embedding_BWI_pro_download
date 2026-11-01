# packages and GDAL settings, used by main.r and the notebook

if (getRversion() < "4.4.0") stop("R 4.4 or newer is needed")
if (packageVersion("zarr") != "0.5.1") stop("zarr is not 0.5.1, run renv::restore() first")

# GDAL settings, so terra can read the AEF files from source.coop
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
Sys.setenv(CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.tiff,.gpkg")
Sys.setenv(GDAL_HTTP_MAX_RETRY = "5", GDAL_HTTP_RETRY_DELAY = "3")
# don't set GDAL_NUM_THREADS, terra got stuck with it in my test

library(here)
library(sf)
library(terra)
library(dplyr)
library(readr)
library(geotessera)

terraOptions(progress = 0)
