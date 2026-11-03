# BWI x Embed Project: makes the boxes / tiles and downloads the embeddings
#
# run from the project folder:  Rscript main.r
#
# AEF is slow (40 s to 2 min per box and year), most of it is waiting for the server.
# so for the full run start several parts at the same time, each one does one part of the areas.
# 8 parts in the terminal, with one log file per part:
#   for i in 1 2 3 4 5 6 7 8; do Rscript main.r $i 8 > output/log_part_$i.txt 2>&1 & done
# do one run with testing <- TRUE first, it saves the AEF index that all parts use
#
# first time on a new computer: renv::restore()

source(here::here("Scripts", "setup.r"))
source(here::here("Scripts", "areas.r"))
source(here::here("Scripts", "embeddings.r"))
source(here::here("Scripts", "download.r"))


# 1. important values, change these ###########################################

# boxes = one file per cluster for Thünen
# tiles = 10 x 10 km tiles for us
run_mode <- "boxes"
years <- c(2022)
embeddings <- c("olmo") # type only these strings "alphaEarth", "tessera", "olmo"
testing <- FALSE # if "boxes" then it will download only 10 files per embeddings per year and if run mode is "tiles" then 2 files per embeddings per year 

DE_buffer <- 100000 # buffer around Germany in meters (100 km)
target_crs <- "EPSG:25832"
pixel_size <- 10 # meters
tile_size <- 10000  # 10 km x 10 km
box_extra_margin <- 20  # meters

# FALSE = leave out the piles of overlapping boxes (I found some groups major ones near: Hachenburg, Kelheim, Eberswalde)
# TRUE = keep all boxes
overlap_boxes <- FALSE

# input files
csv_file <- here::here("data", "bwi2022_cluster_buffer.csv")

# this vector link is from a github "nvkelso"
vector_url <- "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson"

# Tessera embeddings version
tessera_ver <- "v1.1"

# AlphaEarth embeddings from source.coop
aef_https <- "https://data.source.coop/tge-labs/aef/v1/annual"
aef_index_file <- "aef_index.gpkg"

# OlmoEarth v1.3 embeddings from source.coop (zarr with zstd, needs the qs2 package)
olmo_url <- "https://data.source.coop/olmoearth/olmoearth-v1-3-embeddings/embeddings.zarr"

# out directories
dir_list <- list()
dir_list$out_dir <- here::here("output")
dir_list$cache_dir <- here::here("data", "cache")
dir_list$grid_dir <- file.path(dir_list$out_dir, "grid")
dir_list$boxes_dir <- file.path(dir_list$out_dir, "boxes")
dir_list$tiles_dir <- file.path(dir_list$out_dir, "tiles")

################################################################################


for (dir in dir_list) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("created missing dir: ", dir)
  }
}

# which part this run does, from "Rscript main.r 1 4". without numbers it does everything
args <- commandArgs(trailingOnly = TRUE)
part <- 1
n_parts <- 1
if (length(args) == 2) {
  part <- as.numeric(args[1])
  n_parts <- as.numeric(args[2])
}


# 2. AOI: Germany + buffer zone
world <- st_read(vector_url, quiet = TRUE)
DE <- world %>% filter(ADMIN == "Germany") %>% st_transform(target_crs)
DE_buf <- make_aoi(world, DE, DE_buffer, target_crs)

# 3. boxes for BWI, one box per cluster name
cluster_plots <- read_csv(csv_file, show_col_types = FALSE)
cluster_plots <- add_plot_ids(cluster_plots)
boxes <- make_boxes(cluster_plots, box_extra_margin, pixel_size, target_crs)

# finding the overlapping boxes 
boxes <- find_overlap_groups(boxes)
if (overlap_boxes) {
  dropped_boxes <- boxes[0, ]
} else {
  dropped_boxes <- boxes[boxes$group_size >= 3, ]
  boxes <- boxes[boxes$group_size < 3, ]
}
message(nrow(boxes), " boxes, ", nrow(dropped_boxes), " overlapping boxes removed")


# 4. the 10 x 10 km tiles for us
tiles <- make_tiles(DE_buf, boxes, tile_size, target_crs)


# saving, only by part 1 so that parallel runs don't write the same file
if (part == 1) {
  write_csv(cluster_plots, file.path(dir_list$grid_dir, "plot_ids.csv"))
  st_write(st_sf(geometry = DE_buf), file.path(dir_list$grid_dir, "aoi.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  st_write(DE["ADMIN"], file.path(dir_list$grid_dir, "germany.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  st_write(boxes, file.path(dir_list$grid_dir, "cluster_boxes.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  st_write(dropped_boxes, file.path(dir_list$grid_dir, "dropped_boxes.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  st_write(tiles, file.path(dir_list$grid_dir, "tiles_10km.gpkg"), delete_dsn = TRUE, quiet = TRUE)
}


# 5. embedding sources
aef_index <- get_aef_index(aef_https, aef_index_file, DE_buf, tile_size, target_crs, dir_list$cache_dir)

# tessera saves the downloaded pieces on disk
options(geotessera.cache_dir = file.path(dir_list$cache_dir, "tessera"))
gt <- tessera_open(tessera_store_url(tessera_ver, NULL))

# OlmoEarth has the same zarr layout as tessera, so geotessera opens it.
# cache only in memory: each piece is used for one box, a disk cache would only fill the disk
olmo <- NULL
if ("olmo" %in% embeddings) {
  olmo <- tessera_open(olmo_url, cache_dir = NULL)
}


# 6. download
if (run_mode == "boxes") {
  out_root <- dir_list$boxes_dir
} else {
  out_root <- dir_list$tiles_dir
}

# makes region
areas <- make_area_list(run_mode, boxes, tiles)
if (testing) {
  areas <- pick_test_areas(areas, run_mode)
}

# only the part of this run, in one piece of the list
part_number <- ceiling(1:nrow(areas) / nrow(areas) * n_parts)
areas <- areas[part_number == part, ]
message("run_mode: ", run_mode, ", part ", part, " of ", n_parts, ", areas to do: ", nrow(areas))

# one time file per part, so parallel runs don't write into the same file
time_file <- file.path(out_root, paste0("download_times_part_", part, ".csv"))

start_time <- Sys.time()
run_download(areas, years, embeddings, out_root, aef_index, gt, olmo, pixel_size, target_crs, time_file)
total_minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
message("total time: ", round(total_minutes, 1), " minutes")


# 7. checking files and the readme for Thünen
check_downloads(out_root, years, embeddings, pixel_size)

if (run_mode == "boxes" && part == 1) {
  write_readme(out_root, years, box_extra_margin, nrow(dropped_boxes), tessera_ver)
  file.copy(file.path(dir_list$grid_dir, "cluster_boxes.gpkg"), out_root, overwrite = TRUE)
  file.copy(file.path(dir_list$grid_dir, "dropped_boxes.gpkg"), out_root, overwrite = TRUE)
}
