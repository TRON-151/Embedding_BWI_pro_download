# functions to read the AlphaEarth (AEF), TESSERA and OlmoEarth embeddings for one area

# writes a .part file first and renames it at the end, so a half written file is never taken as done
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

##################################################################################

# UTM zone of every cell, from the longitude of the cell centre
cell_zones <- function(template) {
  lon <- project(xyFromCell(template, 1:ncell(template)), crs(template), "EPSG:4326")[, 1]
  floor((lon + 180) / 6) + 1
}

##################################################################################

# AEF file list for the AOI + one tile size, saved in the cache folder after the first time
get_aef_index <- function(aef_https, aef_index_file, DE_buf, tile_size, crs, cache_dir) {
  aef_base <- paste0("/vsicurl/", aef_https)
  aef_index_saved <- file.path(cache_dir, "aef_index_aoi.gpkg")

  if (!file.exists(aef_index_saved)) {
    aef_index_online <- paste0(aef_base, "/", aef_index_file)
    layer_name <- st_layers(aef_index_online)$name[1]

    b <- st_bbox(DE_buf)
    b <- st_bbox(c(xmin = b[["xmin"]] - tile_size, ymin = b[["ymin"]] - tile_size,
                   xmax = b[["xmax"]] + tile_size, ymax = b[["ymax"]] + tile_size), crs = st_crs(crs))

    # a point every 1 km on the edges, as the edges are a bit curved in lon / lat
    b_lonlat <- st_transform(st_segmentize(st_as_sfc(b), 1000), 4326)
    our_area <- st_as_text(st_as_sfc(st_bbox(b_lonlat)))

    aef_index <- st_read(aef_index_online, layer = layer_name, wkt_filter = our_area, quiet = TRUE)
    st_write(aef_index, aef_index_saved, quiet = TRUE)
  }
  aef_index <- st_read(aef_index_saved, quiet = TRUE)

  # column with the file names (values end with .tiff)
  path_col <- NA
  for (col in names(aef_index)) {
    if (is.character(aef_index[[col]]) && any(grepl("tiff$", aef_index[[col]]))) {
      path_col <- col
    }
  }

  # "<year>/<zone>/<file>.tiff" with the https address in front
  aef_index$url <- file.path(aef_base, sub(".*/annual/", "", aef_index[[path_col]]))
  aef_index
}

##################################################################################

# AEF for one template, values stay int8 (-128 = no data)
get_aef <- function(template, year, aef_index) {
  area_ll <- st_transform(st_as_sfc(st_bbox(template)), 4326)
  files <- aef_index[aef_index$year == year, ]
  files <- files[st_intersects(files, area_ll, sparse = FALSE)[, 1], ]
  if (nrow(files) == 0) {
    message("  no AEF file for this area")
    return(NULL)
  }

  cell_zone <- cell_zones(template)
  own_zone <- list()   # only cells that lie in the zone of the file
  any_zone <- list()   # every cell with data
  for (u in files$url) {
    src <- rast(u)
    z <- as.numeric(crs(src, describe = TRUE)$code) %% 100    # 32632 -> 32
    e <- project(ext(template), from = crs(template), to = crs(src))
    e <- terra::intersect(extend(e, 100), ext(src))
    if (is.null(e)) next
    piece <- crop(src, e, snap = "out")   # downloads only this part
    piece <- project(piece, template, method = "near")
    piece[piece == -128] <- NA
    any_zone[[length(any_zone) + 1]] <- piece
    piece[which(cell_zone != z)] <- NA
    own_zone[[length(own_zone) + 1]] <- piece
  }
  if (length(any_zone) == 0) return(NULL)

  # own zone first, the other zone only where the own zone has no data
  out <- cover(merge(sprc(own_zone)), merge(sprc(any_zone)))
  names(out) <- sprintf("aef_%02d", 0:(nlyr(out) - 1))
  out
}

##################################################################################

# TESSERA for one template, gt is the store from tessera_open()
get_tessera <- function(template, year, gt) {
  cell_zone <- cell_zones(template)

  own_zone <- list()
  any_zone <- list()
  for (z in unique(cell_zone)) {
    # area in the CRS of zone z, so we get the pixels of zone z
    area_z <- st_transform(st_buffer(st_as_sfc(st_bbox(template)), 20), paste0("EPSG:326", z))
    r <- tessera_region(gt, area_z, year)
    piece <- project(r, template, method = "near")
    any_zone[[length(any_zone) + 1]] <- piece
    piece[which(cell_zone != z)] <- NA
    own_zone[[length(own_zone) + 1]] <- piece
  }
  out <- cover(merge(sprc(own_zone)), merge(sprc(any_zone)))
  names(out) <- sprintf("tes_%03d", 0:(nlyr(out) - 1))
  out
}

##################################################################################

# OlmoEarth for one area in the CRS of zone z, values stay int8 (NA = no data).
# the store has the same layout as TESSERA (utmNN groups, time/band/y/x, sharded int8),
# so tessera_open() can open it. but tessera_region() needs a "scales" array that
# OlmoEarth does not have, so we use the reader inside geotessera (the ::: functions, their version is fixed by renv)

olmo_region <- function(olmo, area_z, year, z) {
  zone <- geotessera:::zone_open(olmo, z)
  if (is.null(zone)) return(NULL)
  ti <- match(year, zone$years) - 1   # 0 based position of the year
  if (is.na(ti)) stop("no OlmoEarth data for ", year)
  reader <- geotessera:::zone_reader(olmo, zone, "embeddings")

  # pixel window, 0 based, start is included and end is not
  b <- st_bbox(area_z)
  col0 <- max(0, floor((b[["xmin"]] - zone$ox) / zone$px))
  col1 <- min(reader$shape[4], ceiling((b[["xmax"]] - zone$ox) / zone$px))
  row0 <- max(0, floor((zone$oy - b[["ymax"]]) / zone$px))
  row1 <- min(reader$shape[3], ceiling((zone$oy - b[["ymin"]]) / zone$px))

  # downloads only the 256 x 256 chunks that are needed, comes back as [x, y, band, time]
  v <- geotessera:::read_block(reader, c(ti, 0, row0, col0), c(ti + 1, reader$shape[2], row1, col1))
  dim(v) <- dim(v)[1:3]
  v[v == -128] <- NA

  # terra wants [y, x, band]
  rast(aperm(v, c(2, 1, 3)), crs = zone$crs,
       extent = ext(zone$ox + col0 * zone$px, zone$ox + col1 * zone$px,
                    zone$oy - row1 * zone$px, zone$oy - row0 * zone$px))
}

##################################################################################

# OlmoEarth for one template, olmo is the store from tessera_open(olmo_url).
# same steps as get_tessera()
get_olmo <- function(template, year, olmo) {
  cell_zone <- cell_zones(template)

  own_zone <- list()
  any_zone <- list()
  for (z in unique(cell_zone)) {
    # area in the CRS of zone z, so we get the pixels of zone z
    area_z <- st_transform(st_buffer(st_as_sfc(st_bbox(template)), 20), paste0("EPSG:326", z))
    r <- olmo_region(olmo, area_z, year, z)
    if (is.null(r)) next
    piece <- project(r, template, method = "near")
    any_zone[[length(any_zone) + 1]] <- piece
    piece[which(cell_zone != z)] <- NA
    own_zone[[length(own_zone) + 1]] <- piece
  }
  if (length(any_zone) == 0) return(NULL)

  # own zone first, the other zone only where the own zone has no data
  out <- cover(merge(sprc(own_zone)), merge(sprc(any_zone)))
  names(out) <- sprintf("olm_%03d", 0:(nlyr(out) - 1))
  out
}
