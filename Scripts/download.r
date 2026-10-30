# testing function for saving raster
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

# function for counting the number of box will be cut on different level of fixed grid.
count_cut_boxes <- function(size, step = 100) {
  shifts <- seq(0, size - step, by = step)

  # best shift for the vertical edges
  cut_x <- c()
  for (s in shifts) {
    col_left  <- floor((boxes$xmin - s) / size)
    col_right <- floor((boxes$xmax - 1 - s) / size)     # -1 m: a box that only touches an edge is not cut
    cut_x <- c(cut_x, sum(col_left != col_right))
  }
  best_x <- shifts[which.min(cut_x)]

  # best shift for the horizontal edges
  cut_y <- c()
  for (s in shifts) {
    row_bottom <- floor((boxes$ymin - s) / size)
    row_top    <- floor((boxes$ymax - 1 - s) / size)
    cut_y <- c(cut_y, sum(row_bottom != row_top))
  }
  best_y <- shifts[which.min(cut_y)]

  # boxes cut by the best grid of this size
  cut <- floor((boxes$xmin - best_x) / size) != floor((boxes$xmax - 1 - best_x) / size) |
         floor((boxes$ymin - best_y) / size) != floor((boxes$ymax - 1 - best_y) / size)
  sum(cut)
}

##################################################################################

# the UTM zone of every cell of a template, from the longitude of the cell centre.
# used on zone borders, so that every cell takes its value from its own zone
cell_zones <- function(template) {
  lon <- project(xyFromCell(template, 1:ncell(template)), crs(template), "EPSG:4326")[, 1]
  floor((lon + 180) / 6) + 1
}

##################################################################################

# function to load tessera embeddings 
get_tessera <- function(template, year) {
  cell_zone <- cell_zones(template)

  own_zone <- list() # piece where the cell lies in its zone
  any_zone <- list() # piece everywhere it has data
  for (z in unique(cell_zone)) {
    area_z <- st_transform(st_buffer(st_as_sfc(st_bbox(template)), 20), paste0("EPSG:326", z))
    r <- tessera_region(gt, area_z, year)
    piece <- project(r, template, method = "near") # project on our grid
    any_zone[[length(any_zone) + 1]] <- piece
    piece[which(cell_zone != z)] <- NA
    own_zone[[length(own_zone) + 1]] <- piece
  }
  out <- cover(merge(sprc(own_zone)), merge(sprc(any_zone)))
  names(out) <- sprintf("tes_%03d", 0:(nlyr(out) - 1))
  out
}


###############################################################################

# function to load Alpha Earth Embeddings
get_aef <- function(template, year) {
  area_ll <- st_transform(st_as_sfc(st_bbox(template)), 4326)
  files   <- aef_index[aef_index$year == year, ]
  files   <- files[st_intersects(files, area_ll, sparse = FALSE)[, 1], ]
  if (nrow(files) == 0) {
    message("  no AEF file for this area")
    return(NULL)
  }

  cell_zone <- cell_zones(template)
  own_zone <- list() 
  any_zone <- list()    
  for (u in files$url) {
    src <- rast(u)   
    z <- as.numeric(crs(src, describe = TRUE)$code) %% 100 
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

  out <- cover(merge(sprc(own_zone)), merge(sprc(any_zone)))
  names(out) <- sprintf("aef_%02d", 0:(nlyr(out) - 1))
  out
}