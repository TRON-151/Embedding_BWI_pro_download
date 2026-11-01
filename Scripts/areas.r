# functions for the AOI, the cluster boxes and the 10 x 10 km tiles
################################################################
# Germany + buffer, only the land part that touches Germany
make_aoi <- function(world, DE, buffer, crs) {
  DE_buf <- st_buffer(DE, buffer)

  # only the countries which touch the buffer
  sf_use_s2(FALSE)
  neigh <- world[lengths(st_intersects(world, st_transform(DE_buf, 4326))) > 0, ]
  sf_use_s2(TRUE)

  land <- st_union(st_make_valid(st_transform(neigh, crs)))
  aoi <- st_intersection(st_geometry(DE_buf), land)
  if (any(st_geometry_type(aoi) == "GEOMETRYCOLLECTION")) {
    aoi <- st_collection_extract(aoi, "POLYGON")
  }

  aoi_parts <- st_cast(aoi, "POLYGON")
  st_union(aoi_parts[lengths(st_intersects(aoi_parts, DE)) > 0])
}

##################################################################################

buffer_plot <- function(DE, DE_buf) {
  plot(st_geometry(DE_buf), col = "lightblue", border = "blue", main = "DE Geometry with buffered zone")
  plot(st_geometry(DE), col = "orange", border = "darkred", add = TRUE)

  legend("topright",
         legend = c("Buffer (DE_buf)", "Original (DE)"),
         fill = c("lightblue", "orange"),
         border = c("blue", "darkred"))
}

##################################################################################

# plot id for every row, as one cluster name has at most 4 rows
add_plot_ids <- function(cluster_plots) {
  cluster_plots %>%
    group_by(cluster_name) %>%
    mutate(plot_id = paste0(cluster_name, "_", row_number())) %>%
    ungroup()
}

##################################################################################

# one box around all rows of a cluster, with a margin, on the 10 m grid
make_boxes <- function(cluster_plots, margin, pixel_size, crs) {
  boxes <- cluster_plots %>%
    group_by(cluster_name) %>%
    summarise(n_rows = n(),
              xmin = min(ll_x), ymin = min(ll_y),
              xmax = max(ur_x), ymax = max(ur_y))

  # margin, and out to the next 10 m line
  boxes$xmin <- floor((boxes$xmin - margin) / pixel_size) * pixel_size
  boxes$ymin <- floor((boxes$ymin - margin) / pixel_size) * pixel_size
  boxes$xmax <- ceiling((boxes$xmax + margin) / pixel_size) * pixel_size
  boxes$ymax <- ceiling((boxes$ymax + margin) / pixel_size) * pixel_size

  # size in pixels
  boxes$ncol <- (boxes$xmax - boxes$xmin) / pixel_size
  boxes$nrow <- (boxes$ymax - boxes$ymin) / pixel_size

  boxes$wkt <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                       boxes$xmin, boxes$ymin, boxes$xmax, boxes$ymin, boxes$xmax, boxes$ymax,
                       boxes$xmin, boxes$ymax, boxes$xmin, boxes$ymin)
  boxes <- st_as_sf(boxes, wkt = "wkt", crs = crs)

  # UTM zone of the box centre
  centres <- st_transform(st_centroid(st_geometry(boxes)), 4326)
  boxes$utm_zone <- floor((st_coordinates(centres)[, 1] + 180) / 6) + 1
  boxes
}

##################################################################################

# group_size: 1 = box overlaps no other box, 2 = a pair, 3 or more = a pile of boxes
find_overlap_groups <- function(boxes) {
  # "2********" = the insides overlap, only touching at an edge is not counted
  overlaps <- st_relate(boxes, boxes, pattern = "2********")

  # every box takes the smallest group number of the boxes it overlaps, until nothing changes
  group <- 1:nrow(boxes)
  repeat {
    new_group <- sapply(overlaps, function(i) min(group[i]))
    if (all(new_group == group)) break
    group <- new_group
  }
  boxes$overlap_group <- group
  boxes$group_size <- as.numeric(table(group)[as.character(group)])
  boxes
}

##################################################################################

# tiles start at a multiple of 10 km, so every tile edge is also on our 10 m grid
make_tiles <- function(DE_buf, boxes, tile_size, crs) {
  area <- st_bbox(DE_buf)
  x0 <- floor(min(area["xmin"], boxes$xmin) / tile_size) * tile_size
  y0 <- floor(min(area["ymin"], boxes$ymin) / tile_size) * tile_size
  x1 <- ceiling(max(area["xmax"], boxes$xmax) / tile_size) * tile_size
  y1 <- ceiling(max(area["ymax"], boxes$ymax) / tile_size) * tile_size

  # all tiles, with lower left corners
  tiles <- expand.grid(xmin = seq(x0, x1 - tile_size, by = tile_size),
                       ymin = seq(y0, y1 - tile_size, by = tile_size))
  tiles$xmax <- tiles$xmin + tile_size
  tiles$ymax <- tiles$ymin + tile_size
  tiles$tile_id <- sprintf("E%04d_N%04d", tiles$xmin / 1000, tiles$ymin / 1000)

  tiles$wkt <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
                       tiles$xmin, tiles$ymin, tiles$xmax, tiles$ymin, tiles$xmax, tiles$ymax,
                       tiles$xmin, tiles$ymax, tiles$xmin, tiles$ymin)
  tiles <- st_as_sf(tiles, wkt = "wkt", crs = crs)

  # only the tiles which touch the AOI or any box
  tiles$in_aoi <- st_intersects(tiles, DE_buf, sparse = FALSE)[, 1]
  tiles$n_boxes <- lengths(st_intersects(tiles, boxes))
  tiles <- tiles[tiles$in_aoi | tiles$n_boxes > 0, ]

  # UTM zone of the tile centre
  centres <- st_transform(st_centroid(st_geometry(tiles)), 4326)
  tiles$utm_zone <- floor((st_coordinates(centres)[, 1] + 180) / 6) + 1
  tiles
}

##################################################################################

# number of boxes cut by a fixed grid of this size (grid moved to its best position)
count_cut_boxes <- function(boxes, size, step = 100) {
  shifts <- seq(0, size - step, by = step)

  # best shift for the vertical edges
  cut_x <- c()
  for (s in shifts) {
    col_left <- floor((boxes$xmin - s) / size)
    col_right <- floor((boxes$xmax - 1 - s) / size)     # -1 m: a box that only touches an edge is not cut
    cut_x <- c(cut_x, sum(col_left != col_right))
  }
  best_x <- shifts[which.min(cut_x)]

  # best shift for the horizontal edges
  cut_y <- c()
  for (s in shifts) {
    row_bottom <- floor((boxes$ymin - s) / size)
    row_top <- floor((boxes$ymax - 1 - s) / size)
    cut_y <- c(cut_y, sum(row_bottom != row_top))
  }
  best_y <- shifts[which.min(cut_y)]

  cut <- floor((boxes$xmin - best_x) / size) != floor((boxes$xmax - 1 - best_x) / size) |
         floor((boxes$ymin - best_y) / size) != floor((boxes$ymax - 1 - best_y) / size)
  sum(cut)
}
