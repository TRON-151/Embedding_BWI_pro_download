# functions for the download loop, the file checks and the readme for Thünen

# boxes or tiles as one simple table
make_area_list <- function(run_mode, boxes, tiles) {
  if (run_mode == "boxes") {
    areas <- data.frame(name = sprintf("cluster_%07d", boxes$cluster_name),
                        xmin = boxes$xmin, ymin = boxes$ymin, xmax = boxes$xmax, ymax = boxes$ymax,
                        utm_zone = boxes$utm_zone, size = boxes$n_rows)
  } else {
    areas <- data.frame(name = tiles$tile_id,
                        xmin = tiles$xmin, ymin = tiles$ymin, xmax = tiles$xmax, ymax = tiles$ymax,
                        utm_zone = tiles$utm_zone, size = tiles$n_boxes)
  }
  # sorted in 10 km rows from south to north, and west to east inside a row,
  # so each part of a parallel run is one piece of the map
  areas[order(floor(areas$ymin / 10000), areas$xmin), ]
}

##################################################################################

# test run: 5 boxes in UTM zone 32 and 5 in zone 33, or the 2 tiles with the most boxes
pick_test_areas <- function(areas, run_mode) {
  z32 <- areas[areas$utm_zone == 32, ]
  z33 <- areas[areas$utm_zone == 33, ]
  if (run_mode == "boxes") {
    rbind(head(z32, 5), head(z33, 5))
  } else {
    rbind(z32[which.max(z32$size), ], z33[which.max(z33$size), ])
  }
}

##################################################################################

# year by year, so one year is complete before the next one starts.
# files that already exist are skipped, so a stopped run can just be started again.
# the time of each download goes into time_file
run_download <- function(areas, years, embeddings, out_root, aef_index, gt, olmo, pixel_size, crs, time_file) {
  for (y in years) {
    for (p in embeddings) {
      dir.create(file.path(out_root, y, p), recursive = TRUE, showWarnings = FALSE)
    }
  }

  for (year in years) {
    for (i in seq_len(nrow(areas))) {
      a <- areas[i, ]
      template <- rast(ext(a$xmin, a$xmax, a$ymin, a$ymax), res = pixel_size, crs = crs)

      for (p in embeddings) {
        out_file <- file.path(out_root, year, p, paste0(a$name, ".tif"))
        if (file.exists(out_file)) next

        start_time <- Sys.time()
        if (p == "alphaEarth") {
          emb <- try(get_aef(template, year, aef_index))
        } else if (p == "olmo") {
          emb <- try(get_olmo(template, year, olmo))
        } else {
          emb <- try(get_tessera(template, year, gt))
        }
        if (inherits(emb, "try-error") || is.null(emb)) {
          message("  ", a$name, " ", p, " ", year, " failed, it is tried again in the next run")
          next
        }
        save_raster(emb, out_file, int8 = (p %in% c("alphaEarth", "olmo")))
        rm(emb)

        # Sys.time() - start_time picks seconds or minutes by itself, so we always ask for seconds
        seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        message(format(Sys.time(), "%H:%M:%S"), "  ", year, "  ", i, "/", nrow(areas), "  ",
                a$name, "  ", p, "  ", round(seconds, 1), " s")

        one_row <- data.frame(time = format(Sys.time()), year = year, area = a$name,
                              embedding = p, seconds = round(seconds, 1))
        if (file.exists(time_file)) {
          write.table(one_row, time_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        } else {
          write.table(one_row, time_file, sep = ",", row.names = FALSE)
        }
      }
      gc()
    }
  }
}

##################################################################################
# to check the downloaded files
check_downloads <- function(out_root, years, embeddings, pixel_size) {
  # 1. each year, type of embeddings, how many files
  for (y in years) {
    for (p in embeddings) {
      n <- length(list.files(file.path(out_root, y, p), pattern = "\\.tif$"))
      print(paste(y, p, n, "files"))
    }
  }

  aef_dir <- file.path(out_root, years[1], "alphaEarth")
  tes_dir <- file.path(out_root, years[1], "tessera")
  both <- intersect(list.files(aef_dir, pattern = "\\.tif$"), list.files(tes_dir, pattern = "\\.tif$"))
  if (length(both) == 0) {
    print("no area with both embeddings yet")
    return(invisible(NULL))
  }

  # 2. comparing both embeddings geometry, with layers as well
  for (f in head(both, 5)) {
    a <- rast(file.path(aef_dir, f))
    b <- rast(file.path(tes_dir, f))
    print(paste(f, compareGeom(a, b, stopOnError = FALSE), nlyr(a), nlyr(b)))
  }

  # 3. are the file edges on the 10 m grid
  print(all(as.vector(ext(rast(file.path(aef_dir, both[1])))) %% pixel_size == 0))

  # 4. same grid for the same area in different years
  for (y in years) {
    other <- file.path(out_root, y, "alphaEarth", both[1])
    if (file.exists(other)) {
      same <- compareGeom(rast(file.path(aef_dir, both[1])), rast(other), stopOnError = FALSE)
      print(paste(y, same))
    }
  }
}

##################################################################################

write_readme <- function(out_root, years, box_extra_margin, n_dropped, tessera_ver) {
  if (n_dropped > 0) {
    dropped_text <- paste0(n_dropped, " clusters are not included. They lie in groups of 3 or more boxes that ",
                           "overlap each other (near Hachenburg, Kelheim and Eberswalde). They are in dropped_boxes.gpkg.")
  } else {
    dropped_text <- "All clusters of the csv are included, also the ones whose boxes overlap each other."
  }

  readme <- c(
    "EMBEDDINGS FOR THE BWI CLUSTERS",
    paste("Made:", Sys.Date()),
    "",
    "WHAT IS IN THE FILES",
    "<year>/<embedding>/cluster_<cluster_name with 7 digits>.tif, one file per cluster and year.",
    paste("Years:", paste(years, collapse = ", ")),
    "CRS: EPSG:25832. Pixel size: 10 m. Both embeddings and all years are on the same pixel grid.",
    paste0("Each box goes around all rows of one cluster_name in the csv, plus ", box_extra_margin,
           " m, and out to the next 10 m line. The boxes are in cluster_boxes.gpkg."),
    dropped_text,
    "",
    "alphaEarth: AlphaEarth Foundations annual embeddings (copy on source.coop), 64 bands aef_00 to aef_63.",
    "  The values are the raw int8 values, -128 = no data. Real value = (v / 127.5)^2 * sign(v).",
    paste0("tessera: TESSERA ", tessera_ver, ", read with the geotessera R package. ",
           "128 bands tes_000 to tes_127, float32, NA = no data.")
  )
  writeLines(readme, file.path(out_root, "README.txt"))
}
