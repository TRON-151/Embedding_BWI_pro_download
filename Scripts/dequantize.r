# dequantizes the int8 AlphaEarth and OlmoEarth files to float32
# PC:    for i in 1 2 3 4 5 6 7 8; do Rscript dequantize.r boxes $i 8 > output/logs/dequant_part_$i.txt 2>&1 & done
# Palma: sbatch palma/run_dequant.sh

library(terra)
terraOptions(progress = 0, memmax = 8)

run_mode <- "tiles"   # "boxes" or "tiles"
years <- c(2022)
embeddings <- c("alphaEarth", "olmo")
testing <- FALSE   # TRUE = only 5 files per embedding

# from "Rscript dequantize.r boxes 1 8"
args <- commandArgs(trailingOnly = TRUE)
part <- 1
n_parts <- 1
if (length(args) >= 1) run_mode <- args[1]
if (length(args) >= 3) {
  part <- as.numeric(args[2])
  n_parts <- as.numeric(args[3])
}

if (run_mode == "boxes") {
  in_root <- here::here("output", "boxes")
  out_root <- path.expand("~/Nextcloud/embed_bwi/Embedding_complete_data/boxes")
} else {
  in_root <- file.path(Sys.getenv("EMB_OUT", here::here("output")), "tiles")
  out_root <- in_root
}


# from source.coop tge-labs/aef
# no -128 step needed, the files have NoData = -128 so terra reads it as NA
aef_dequantize <- function(values) {
  ((values / 127.5)^2) * sign(values)
}

# from source.coop olmoearth/olmoearth-v1-3-embeddings
olmo_dequantize <- function(q) {
  q[q == -128] <- NA
  (abs(q) / 127.5)^2 * sign(q)
}


files <- data.frame()
for (y in years) {
  for (p in embeddings) {
    f <- list.files(file.path(in_root, y, p), pattern = "\\.tif$", full.names = TRUE)
    if (testing) f <- head(f, 5)
    files <- rbind(files, data.frame(embedding = rep(p, length(f)), in_file = f,
                                     out_file = file.path(out_root, y, paste0(p, "_dequant"), basename(f))))
  }
}

# part 1 of 8 takes file 1, 9, 17, ...
files <- files[(seq_len(nrow(files)) - 1) %% n_parts + 1 == part, ]
files <- files[!file.exists(files$out_file), ]
message(run_mode, ", part ", part, " of ", n_parts, ": ", nrow(files), " files to do")

for (d in unique(dirname(files$out_file))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(files))) {
  f <- files[i, ]
  if (f$embedding == "alphaEarth") fun <- aef_dequantize else fun <- olmo_dequantize
  r <- rast(f$in_file)

  # write to .part first, so a stopped run leaves no half file
  part_file <- paste0(f$out_file, ".part")
  try({
    app(r, fun, filename = part_file, overwrite = TRUE,
        wopt = list(filetype = "GTiff", datatype = "FLT4S", names = names(r), gdal = "COMPRESS=DEFLATE"))
    file.rename(part_file, f$out_file)
    message(format(Sys.time(), "%H:%M:%S"), "  ", i, "/", nrow(files), "  ", f$embedding, "  ", basename(f$in_file))
  })
  gc()
}