# GDAL settings, so terra can read files from the internet without a login
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
Sys.setenv(GDAL_HTTP_MAX_RETRY = "5", GDAL_HTTP_RETRY_DELAY = "3")
Sys.setenv(AWS_NO_SIGN_REQUEST = "YES", AWS_REGION = "us-west-2")
Sys.setenv(AWS_S3_ENDPOINT = "s3.us-west-2.amazonaws.com", AWS_VIRTUAL_HOSTING = "FALSE")
terraOptions(progress = 0)


buffer_plot <- function(DE, DE_buf){
  plot(DE_buf, col = "lightblue", border = "blue", main = "DE Geometry with buffered zone")
  plot(DE$geometry, col = "orange", border = "darkred", add = TRUE)

  legend("topright", 
         legend = c("Buffer (DE_buf)", "Original (DE)"), 
         fill = c("lightblue", "orange"), 
         border = c("blue", "darkred"))

}