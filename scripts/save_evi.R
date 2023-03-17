area <- "Appalachians"

# get dates
filenames <- list.files(str_c(.path$rs_modis, area), pattern = "hdf")
day_code_list <- filenames %>%
  str_split("\\.", simplify = T) %>%
  data.frame() %>%
  pull(X2) %>%
  unique() %>%
  sort()

# save evi and qa layers in tif
for (day_code in day_code_list) {
  filenames_day <- list.files(str_c(.path$rs_modis, area), pattern = day_code, full.names = T)
  if (length(filenames_day) > 0) {
    year <- day_code %>%
      str_sub(2, 5) %>%
      as.numeric()
    doy <- day_code %>%
      str_sub(6, 8) %>%
      as.numeric()
    date <- as.Date(str_c(year, "-01-01")) + doy - 1
    evi_ras_list <- vector(mode = "list")
    qa_ras_list <- vector(mode = "list")
    for (i in 1:length(filenames_day)) {
      file <- filenames_day[i]
      ras <- file %>%
        terra::rast()
      evi_ras_list[[i]] <- ras %>%
        terra::subset(subset = 2)
      qa_ras_list[[i]] <- ras %>%
        terra::subset(subset = 3)
    }
    evi_ras <- do.call(terra::mosaic, c(evi_ras_list, fun = "mean"))
    qa_ras <- do.call(terra::mosaic, c(qa_ras_list, fun = "max"))
    # terra::plot(evi_ras)
    # terra::crs(evi_ras,proj=T)
    terra::writeRaster(evi_ras, str_c(.path$rs_evi, area, "/evi.", date, ".tif"), overwrite = T)
    terra::writeRaster(qa_ras, str_c(.path$rs_evi, area, "/qa.", date, ".tif"), overwrite = T)
    print(date)
  }
}
