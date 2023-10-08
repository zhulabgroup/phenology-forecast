read_lc <- function(indir = "alldata/input/", domain, area, outdir = "alldata/intermediate/") {
  area_name <- util_area_underscore(area_full = area)

  dir_lc_hdf <- str_c(indir, "MODIS_LC/", area_name, "/raw/")

  v_file <- list.files(dir_lc_hdf, pattern = "hdf")
  v_year_code <- v_file %>%
    str_split("\\.", simplify = T) %>%
    data.frame() %>%
    pull(X2) %>%
    unique() %>%
    sort() %>%
    str_sub(1, 5)

  ls_ras_lc <- vector(mode = "list")
  for (year_code in v_year_code) {
    Sys.sleep(runif(1))

    f_year <- list.files(dir_lc_hdf, pattern = year_code, full.names = T)
    if (length(f_year) > 0) {
      year <- year_code %>%
        str_sub(2, 5) %>%
        as.numeric()

      ls_ras_lc_year <- vector(mode = "list")
      for (i in 1:length(f_year)) {
        file <- f_year[i]
        ras <- file %>%
          terra::rast()
        ls_ras_lc_year[[i]] <- ras %>%
          terra::subset(subset = 1)
      }
      ls_ras_lc[[as.character(year)]] <- do.call(terra::mosaic, c(ls_ras_lc_year, fun = "min")) %>%
        terra::crop(domain) %>%
        terra::mask(domain)

      print(year)
    }
  }

  ras_lc <- terra::rast(ls_ras_lc)

  # dir_out <- str_c(outdir, area_name, "/")
  # dir.create(dir_out, recursive = T, showWarnings = F)
  # f_out <- str_c(dir_out, "lc.tif")
  # terra::writeRaster(ras_lc, f_out)

  return(ras_lc)
}
