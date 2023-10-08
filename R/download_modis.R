download_modis <- function(data = "evi", area, dir = "alldata/input/", num_cores = 36) {
  area_name <- util_area_underscore(area_full = area)

  dir_download <- str_c(
    dir,
    case_when(
      data == "evi" ~ "MODIS_EVI/",
      data == "lc" ~ "MODIS_LC/"
    )
  )
  dir_download_area <- str_c(
    dir_download,
    area_name, "/"
  )

  if (data == "evi") {
    file_download_url_terra <- str_c(dir_download_area, "terra_download.txt")
    file_download_url_aqua <- str_c(dir_download_area, "aqua_download.txt")
    file_download_url <- c(file_download_url_terra, file_download_url_aqua)
  }
  if (data == "lc") {
    file_download_url <- str_c(dir_download_area, "land_cover_download.txt")
  }

  if (sum(!file.exists(file_download_url)) == 0) {
    dir_download_area_raw <- str_c(dir_download_area, "raw/")
    dir.create(dir_download_area_raw, recursive = T, showWarnings = F)

    # set credentials
    cred <- list(
      username = getPass::getPass(msg = "insert your Earthdata username: "),
      password = getPass::getPass(msg = "insert your Earthdata password: ")
    )

    # download in parallel
    for (f in file_download_url) {
      system(paste0("cat ", f, " | xargs -n 1 -P ", num_cores, " wget --user=", cred$username, " --password=", cred$password, " --auth-no-challenge=on --no-verbose --no-clobber --no-host-directories --no-directories --continue --directory-prefix=", dir_download_area_raw))
    }

    print(str_c("Data were downloaded to ", dir_download_area_raw))
  } else {
    print("Please download file with download urls")
    return(NULL)
  }
}
