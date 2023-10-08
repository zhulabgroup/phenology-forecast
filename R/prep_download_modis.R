prep_download_modis <- function(data = "evi", domain, area, dir = "alldata/input/") {
  area_name <- util_area_underscore(area_full = area)

  # prepare directory
  dir_download <- str_c(
    dir,
    case_when(
      data == "evi" ~ "MODIS_EVI/",
      data == "lc" ~ "MODIS_LC/"
    )
  )
  msg_dir <- str_c("Please set up data folder ", dir_download, " (recommend setting symbolic link for this data folder)")
  print(msg_dir)

  dir_download_area <- str_c(
    dir_download,
    area_name, "/"
  )
  dir.create(dir_download_area, recursive = T, showWarnings = F)
  msg_dir_area <- str_c("Your data is going to be downloaded to ", dir_download_area)
  print(msg_dir_area)

  # data portal
  msg_search <- "Search in https://search.earthdata.nasa.gov/search with the parameters below"
  print(msg_search)

  # product name
  if (data == "evi") {
    msg_data <- str_c("Dataset: MYD13A1 v061 (MODIS/Aqua Vegetation Indices 16-Day L3 Global 500m SIN Grid V0061)",
      "Dataset: MOD13A1 v061 (MODIS/Terra Vegetation Indices 16-Day L3 Global 500m SIN Grid V061)",
      sep = " and "
    )
  }

  if (data == "lc") {
    msg_data <- "Dataset: MCD12Q1 v061 (MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500 m SIN Grid V061)"
  }
  print(msg_data)

  # get area extent
  box <- sf::st_bbox(domain)
  msg_box <- str_c(
    "SW: ", box$ymin %>% floor(), ", ", box$xmin %>% floor(), ", ",
    "NE: ", box$ymax %>% ceiling(), ", ", box$xmax %>% ceiling()
  )
  msg_box_full <- str_c("Rectangle bounding box ", msg_box)
  print(msg_box_full)

  # download url
  if (data == "evi") {
    file_download_url_terra <- str_c(dir_download_area, "terra_download.txt")
    file_download_url_aqua <- str_c(dir_download_area, "aqua_download.txt")
    file_download_url <- str_c(file_download_url_terra,
      file_download_url_aqua,
      sep = " and "
    )
  }
  if (data == "lc") {
    file_download_url <- str_c(dir_download_area, "land_cover_download.txt")
  }

  msg_url <- str_c("Get download links as txt to ", file_download_url)
  print(msg_url)
}
