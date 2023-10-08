read_coord <- function(dir = "alldata/intermediate/", area) {
  area_name <- util_area_underscore(area_full = area)

  dir_area <- str_c(dir, area_name, "/")
  f_coord <- str_c(dir_area, "coord.rds")
  sp_coord <- read_rds(f_coord)

  return(sp_coord)
}
