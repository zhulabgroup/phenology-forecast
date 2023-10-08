read_climate <- function(dir = "alldata/intermediate/", area) {
  area_name <- util_area_underscore(area_full = area)

  dir_area <- str_c(dir, area_name, "/")
  f_daymet <- str_c(dir_area, "climate.rds")
  df_daymet <- read_rds(f_daymet)

  return(df_daymet)
}
