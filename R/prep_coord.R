prep_coord <- function(ras_lc_mask, num_coord = 100, seed = 1, area, outdir = "alldata/intermediate/") {
  area_name <- util_area_underscore(area_full = area)

  set.seed(seed)
  sp_coord <- terra::spatSample(x = ras_lc_mask, size = num_coord, na.rm = T, xy = T, values = F) %>%
    data.frame() %>%
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
    )

  dir_out <- str_c(outdir, area_name, "/")
  dir.create(dir_out, recursive = T, showWarnings = F)
  f_out <- str_c(dir_out, "coord.rds")
  write_rds(sp_coord, f_out)
  return(sp_coord)
}
