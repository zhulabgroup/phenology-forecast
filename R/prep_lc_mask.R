prep_lc_mask <- function(ras_lc, lc_code = NULL, lc_name = "Deciduous Broadleaf Forests") {
  if (lc_name == "Grasslands") {
    lc_code <- 10
  }
  if (lc_name == "Deciduous Broadleaf Forests") {
    lc_code <- 4
  }
  # https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf


  ras_lc[ras_lc != lc_code] <- NA

  ras_lc_mask <- terra::app(ras_lc, median, na.rm = FALSE)

  ras_lc_mask[!is.na(ras_lc_mask)] <- 1
  return(ras_lc_mask)
}
