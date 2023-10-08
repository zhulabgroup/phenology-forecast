read_neon_domain <- function(dir = "alldata/input/NEON_domain/", area_full, proj = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") {
  # https://www.neonscience.org/data-samples/data/spatial-data-maps

  # get US boundary
  file <- str_c(dir, "NEON_Domains.shp")
  domains <- sf::st_read(file)

  domain_wgs <- domains %>% filter(DomainName == area_full)

  domain_sinu <- sf::st_transform(domain_wgs, crs = proj)

  domain <- list(
    wgs = domain_wgs,
    sinu = domain_sinu
  )
  return(domain)
}
