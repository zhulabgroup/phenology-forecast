# https://www.neonscience.org/data-samples/data/spatial-data-maps

# get US boundary
file <- str_c(.path$neon_domain, "NEON_Domains.shp")
domains <- sf::st_read(file)
plot(sf::st_geometry(domains))
domains$DomainName %>% unique()

domain <- domains %>% filter(DomainName == area_full)
plot(sf::st_geometry(domain))

domain_reproj <- sf::st_transform(domain,
  crs = sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
)
