# https://www.neonscience.org/data-samples/data/spatial-data-maps

# get US boundary
file<-str_c(.path$neon_domain,"NEON_Domains.shp")
domains <- sf::st_read(file)
# plot(domains)
domains$DomainName %>% unique()
