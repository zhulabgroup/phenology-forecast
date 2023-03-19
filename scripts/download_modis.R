# set symbolic link for raw data folder
# ln -s /nfs/turbo/seas-zhukai/phenology/MODIS data/raw/MODIS

# get area extent
source("scripts/read_neon_domain.R")
sf::st_bbox(domains %>% filter(DomainName == area_full))

# Search in https://search.earthdata.nasa.gov/search
# Dataset: MYD13A1 and MOD13A1 v061
# MODIS/Aqua Vegetation Indices 16-Day L3 Global 500m SIN Grid V0061
# MODIS/Terra Vegetation Indices 16-Day L3 Global 500m SIN Grid V061
# rectangle SW: 25, -125, NE: 53, -67(Continental US, following NLDAS's bounding box, 22 tiles) (Customize to study area)
# get download links as txt

# set credentials
cred <- list(
  username = "[insert your username]",
  password = "[insert your password]"
)

# download in parallel
system(paste0("cat ", .path$rs_modis, area, "/terra_download.txt | xargs -n 1 -P 36 wget --user=", cred$username, " --password=", cred$password, " --auth-no-challenge=on --no-verbose --no-clobber --no-host-directories --no-directories --continue --directory-prefix=", .path$rs_modis, area, "/"))
system(paste0("cat ", .path$rs_modis, area, "/aqua_download.txt | xargs -n 1 -P 36 wget --user=", cred$username, " --password=", cred$password, " --auth-no-challenge=on --no-verbose --no-clobber --no-host-directories --no-directories --continue --directory-prefix=", .path$rs_modis, area, "/"))
