# ln -s /nfs/turbo/seas-zhukai/phenology/MODIS data/raw/MODIS

.path$rs_modis
area_full <- "Appalachians / Cumberland Plateau"
# area_full<-"Pacific Southwest"
sf::st_bbox(domains %>% filter(DomainName == area_full))

# Search in https://search.earthdata.nasa.gov/search
# Dataset: MYD13A1 and MOD13A1 v061
# MODIS/Aqua Vegetation Indices 16-Day L3 Global 500m SIN Grid V0061
# MODIS/Terra Vegetation Indices 16-Day L3 Global 500m SIN Grid V061
# rectangle SW: 25, -125, NE: 53, -67(Continental US, following NLDAS's bounding box, 22 tiles) (Customize to study area)
# get download links as txt

# Use bash script to download files (but encountered error)
# use wget for Linux https://disc.gsfc.nasa.gov/data-access
# set up cookies
# vim ~/.urs_cookies
# wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies --user=yiluansong --ask-password https://e4ftl01.cr.usgs.gov//DP131/MOLA/MYD13A1.061/2023.01.25/MYD13A1.A2023025.h10v05.061.2023042003035.hdf 
# batch download
# wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -nc -nv --content-disposition -i ./aqua_download.txt --directory-prefix=./
# wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -nc -nv --content-disposition -i ./terra_download.txt --directory-prefix=./
