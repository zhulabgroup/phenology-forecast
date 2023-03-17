# choose random coordinates
domain <- domains %>% filter(DomainName == area_full)
plot(sf::st_geometry(domain))

set.seed(1)
coord <- sf::st_sample(domain, 100)
coord_df <- coord %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  rename(lon = X, lat = Y) %>%
  mutate(id = row_number()) %>%
  select(id, everything())

plot(sf::st_geometry(coord), add = T)

# sf::st_crs(coord)$proj4string
coord_reproj <- sf::st_transform(coord,
  crs = sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
)

# get dates
filenames <- list.files(str_c(.path$rs_evi, area), pattern = "evi")
date_list <- filenames %>%
  str_split("\\.", simplify = T) %>%
  data.frame() %>%
  pull(X2) %>%
  unique() %>%
  sort() %>%
  as.Date()

evi_df_list <- vector(mode = "list")
for (i in 1:length(date_list)) {
  date <- date_list[i]

  evi_df <- coord_df %>%
    mutate(date = date) %>%
    mutate(
      evi = NA,
      qa = NA
    )

  evi_file <- list.files(str_c(.path$rs_evi, area), pattern = str_c("evi.", date %>% as.character()), full.names = T)
  evi_ras <- terra::rast(evi_file)
  evi_df$evi <- terra::extract(evi_ras, coord_reproj %>% terra::vect()) %>%
    select(-ID) %>%
    pull(var = 1)

  qa_file <- list.files(str_c(.path$rs_evi, area), pattern = str_c("qa.", date %>% as.character()), full.names = T)
  qa_ras <- terra::rast(qa_file)
  evi_df$qa <- terra::extract(qa_ras, coord_reproj %>% terra::vect()) %>%
    select(-ID) %>%
    pull(var = 1)
  # https://rspatial.org/modis/4-quality.html
  # https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A1#bands

  evi_df_list[[i]] <- evi_df
  print(i)
}
evi_df <- bind_rows(evi_df_list) %>%
  mutate(evi = evi * 0.0001 * 0.0001) %>%
  mutate(qa_str = R.utils::intToBin(qa) %>% str_pad(15, pad = "0", side = "left")) %>%
  mutate(qa_good = str_sub(qa_str, 1, 2) %>% as.numeric()) %>%
  mutate(qa_good = (qa_good == 0))
