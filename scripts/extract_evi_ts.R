source("scripts/read_neon_domain.R")

# choose random coordinates
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

day_code_list_new <- str_c(
  "A",
  lubridate::year(date_list),
  lubridate::yday(date_list) %>% str_pad(3, "left", "0")
)
day_code_list_diff <- setdiff(day_code_list, day_code_list_new)

cl <- makeCluster(36, type = "SOCK", outfile = "")
registerDoSNOW(cl)
evi_df_list <-
  foreach(
    date = date_list,
    .packages = c("tidyverse", "terra", "sf")
  ) %dopar% {
    Sys.sleep(runif(1) * 36)

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

    print(date)
    evi_df
  }
stopCluster(cl)

evi_df <- bind_rows(evi_df_list) %>%
  mutate(evi = evi * 0.0001 * 0.0001) %>%
  mutate(qa_str = R.utils::intToBin(qa) %>% str_pad(15, pad = "0", side = "left")) %>%
  mutate(qa_good = str_sub(qa_str, 1, 2) %>% as.numeric()) %>%
  mutate(qa_good = (qa_good == 0))

write_rds(evi_df, str_c(.path$rs_evi, area, ".ts.rds"))
