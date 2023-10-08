tidy_evi <- function(sp_evi, year_forecast = 2018) {
  df_evi <- sp_evi %>%
    sf::st_transform(4326) %>%
    bind_cols(sf::st_coordinates(.)) %>%
    rename(lon = X, lat = Y) %>%
    sf::st_drop_geometry() %>%
    mutate(evi = evi * 0.0001 * 0.0001) %>%
    mutate(qa_str = R.utils::intToBin(qa) %>% str_pad(15, pad = "0", side = "left")) %>%
    mutate(qa_good = str_sub(qa_str, 1, 2) %>% as.numeric()) %>%
    mutate(qa_good = (qa_good == 0)) %>%
    filter(qa_good) %>%
    select(-qa, -qa_str, -qa_good) %>%
    mutate(
      year = as.numeric(format(date, format = "%Y")),
      # month = as.numeric(format(date, format = "%m")),
      # day = as.numeric(format(date, format = "%d")),
      doy = lubridate::yday(date)
    ) %>%
    rename(id = ID) %>%
    mutate(label = case_when(
      year < year_forecast ~ "training",
      TRUE ~ "validation"
    )) %>%
    select(id, lat, lon, date, year, doy, evi, label) %>%
    as_tibble()

  return(df_evi)
}
