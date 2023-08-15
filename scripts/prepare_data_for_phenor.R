# download daymet and reform evi_DB structure
# Ziyu, I made the data structure exactly the same as that of phenocam_DB, especially that environmental variables should be matrices, not tibble or lists.

df_evi_meta <- evi_df %>%
  distinct(id, lat, lon)

cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

evi_DB <- foreach(
  idoi = v_id,
  .packages = c("tidyverse", "lubridate", "daymetr")
) %dopar% {
  v_sos <- evi_all %>%
    filter(id == idoi) %>%
    distinct(year, sos) %>%
    pull(sos) %>%
    round()

  df_daymet <- daymetr::download_daymet(
    site = df_evi_meta %>% filter(id == idoi) %>% pull(id),
    lat = df_evi_meta %>% filter(id == idoi) %>% pull(lat),
    lon = df_evi_meta %>% filter(id == idoi) %>% pull(lon),
    start = 2000, # lubridate::year(min(evi_df$date)),
    end = 2022, # min(lubridate::year(max(evi_df$date))-1),
    internal = TRUE,
    simplify = TRUE
  ) %>%
    filter(measurement %in% c("dayl..s.", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.", "vp..Pa.")) %>%
    spread(key = "measurement", value = "value") %>%
    rename(
      dayl = `dayl..s.`,
      prcp = `prcp..mm.day.`,
      tmax = `tmax..deg.c.`,
      tmin = `tmin..deg.c.`,
      vp = `vp..Pa.`
    ) %>%
    mutate(date = as.Date(yday, origin = paste0(year, "-01-01")) - 1) %>%
    mutate(temp = (tmax + tmin / 2)) %>%
    mutate(dayl = dayl / 3600) %>%
    as_tibble() %>%
    mutate(year = lubridate::year(date)) %>%
    mutate(doy = lubridate::yday(date)) %>%
    dplyr::select(site, date, year, doy, dayl, temp, tmax, tmin, prcp, vp)

  evi_temp <- df_daymet %>%
    dplyr::select(year, doy, temp) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "temp") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_ltm <- evi_temp %>% rowMeans(na.rm = T)

  evi_tmin <- df_daymet %>%
    dplyr::select(year, doy, tmin) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "tmin") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_tmax <- df_daymet %>%
    dplyr::select(year, doy, tmax) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "tmax") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_prcp <- df_daymet %>%
    dplyr::select(year, doy, prcp) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "prcp") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_vp <- df_daymet %>%
    dplyr::select(year, doy, vp) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "vp") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_dayl <- df_daymet %>%
    dplyr::select(year, doy, dayl) %>%
    arrange(doy) %>%
    filter(doy %in% 1:365) %>%
    spread(key = "doy", value = "dayl") %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    unname() %>%
    t()

  site_list <- list(
    site = as.character(idoi),
    location = c(
      df_evi_meta %>% filter(id == idoi) %>% pull(lon),
      df_evi_meta %>% filter(id == idoi) %>% pull(lat)
    ),
    doy = 1:365,
    ltm = evi_ltm,
    transition_dates = v_sos,
    year = v_year,
    Ti = evi_temp,
    Tmini = evi_tmin,
    Tmaxi = evi_tmax,
    Li = evi_dayl,
    Pi = evi_prcp,
    VPDi = evi_vp
  )
}

names(evi_DB) <- v_id %>% as.character()
stopCluster(cl)

write_rds(evi_DB, str_c(.path$dat_proc, "evi_DB.rds"))
