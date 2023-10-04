v_id <- evi_df %>%
  pull(id) %>%
  unique() %>%
  sort()
v_year <- 2000:2022

df_evi_meta <- evi_df %>%
  distinct(id, lat, lon)


df_climate <- data.frame()

cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

df_daymet <- foreach(
  idoi = v_id,
  .packages = c("tidyverse", "lubridate", "daymetr")
) %dopar% {
  
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
    
}

df_climate <- do.call(rbind,df_daymet)

stopCluster(cl)


write_rds(df_climate, str_c(.path$dat_proc, "climate_df.rds"))
