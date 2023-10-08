download_daymet <- function(sp_coord, outdir = "alldata/intermediate/", area, num_cores = 35) {
  area_name <- util_area_underscore(area_full = area)

  df_coord <- sp_coord %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    rename(lon = X, lat = Y)

  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  ls_df_daymet <-
    foreach(
      idoi = 1:nrow(sp_coord_wgs),
      .packages = c("tidyverse", "lubridate", "daymetr")
    ) %dopar% {
      daymetr::download_daymet(
        site = idoi,
        lat = df_coord %>% slice(idoi) %>% pull(lat),
        lon = df_coord %>% slice(idoi) %>% pull(lon),
        start = 2000, # lubridate::year(min(evi_df$date)),
        # end = 2022, # min(lubridate::year(max(evi_df$date))-1),
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
        select(id = site, lat = latitude, lon = longitude, date, year, doy, dayl, temp, tmax, tmin, prcp, vp)
    }

  df_daymet <- bind_rows(ls_df_daymet)

  dir_out <- str_c(outdir, area_name, "/")
  dir.create(dir_out, recursive = T, showWarnings = F)
  f_out <- str_c(dir_out, "climate.rds")
  write_rds(df_daymet, f_out)
}
