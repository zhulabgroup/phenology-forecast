# download daymet and reform evi_DB structure
v_id <- df_evi %>%
  pull(id) %>%
  unique() %>%
  sort()
v_year <- 2000:2021

df_evi_meta <- df_evi %>%
  distinct(id, lat, lon)

cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

evi_DB <- foreach(
  idoi = v_id,
  .packages = c("tidyverse", "lubridate", "daymetr")
) %dopar% {
  v_sos <- df_pheno_paras %>%
    filter(id == idoi) %>%
    distinct(year, sos) %>%
    pull(sos) %>%
    round()
    
  evi_temp <- df_climate %>%
    dplyr::select(temp) %>% matrix(nrow = 365,byrow = TRUE)

  evi_ltm <- evi_temp %>% rowMeans(na.rm = T)

  evi_tmin <- df_climate %>%
    dplyr::select(year, doy, tmin) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_tmax <- df_climate %>%
    dplyr::select(year, doy, tmax) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_prcp <- df_climate %>%
    dplyr::select(year, doy, prcp) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_vp <- df_climate %>%
    dplyr::select(year, doy, vp) %>%
    as.matrix() %>%
    unname() %>%
    t()

  evi_dayl <- df_climate %>%
    dplyr::select(year, doy, dayl) %>%
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
