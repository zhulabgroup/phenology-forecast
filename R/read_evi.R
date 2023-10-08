read_evi_tidy <- function(dir = "alldata/intermediate/", area, year_forecast = 2018) {
  area_name <- util_area_underscore(area_full = area)

  dir_area <- str_c(dir, area_name, "/")
  f_evi <- str_c(dir_area, "evi.rds")
  sp_evi <- read_rds(f_evi)

  df_evi <- tidy_evi(sp_evi, year_forecast = year_forecast)

  return(df_evi)
}

read_evi_full <- function(indir = "alldata/input/", domain, area, sp_coord, outdir = "alldata/intermediate/", num_cores) {
  area_name <- util_area_underscore(area_full = area)

  dir_evi_hdf <- str_c(indir, "MODIS_EVI/", area_name, "/raw/")

  v_file <- list.files(dir_evi_hdf, pattern = "hdf")
  v_day_code <- v_file %>%
    str_split("\\.", simplify = T) %>%
    data.frame() %>%
    pull(X2) %>%
    unique() %>%
    sort()

  cl <- makeCluster(num_cores, type = "SOCK", outfile = "")
  registerDoSNOW(cl)

  ls_sp_evi <-
    foreach(
      day_code = v_day_code,
      .packages = c("tidyverse", "terra")
    ) %dopar% {
      Sys.sleep(runif(1))

      f_day <- list.files(dir_evi_hdf, pattern = day_code, full.names = T)
      if (length(f_day) > 0) {
        year <- day_code %>%
          str_sub(2, 5) %>%
          as.numeric()
        doy <- day_code %>%
          str_sub(6, 8) %>%
          as.numeric()
        date <- as.Date(str_c(year, "-01-01")) + doy - 1

        ls_sp_evi_day <- vector(mode = "list")
        for (i in 1:length(f_day)) {
          ls_sp_evi_day[[i]] <- f_day[i] %>%
            terra::rast() %>%
            terra::subset(subset = c(2, 3)) %>%
            terra::extract(y = sp_coord, xy = T) %>%
            drop_na() %>%
            rename(
              evi = `"500m 16 days EVI"`,
              qa = `"500m 16 days VI Quality"`
            ) %>%
            sf::st_as_sf(
              coords = c("x", "y"),
              crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
            )
        }
        sp_evi_day <- bind_rows(ls_sp_evi_day) %>%
          mutate(date = date)

        print(date)

        sp_evi_day
      }
    }
  stopCluster(cl)

  sp_evi <- bind_rows(ls_sp_evi)

  dir_out <- str_c(outdir, area_name, "/")
  dir.create(dir_out, recursive = T, showWarnings = F)
  f_out <- str_c(dir_out, "evi.rds")
  write_rds(sp_evi, f_out)

  return(sp_evi)
}
