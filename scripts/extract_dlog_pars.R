# extract phenological metrics
v_id <- evi_df %>%
  pull(id) %>%
  unique() %>%
  sort()
v_year <- 2000:2022

# Ziyu, reusing variable evi_all is risky. please revise. It is better to fill items of a list and use bind_rows on the list.

mn_vec <- c()
mx_vec <- c()
rsp_vec <- c()
sos_vec <- c()
rau_vec <- c()
eos_vec <- c()
cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

ls_df_par <-

  foreach(
    idoi = v_id,
    .packages = c("tidyverse", "greenbrown")
  ) %dopar% {
    df_evi_subset <- evi_df %>%
      filter(id == idoi) %>%
      dplyr::select(-lat, -lon) %>%
      tidyr::complete(
        date = seq(lubridate::date("2000-01-01"),
                   lubridate::date("2022-12-31"),
                   by = "day"
        ),
        fill = list(id = idoi)
      ) %>%
      mutate(
        year = as.numeric(format(date, format = "%Y")),
        month = as.numeric(format(date, format = "%m")),
        day = as.numeric(format(date, format = "%d")),
        doy = lubridate::yday(date)
      ) %>%
      filter(year <= 2022) %>%
      filter(doy <= 365)
    for (yrs in 2000:2022){
      evi_ts <- df_evi_subset %>% filter(year == yrs) %>%
      pull(evi) %>%
        ts(frequency = 365, start = c(yrs, 1))
      evi_ts <- data.frame(evi_pt = evi_ts)
      
      evi_ts_all <- bind_rows(evi_ts,evi_ts_all)
      
      
      res <- FitDoubleLogBeck(evi_ts, plot=TRUE, ninit=100)
      mn_vec <- c(mn_vec, res$params[1])
      mx_vec <- c(mx_vec, res$params[2])
      sos_vec <- c(sos_vec, res$params[3])
      rsp_vec <- c(rsp_vec, res$params[4])
      eos_vec <- c(eos_vec, res$params[5])
      rau_vec <- c(rau_vec, res$params[6])
      
    }
    # time series pre-processing
    # evi_tsgf <- TsPP(evi_ts, tsgf = TSGFspline)
    # logistic approach
    # tsgf1 <- TSGFdoublelog(window(evi_ts,2000,c(2022,365)),interpolate = T,method = "Elmore",
    # check.seasonality = NULL)
    # plot(evi_tsgf)
    # lines(evi_tsgf,col="blue")
    
    print(idoi)
    
    df_par <- df_evi_subset  %>%
      left_join(
        data.frame(
          year = v_year,
          MN = mn_vec,
          MX = mx_vec,
          SOS = sos_vec,
          RSP = rsp_vec,
          EOS = eos_vec,
          RAU = rau_vec
        ),
        by = "year"
      )
    
    df_par
  }

par_all <- bind_rows(ls_df_par)
stopCluster(cl)

#write_rds(par_all, str_c(.path$dat_proc, "dlog_par_all.rds"))




