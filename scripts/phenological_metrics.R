# install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

# extract phenological metrics
v_id <- evi_df %>%
  pull(id) %>%
  unique() %>%
  sort()
v_year <- 2000:2022

# Ziyu, reusing variable evi_all is risky. please revise. It is better to fill items of a list and use bind_rows on the list.

cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

ls_df_evi <-
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

    evi_ts <- df_evi_subset %>%
      pull(evi) %>%
      ts(frequency = 365, start = c(2000, 1))

    res <- Phenology(evi_ts, tsgf = "TSGFspline", approach = "White")

    # time series pre-processing
    # evi_tsgf <- TsPP(evi_ts, tsgf = TSGFspline)

    # logistic approach
    # tsgf1 <- TSGFdoublelog(window(evi_ts,2000,c(2022,365)),interpolate = T,method = "Elmore",
    # check.seasonality = NULL)

    # plot(evi_tsgf)
    # lines(evi_tsgf,col="blue")

    print(idoi)

    df_evi <- df_evi_subset %>%
      mutate(evi_tsgf = res$series) %>%
      mutate(
        mean_sos = mean(res$sos, na.rm = T),
        mean_eos = mean(res$eos, na.rm = T)
      ) %>%
      left_join(
        data.frame(
          year = v_year,
          sos = res$sos,
          eos = res$eos
        ),
        by = "year"
      )

    df_evi
  }

evi_all <- bind_rows(ls_df_evi)
stopCluster(cl)

ggplot(evi_all) +
  geom_line(
    aes(x = doy, y = evi_tsgf, group = year, col = year),
    alpha = 0.5
  ) +
  geom_vline(aes(xintercept = sos), alpha = 0.2) +
  geom_vline(aes(xintercept = eos), alpha = 0.2) +
  theme_classic() +
  facet_wrap(. ~ id) +
  scale_color_viridis_c()



#### Ziyu, here are some residual code from previous testing. Please tidy up or delete.

data(ndvi)
x <- as.vector(window(ndvi, start = c(1991, 1), end = c(1991, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
fit
plot(x)
lines(fit$predicted, col = "blue")

# do more inital trials, plot iterations and compute parameter uncertainties
FitDoubleLogElmore(x, hessian = TRUE, plot = TRUE, ninit = 1000)

# fit double-logistic function to one year of data,
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length = 365) # time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout = tout)
plot(x)
lines(tout, fit$predicted, col = "blue")
PhenoDeriv(fit$predicted, plot = TRUE)
