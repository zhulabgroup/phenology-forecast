#install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

evi_df_ts <- evi_df %>%
  mutate(
    year = as.numeric(format(date, format = "%Y")),
    month = as.numeric(format(date, format = "%m")),
    day = as.numeric(format(date, format = "%d")),
    doy = lubridate::yday(date)
  ) %>%
  filter(id == 1, year == 2010) %>%
  tidyr::complete(doy = seq(1, 365, by = 1)) %>%
  filter(doy <= 365) %>%
  arrange(doy) %>%
  pull(evi) %>%
  ts(frequency = 365)

Phenology(evi_df_ts, tsgf = "TSGFspliine", approach = "White")