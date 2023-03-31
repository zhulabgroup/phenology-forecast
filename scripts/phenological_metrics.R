#install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

evi_df_ts <- evi_df %>%
  filter(id == 1) %>%
  tidyr::complete(date = seq(min(date), max(date), by = "day")) %>%
  mutate(
    year = as.numeric(format(date, format = "%Y")),
    month = as.numeric(format(date, format = "%m")),
    day = as.numeric(format(date, format = "%d")),
    doy = lubridate::yday(date)
  ) %>%
  filter(doy <= 365) %>%
  pull(evi) %>%
  ts(frequency = 365, start= c(2000,49))

res<-Phenology(evi_df_ts, tsgf = "TSGFspline", approach = "White")

# time series pre-processing
evi_tsgf <- TsPP(evi_df_ts, tsgf = TSGFspline)
plot(evi_tsgf)

evi_tsgf_df <- evi_tsgf %>% 
  as_tibble() %>% 
  rename(evi = x) %>% 
  bind_cols(data.frame(date = seq(min(evi_df$date), max(evi_df$date), by = "day")) %>% 
              mutate(doy = lubridate::yday (date),
                     year = lubridate::year(date)) %>% 
              filter(doy <= 365) ) 
  
  
ggplot(evi_tsgf_df)+
  geom_line(aes(x = doy, y= evi, group = year, col=year))+
  geom_vline(xintercept =  mean(res$sos, na.rm = T)+49-1)+
  theme_classic()
