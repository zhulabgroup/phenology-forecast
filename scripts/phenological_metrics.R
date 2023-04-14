#install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

df_metric <- evi_all <- data.frame()
df_metric <- data.frame(id=1:12,sos = NA, eos = NA)

for (i in 1:12){
  evi_ts <- evi_df %>%
    filter(id == 12) %>%
    tidyr::complete(date = seq(min(date), max(date), by = "day")) %>%
    mutate(
      year = as.numeric(format(date, format = "%Y")),
      month = as.numeric(format(date, format = "%m")),
      day = as.numeric(format(date, format = "%d")),
      doy = lubridate::yday(date)
    ) %>% filter(year <= 2022) %>% 
    filter(doy <= 365) %>%
    pull(evi) %>% 
    ts(frequency = 365, start= c(2000,49))
  
  #fit <- FitDoubleLogElmore(evi_tsgf)
  #fit
  #lines(fit$predicted,col = "blue")
  
  res<-Phenology(evi_ts, tsgf = "TSGFspline", approach = "White")
  
  df_metric$sos[1] <- mean(res$sos,na.rm = T)+49-1
  df_metric$eos[1] <- mean(res$eos,na.rm = T)+49-1
  
  
  # time series pre-processing
  evi_tsgf <- TsPP(evi_ts, tsgf = TSGFspline)
  tsgf1 <- TSGFdoublelog(window(evi_ts,2000,c(2022,365)),interpolate = T,method = "Elmore", 
                         check.seasonality = NULL)
  plot(evi_tsgf)
  lines(tsgf1,col="blue")
  
  evi_tsgf_df <- tsgf1 %>% 
    as_tibble() %>% 
    rename(evi = x) %>% 
    bind_cols(data.frame(date = seq(min(evi_df$date), max(evi_df$date), 
                                    by = "day")) %>% 
                mutate(doy = lubridate::yday (date),
                       year = lubridate::year(date),
                       id = 1) %>% filter(year <= 2022) %>%
                filter(doy <= 365) ) 
  evi_all <- bind_rows(evi_all,evi_tsgf_df)
  
  
  
  
}
evi_all <- inner_join(evi_all,df_metric,by="id")
View(evi_all)

ggplot()+
  geom_line(data = evi_all,
            aes(x = doy, y= evi, group = c(year), col=year))+
  geom_vline(data = df_metric,
             aes(xintercept = sos))+
  geom_vline(data = df_metric,
             aes(xintercept = eos))+
  theme_classic()+ facet_wrap(.~id)

