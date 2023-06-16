#install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

# extract phenological metrics
df_metric <- evi_all <- data.frame()
df_metric <- data.frame(id=1:100,sos = NA, eos = NA)
sos_matrix <- matrix(NA,nrow = 100,ncol = 23)

for (i in 1:100){
  evi_ts <- evi_df %>% filter(id == i) %>%
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
  
  df_metric$sos[i] <- mean(res$sos,na.rm = T)+49-1
  df_metric$eos[i] <- mean(res$eos,na.rm = T)+49-1
  sos <- res$sos + 49-1
  eos <- res$eos + 49-1
  for(year in 2000:2022){
    sos_value = sos
    sos_matrix[i,year-2000+1] <- sos_value[year-1999]

  }
  
  # time series pre-processing
  evi_tsgf <- TsPP(evi_ts, tsgf = TSGFspline)
  
  # logistic approach
  #tsgf1 <- TSGFdoublelog(window(evi_ts,2000,c(2022,365)),interpolate = T,method = "Elmore", 
                         #check.seasonality = NULL)
  plot(evi_tsgf)
  lines(evi_tsgf,col="blue")
  
  evi_tsgf_df <- evi_tsgf %>% 
    as_tibble() %>% 
    rename(evi = x) %>% 
    bind_cols(data.frame(date = seq(min(evi_df$date), max(evi_df$date), 
                                    by = "day")) %>% 
                mutate(doy = lubridate::yday (date),
                       year = lubridate::year(date),
                       id = i) %>% filter(year <= 2022) %>%
                filter(doy <= 365) ) 
  evi_all <- bind_rows(evi_all,evi_tsgf_df)
  colnames(sos_matrix) <- 2000:2022
  rownames(sos_matrix) <- unique(evi_df$id)
  
  
  
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

# download daymet and reform evi_DB structure
evi_DB <- list()
for (i in evi_df[1:100,]$id){
  df_daymet <- daymetr::download_daymet(
    site = evi_df$id[i],
    lat = evi_df$lat[i],
    lon = evi_df$lon[i],
    start = lubridate::year(min(evi_df$date)),
    end = min(lubridate::year(max(evi_df$date))-1),
    internal = TRUE,
    simplify = TRUE
  ) %>%
    filter(measurement %in% c("dayl..s.","tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.", "vp..Pa.")) %>%
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
    mutate(dayl = dayl / 3600) %>% as_tibble()  %>%
    mutate(year = lubridate::year(date))
  
  df_daymet <- df_daymet[c("site", "date", "dayl","year", "temp", "tmax", "tmin","prcp", "vp")]
  evi_daymet <- evi_all %>% filter(id == i) %>% left_join(df_daymet, by = c("date","year"))
  evi_daymet <- as_tibble(evi_daymet)
  
  evi_temp <- evi_daymet %>% arrange(doy) %>% filter(site == i)
  evi_temp <- evi_temp[c("year","doy","temp")] %>% 
    pivot_wider(names_from = year,values_from = temp)
  evi_temp <- evi_temp[2:23]
  
  evi_ltm <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_ltm <- evi_ltm[c("year","doy","temp")] %>%
    pivot_wider(names_from = year,values_from = temp) 
  evi_ltm <- evi_ltm[2:23] %>%
    mutate(ltm = rowMeans(., na.rm = TRUE))
  
  evi_tmin <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_tmin <- evi_tmin[c("year","doy","tmin")] %>%
    pivot_wider(names_from = year,values_from = tmin) 
  evi_tmin <- evi_tmin[2:23]
  
  evi_tmax <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_tmax <- evi_tmax[c("year","doy","tmax")] %>%
    pivot_wider(names_from = year,values_from = tmax) 
  evi_tmax <- evi_tmax[2:23]
  
  evi_prcp <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_prcp <- evi_prcp[c("year","doy","prcp")] %>%
    pivot_wider(names_from = year,values_from = prcp) 
  evi_prcp <- evi_prcp[2:23]
  
  evi_vp <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_vp <- evi_vp[c("year","doy","vp")] %>%
    pivot_wider(names_from = year,values_from = vp) 
  evi_vp <- evi_vp[2:23]
  
  evi_dayl <- evi_daymet %>% arrange(doy) %>% filter(site == i) 
  evi_dayl <- evi_dayl[c("year","doy","dayl")] %>%
    pivot_wider(names_from = year,values_from = dayl) 
  evi_dayl <- evi_dayl
  
  
  
  site_list <- list()
  site_list[["site"]] <- unique(evi_daymet$site)
  site_list[["location"]] <- c(evi_df$lon[i],evi_df$lat[i])
  site_list[["doy"]] <- sort(unique(evi_daymet$doy))
  site_list[["ltm"]] <- evi_ltm$ltm
  site_list[["sos"]] <- unname(round(sos_matrix[1,])) 
  site_list[["year"]] <- unique(evi_daymet$year)
  site_list[["Ti"]] <- evi_temp
  site_list[["Tmini"]] <- evi_tmin
  site_list[["Tmaxi"]] <- evi_tmax
  site_list[["Li"]] <- evi_dayl
  site_list[["Pi"]] <- evi_prcp
  site_list[["VPDi"]] <- evi_vp
  
  
  name <- as.character(i)
  evi_DB[[name]] <- site_list
  
}
View(evi_temp)

View(evi_DB)
write_rds(evi_DB,"evi_DB.rds")



