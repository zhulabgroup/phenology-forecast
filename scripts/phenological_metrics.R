#install.packages("greenbrown", repos = "http://R-Forge.R-project.org", dependencies = FALSE)
library(greenbrown)

data(ndvi)
winter <- (1:length(ndvi))[cycle(ndvi)==1|cycle(ndvi)==2|cycle(ndvi)==12]
ndvi[sample(winter,length(winter)*0.5)] <- NA
plot(ndvi)
spl.trs <- Phenology(ndvi,tsgf="TSGFspline",approach="White")
spl.trs
plot(spl.trs$peak,col = "red");lines(ndvi)
plot(spl.trs,tyle = c("LOS"))
plot(spl.trs,type = c("msp","mgs","mau","peak"))
plot(spl.trs$series,col="red");lines(ndvi)

evi_all <- data.frame()
res_ls <- c()

for (i in 4:4){
  evi_ts <- evi_df %>%
    filter(id == i) %>%
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
  
  res<-Phenology(evi_ts, tsgf = "TSGFspline", approach = "White")
  res_ls <- append(res_ls,res)
  
  # time series pre-processing
  evi_tsgf <- TsPP(evi_ts, tsgf = TSGFspline)
  #plot(evi_tsgf)
  
  evi_tsgf_df <- evi_tsgf %>% 
    as_tibble() %>% 
    rename(evi = x) %>% 
    bind_cols(data.frame(date = seq(min(evi_df$date), max(evi_df$date), by = "day")) %>% 
                mutate(doy = lubridate::yday (date),
                       year = lubridate::year(date),
                       id = i) %>% 
                filter(doy <= 365) ) 
  evi_all <- bind_rows(evi_all,evi_tsgf_df)
  
  
  
}
View(evi_all)

#sos_means <- lapply(res_ls,function(x)mean(x$sos,na.rm=T))
#evi_all %>% group_by(id) %>% 
  #mutate(sos_mean = sos_means,each=length(res_ls[[1]]$sos))
ggplot(evi_all)+
  geom_line(aes(x = doy, y= evi, group = c(year), col=year))+
  geom_vline(xintercept =  mean(res$sos, na.rm = T)+49-1)+
  geom_vline(xintercept = mean(res$eos,na.rm = T)+49-1)+
  theme_classic()+ facet_wrap(.~id)

