model_pred <- read_rds(str_c(.path$dat_proc, "model_pred.rds"))
View(model_pred)

evi_all <- read_rds(str_c(.path$dat_proc, "evi_all.rds"))
evi_DB <- read_rds(str_c(.path$dat_proc, "evi_DB.rds"))

par_all <- read_rds(str_c(.path$dat_proc, "dlog_par_all.rds"))

# select one year of NDVI data
x <- as.vector(window(ndvi, start=c(1991,1), end=c(1991, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(evi_ts)
fit
plot(evi_ts)
lines(fit$predicted, col="blue")

# do more inital trials, plot iterations and compute parameter uncertainties
res <- FitDoubleLogBeck(evi_ts, plot=TRUE, ninit=100)	

data.frame(t = 1:365, 
           obs = evi_ts,
           pred = res$predicted) %>% 
  ggplot()+
  geom_point(aes(x = t, y = obs))+
  geom_line(aes(x = t, y = pred))

res$params
res$formula

PhenoDeriv(res$predicted, plot = TRUE)
# # fit double-logistic function to one year of data, 
# # interpolate to daily time steps and calculate phenology metrics
# tout <- seq(1, 12, length=365)	# time steps for output (daily)
# fit <- FitDoubleLogElmore(x, tout=tout)
# plot(x)
# lines(tout, fit$predicted, col="blue")
# PhenoDeriv(fit$predicted, plot=TRUE)
# res$params


pred_evi <- function(params, t ) {
  mn <- as.numeric(params["MN"])
  mx <- as.numeric(params["MX"])
  sos <- as.numeric(params["SOS"])
  eos <- as.numeric(params["EOS"])
  rsp <- as.numeric(params["RSP"])
  rau <- as.numeric(params["RAU"])
  
  
  evi <- (mn + (mx - mn) * (1/(1 + exp(-rsp * (t - sos))) + 
                              1/(1 + exp(rau * (t - eos)))))
  
  return(evi)
}


res <- FitDoubleLogBeck(evi_ts, plot=TRUE, ninit=100)	

data.frame(t = 1:365, 
           obs = evi_ts,
           pred = res$predicted) %>% 
  ggplot()+
  geom_point(aes(x = t, y = obs))+
  geom_line(aes(x = t, y = pred))



paras <- par_all%>% dplyr::select(id,year,MN,MX,SOS,EOS,RSP,RAU) %>% distinct()


para_est_df <- data.frame(
  Ctemp = numeric(0),
  Cprecip = numeric(0),
  Cmn = numeric(0),
  Cmx = numeric(0),
  Csos = numeric(0),
  Ceos = numeric(0),
  Crsp = numeric(0),
  Crau = numeric(0)
)

indiv_df <- list()
model_list <- list()
pred_all <- data.frame()
for(i in 1:100){
  evi_all_sub <- evi_all %>% filter(id == i)
  paras_sub <- paras %>% filter(id == i)
  temp = colMeans(evi_DB[[i]]$Ti)
  precip = colMeans(evi_DB[[i]]$Pi)
  mn = paras_sub$MN
  mx = paras_sub$MX
  eos = paras_sub$EOS
  rsp = paras_sub$RSP
  rau = paras_sub$RAU
  
  new_df <- data.frame(
    Ctemp = temp,
    Cprecip = precip,
    Cmn = mn,
    Cmx = mx,
    Ceos = eos,
    Crsp = rsp,
    Crau = rau
    
  )
  para_est_df <- bind_rows(para_est_df,new_df)
  indiv_df[[i]] <- new_df
}

fit_lm_maxevi<- lm(Cmx ~ Ctemp + Cprecip, data = para_est_df)
fit_lm_minevi<- lm(Cmn ~ Ctemp + Cprecip, data = para_est_df)
fit_lm_rsp<- lm(Crsp ~ Ctemp + Cprecip, data = para_est_df)
fit_lm_rau<- lm(Crau ~ Ctemp + Cprecip, data = para_est_df)
fit_lm_eos<- lm(Ceos ~ Ctemp + Cprecip, data = para_est_df)
model_list[[1]] <- fit_lm_maxevi
model_list[[2]] <- fit_lm_minevi
model_list[[3]] <- fit_lm_rsp
model_list[[4]] <- fit_lm_rau
model_list[[5]] <- fit_lm_eos

for(i in 1:100){
  pred_maxevi <- predict(model_list[[1]],indiv_df[[i]])
  pred_minevi <-predict(model_list[[2]],indiv_df[[i]])
  pred_rsp <-predict(model_list[[3]],indiv_df[[i]])
  pred_rau <-predict(model_list[[4]],indiv_df[[i]])
  pred_eos <-predict(model_list[[5]],indiv_df[[i]])
  for(m in 1:9){
    p <- model_pred %>% 
      filter(model==as.character(levels(model_pred$model)[m])) %>% 
      filter(site == i)
    df_pred_para <- data.frame(id = i,
                            model = p$model,
                             MX = pred_maxevi,
                             MN = pred_minevi,
                             RSP = pred_rsp,
                             RAU = pred_rau,
                             SOS = p$pred,
                             EOS = pred_eos)
    pred_all <- bind_rows(pred_all,df_pred_para)
  }

}






df_evi_plot <- data.frame()

for(s in 1:1){
  for(m in 1:9){
    orig_paras <- paras %>% filter(id == s)
    pred_paras <- pred_all %>% filter(id == s) %>% 
      filter(model == levels(model_pred$model)[m])
    for(i in 1:23){
      df <- data.frame(t = 1:365,
                       id = s,
                       orig_evi = pred_evi(orig_paras[i,],1:365),
                       pred_evi = pred_evi(pred_paras[i,],1:365),
                       model = levels(model_pred$model)[m])
      df_evi_plot <- bind_rows(df_evi_plot,df)
    }
  }
}

date = evi_all %>% filter(id %in% c(1)) %>% dplyr::select(date)
df_evi_plot <- df_evi_plot %>% group_by(id,model)  %>%
  mutate(date) %>% 
  mutate(year = as.numeric(format(date, format = "%Y")))

#write_rds(df_evi_plot, str_c(.path$dat_proc, "df_evi_plot.rds"))

# rmse for each model
df_evi_plot %>% group_by(model) %>% 
  mutate(err_evi = sqrt(mean(abs(pred_evi - orig_evi)^2))) %>% 
  ungroup %>%
  distinct(model,err_evi)

df_evi_plot <- df_evi_plot %>% left_join(evi_ts_all,by = "date")

evi_ts_all <- data.frame(evi_ts_all) %>% mutate(date)


plot_pred <- df_evi_plot %>% filter(model %in%c("LIN","AT","TT")) %>%
  rowwise() %>% 
  ggplot()+
  geom_line(aes(x = date, y = pred_evi),color = "green",alpha = 0.8) +
  geom_line(aes(x = date, y = orig_evi),color = "black",alpha = 0.8) +
  facet_wrap(.~model,ncol = 1) + ylab("EVI")+ 
geom_point(aes(x = date, y = evi_pt),alpha = 0.5)

library(cowplot)
plot_grid(plot_pred,ncol = 1)

x











month_mean_evi <- evi_all %>% filter(id==4) %>% group_by(month) %>%
  mutate(mean_evi_mon = sum(evi_tsgf)/(n_distinct(year)*n_distinct(day))) %>%
  dplyr::select(month,mean_evi_mon) %>% dplyr::distinct(mean_evi_mon)

evi <- month_mean_evi$mean_evi_mon

x <- as.vector(window(ndvi,start = c(1991,1),end = c(1991,12)))
plot(evi)
# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(evi)
fit
lines(fit$predicted, col = "blue")

# do more inital trials, plot iterations and compute parameter uncertainties
FitDoubleLogElmore(evi, hessian = TRUE, plot = TRUE, ninit = 1000)

# fit double-logistic function to one year of data,
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length = 365) # time steps for output (daily)
fit <- FitDoubleLogElmore(evi, tout = tout)
plot(evi)
lines(tout, fit$predicted, col = "blue")
PhenoDeriv(fit$predicted, plot = TRUE)[1]



model_pred %>% filter(site==4) %>% group_by(model) %>%
  mutate(pred_mean = sum(pred) / n_distinct(year),
         doy_mean = sum(doy,na.rm = TRUE) / n_distinct(year)) %>%
  dplyr::select(site,model,pred_mean,doy_mean) %>%
  dplyr::distinct(site,model,pred_mean,doy_mean)









View(evi_DB[[1]])
model_pred_test <- model_pred %>% filter(site == 1 & model == "LIN")
evi_all %>% filter(year==2001 & id == 1)
evi_DB_pred <- evi_DB[[1]]
evi_DB_pred$transition_dates = model_pred_test$pred
View(evi_DB_pred)

evi_all_test = evi_all %>% filter(id == 1)

evi_test <- evi_df %>% filter(id == 1)
install.packages("minpack.lm")
library(minpack.lm)
model <- nls(evi_all_test$evi_tsgf ~ 
        m1 + (m2-m7*evi_all_test$doy)*
          ((1/(1+exp(m3/m4-evi_all_test$doy)/(1/m4)))) - 
          (1/(1+exp((m5/m6-evi_all_test$doy)/(1/m6)))),
      data = evi_all_test,
      start = c(m1=0.1,m2=0.1,m3=0.1,m4=0.1,m5=0.1,m6=0.1,m7=0.1))



