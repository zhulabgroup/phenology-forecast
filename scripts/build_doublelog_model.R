model_pred <- read_rds(str_c(.path$dat_proc, "model_pred.rds"))
View(model_pred)

evi_all <- read_rds(str_c(.path$dat_proc, "evi_all.rds"))


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



