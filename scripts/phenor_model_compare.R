library(phenor)
library(greenbrown)
library(tidyverse)

evi_DB <- read_rds("evi_DB.rds")
View(evi_DB)

# load the included data using
data("phenocam_DB")
View(phenocam_DB)
# comma separated parameter file inlcuded in the package
# for all the included models, this file is used by default
# in all optimization routines
path <- sprintf("%s/extdata/parameter_ranges.csv",path.package("phenor"))
par_ranges <- read.table(path,
                         header = TRUE,
                         sep = ",")

# optimize model parameters
set.seed(1234)
optim.par <- optimize_parameters(par = c(0,0),
                               data = evi_DB,
                               cost = rmse,
                               model = "LIN",
                               method = "genoud",
                               lower = c(-1000,-1000),
                               upper = c(1000,1000),
                               control=NULL)
optim.par
?pr_fit_parameters


#SQ
c(180,180,0,2.5,2.5,7.5,1000,180)
c(1,1,-10,-5,-5,0,0,0)
c(365,365,10,10,10,15,2000,350)

#AT
c(180,0,250,500,2.5)
c(1,-10,0,0,0)
c(365,10,500,1000,5)

#PA
c(180,180,0,2.5,2.5,7.5,0.5,1000,175)
c(1,1,-10,-5,-5,0,0,0,0)
c(365,365,10,10,10,15,1,2000,350)

#UN
c(180,5,5,-10,-10,7.5,0,50,175)
c(1,0,0.1,-20,-20,-5,-10,0,0)
c(365,10,10,0.1,0.1,20,10,100,350)

#SM1
c(180,180,0,2.5,2.5,7.5,1000,175)
c(1,1,-10,-5,-5,0,0,0)
c(365,365,10,10,10,15,2000,350)

#PM1
c(180,0,2.5,2.5,7.5,0.5,1000,175)
c(1,-10,-5,-5,0,0,0,0)
c(365,10,10,10,15,1,2000,350)

#SGSI
c(-7.5,22.5,0.5,1000,3500,10,11)
c(-15,0,0,0,2000,8,10)
c(0,45,1,2000,5000,11.5,12)


# now run the model for all data in the nested list using the estimated parameters
modelled <- pr_predict(data = phenocam_DB, par = optim.par$par,model = "LIN")

# compare predictions with observations
df_list<-vector(mode="list")
for (s in names(phenocam_DB)) {
  df_list[[s]]<-data.frame(site=s,
                           year=phenocam_DB[[s]]$year,
                           doy=phenocam_DB[[s]]$transition_dates)
  
}

df<-bind_rows(df_list)
df_TT<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "TT") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_TT)

df_PTT<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "PTT") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_PTT)

df_AT<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "AT") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_AT)

df_SQ<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "SQ") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_SQ)

df_M1<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "M1") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_M1)

df_PA<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "PA") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_PA)

df_SM1<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "SM1") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_SM1)

df_PM1<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "PM1") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_PM1)

df_SGSI<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "SGSI") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_SGSI)

df_LIN<-df %>% 
  mutate(pred=modelled) %>% mutate(model = "LIN") %>%
  mutate(rmse = sqrt(mean((doy-pred)^2)))
View(df_LIN)


df_com <- rbind(df_LIN,df_TT,df_PTT,df_M1,df_AT,df_SQ,df_PA,df_SM1,df_PM1)
View(df_com)

# Reorder the levels of the model variable in your data frame
df_com$model <- factor(df_com$model, 
                       levels = c("LIN", "TT", "PTT", "M1","AT","SQ","SM1","PA","PM1"))


ggplot(df_com)+
  geom_jitter(aes(x=doy, y=pred, col=site))+
  theme_classic()+
  guides(col="none")+
  facet_wrap(.~model) + ylab("pred doy")

ggplot(df_com) + geom_boxplot(aes(x = model,y = rmse,color = model)) + 
  theme_classic() +
  ylim(0,20) + 
  scale_x_discrete(limits = c("LIN", "TT", "PTT", "M1","AT","SQ","SM1","PA","PM1")) +
  scale_color_manual(values = c("LIN" = "black", "TT" = "orange", 
                                "PTT" = "orange", "M1" = "orange",
                                "AT" = "blue","SQ" = "blue",
                                "SM1" = "blue","PA" = "blue",
                                "PM1" = "blue")) +  ylab("RMSE (days)") + xlab("Models") +
  guides(color = FALSE)



data(ndvi)
x <- as.vector(window(ndvi, start=c(1991,1), end=c(1991, 12)))
plot(x)

# fit double-logistic function to one year of data
fit <- FitDoubleLogElmore(x)
fit
plot(x)
lines(fit$predicted, col="blue")

# do more inital trials, plot iterations and compute parameter uncertainties
FitDoubleLogElmore(x, hessian=TRUE, plot=TRUE, ninit=1000)	

# fit double-logistic function to one year of data, 
# interpolate to daily time steps and calculate phenology metrics
tout <- seq(1, 12, length=365)	# time steps for output (daily)
fit <- FitDoubleLogElmore(x, tout=tout)
plot(x)
lines(tout, fit$predicted, col="blue")
PhenoDeriv(fit$predicted, plot=TRUE)

