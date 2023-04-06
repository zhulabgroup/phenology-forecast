library(tidyverse)
library(phenor)
data("phenocam_DB")
path <- sprintf("%s/extdata/parameter_ranges.csv",path.package("phenor"))
path_ranges <- read.table(path,
                          header=TRUE,
                          sep=",")

set.seed(1234)

optim.par <- optimize_parameters(par = c(180,0,250,500,2.5),
                               data = phenocam_DB,
                               cost = rmse,
                               model = "AT",
                               method = "GenSA",
                               lower = c(1,-10,0,0,0),
                               upper = c(365,10,500,1000,5),
                               control=NULL)
modelled <- estimate_phenology(data = phenocam_DB,
                               par = optim.par$par, model = "AT")
df_list <- vector(mode = "list")
for(s in names(phenocam_DB)){
  df_list[[s]] <- data.frame(site = s,
                             year=phenocam_DB[[s]]$year,
                             doy = phenocam_DB[[s]]$transition_dates)
}

df <- bind_rows(df_list)
df_AT <- df %>% mutate(pred = modelled) %>% 
  mutate(rmse = sqrt(mean((doy - pred)^2))) %>%
  mutate(model = "AT")

View(df_PTT)
View(df_TT)
View(df_AT)
ggplot(df) + geom_jitter(aes(x=doy,y=pred,col=site)) + 
  theme_classic() + 
  guides(col="none") +
  facet_wrap(.~model)









