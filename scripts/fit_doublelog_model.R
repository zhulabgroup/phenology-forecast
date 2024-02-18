# load data set
load("~/phenology-forecast/data/df_model_pred.rda")
df_pheno_paras <- readRDS("~/phenology-forecast/data/df_pheno_paras.rds")
df_evi <- df_evi %>% filter(year <= 2021)
df_paras_v <- df_pheno_paras %>% filter(year <= 2017) %>%
  group_by(id) %>%
  summarise(vm1 = mean(m1_mean),
            vm1sd = sqrt(mean(m1_sd^2) + var(m1_mean)),
            vm2 = mean(m2_mean),
            vm2sd = sqrt(mean(m2_sd^2) + var(m2_mean)),
            vm3 = mean(m3_mean),
            vm3sd = sqrt(mean(m3_sd^2) + var(m3_mean)),
            vm4 = mean(m4_mean),
            vm4sd = sqrt(mean(m4_sd^2) + var(m4_mean)),
            vm5 = mean(m5_mean),
            vm5sd = sqrt(mean(m5_sd^2) + var(m5_mean)),
            vm6 = mean(m6_mean),
            vm6sd = sqrt(mean(m6_sd^2) + var(m6_mean)),
            vm7 = mean(m7_mean),
            vm7sd = sqrt(mean(m7_sd^2) + var(m7_mean)))
df <- left_join(df_pheno_paras %>% filter(year <= 2021),df_paras_v)
d <- df %>% filter(year >= 2018 & year <= 2021) %>% group_by(id) %>%
  mutate(m1_mean = vm1,
         m1_sd = vm1sd,
         m2_mean = vm2,
         m2_sd = vm2sd,
         m3_mean = vm3,
         m3_sd = vm3sd,
         m4_mean = vm4,
         m4_sd = vm4sd,
         m5_mean = vm5,
         m5_sd = vm5sd,
         m6_mean = vm6,
         m6_sd = vm6sd,
         m7_mean = vm7,
         m7_sd = vm7sd)
df_pheno_paras_v <- df_pheno_paras %>% filter(year <= 2017) %>% rbind(d[,1:29]) %>% 
  group_by(year) %>% arrange(id) %>% ungroup()
#usethis::use_data(df_pheno_paras_v)


pred_evi <- function(params, t ) {
  m1 <- as.numeric(params["m1_mean"])
  m2 <- as.numeric(params["m2_mean"])
  m3 <- as.numeric(params["m3_mean"])
  m4 <- as.numeric(params["m4_mean"])
  m5 <- as.numeric(params["m5_mean"])
  m6 <- as.numeric(params["m6_mean"])
  m7 <- as.numeric(params["m7_mean"])
  
  evi <- (m1 + (m2 - m7*t) * (1/(1 + exp((m3-t)/m4)) - 
                              1/(1 + exp((m5-t)/m6))))
  return(evi)
}


cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)
year_loop <- function(s,m,orig_paras,pred_paras){
  df_evi_plot <- data.frame()
  for(i in 1:22){
    df <- data.frame(t = 1:365,
                     id = s,
                     orig_evi = pred_evi(orig_paras[i,],1:365),
                     pred_evi = pred_evi(pred_paras[i,],1:365),
                     model = levels(df_model_pred$model)[m])
    df_evi_plot <- bind_rows(df_evi_plot,df)
  }
  return(df_evi_plot)
}

df_evi_plot <- foreach(s=1:100,.combine = "bind_rows",
                       .packages = c("tidyverse", "phenor")) %dopar% {
    df_evi_plot <- data.frame()
    orig_paras <- df_pheno_paras_v %>% filter(id == s)
    for(m in 1:9){
      pred_paras <- df_pheno_paras_v %>% filter(id == s) %>%
        mutate(m3_mean = df_model_pred %>% 
                filter(model == levels(df_model_pred$model)[m]) %>% 
                 filter(site == s) %>%
                  pull(pred)) 
      df_evi_plot <- rbind(df_evi_plot,year_loop(s,m,orig_paras,pred_paras))
    }
    return(df_evi_plot)
}
df_evi_plot <- data.frame(df_evi_plot)
stopCluster(cl)

date = df_climate %>% filter(id == 1) %>% dplyr::select(date)
df_evi_plot <- df_evi_plot %>% group_by(id,model)  %>%
  mutate(date) %>% 
  mutate(year = as.numeric(format(date, format = "%Y")))

evi_pt <- date %>% left_join(df_evi %>% filter(id == 1) %>% 
                               dplyr::select(date,evi), by = "date")
df_evi_plot <- df_evi_plot %>% left_join(evi_pt, by = "date")
usethis::use_data(df_evi_plot)

plot_pred <- df_evi_plot %>% filter(model %in%c("LIN","M1","PA") &
                                      id == 1 & year >= 2018) %>% rowwise() %>% 
  ggplot()+
  geom_line(aes(x = date, y = orig_evi),color = "black",alpha = 0.8)+
  geom_line(aes(x = date, y = pred_evi),color = "green",alpha = 0.6)+
  facet_wrap(.~model,ncol = 1) + ylab("EVI") + 
  geom_point(aes(x = date, y = evi),alpha = 0.4) +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype = "dashed",color = "blue")
library(cowplot)
plot_grid(plot_pred,ncol = 1)

# Calculate rmse for evi
df_evi_train_rmse <- df_evi_plot %>% filter(year <= 2017) %>%
  mutate(error = (pred_evi - evi)^2) %>% filter(t %in% c(100:130))%>%
  group_by(model,id) %>%
  summarise(rmse = mean(error,na.rm = TRUE) %>% sqrt(), .groups = "drop")

df_evi_test_rmse <- df_evi_plot %>% filter(year > 2017) %>%
  mutate(error = (pred_evi - evi)^2) %>% filter(t %in% c(100:130)) %>%
  group_by(model,id) %>%
  summarise(rmse = mean(error,na.rm = TRUE) %>% sqrt(), .groups = "drop")

ggplot(df_evi_train_rmse) +
  geom_boxplot(aes(x = model, y = rmse, color = model)) +
  theme_classic() +
  ylim(0.05,0.16) + 
  scale_x_discrete(limits = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")) +
  scale_color_manual(values = c(
    "LIN" = "black", "TT" = "orange",
    "PTT" = "orange", "M1" = "orange",
    "AT" = "blue", "SQ" = "blue",
    "SM1" = "blue", "PA" = "blue",
    "PM1" = "blue"
  )) +
  ylab("Training RMSE for EVI") +
  xlab("Models") +
  guides(color = FALSE)

ggplot(df_evi_test_rmse) +
  geom_boxplot(aes(x = model, y = rmse, color = model)) +
  theme_classic() +
  ylim(0.05, 0.16) +
  scale_x_discrete(limits = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")) +
  scale_color_manual(values = c(
    "LIN" = "black", "TT" = "orange",
    "PTT" = "orange", "M1" = "orange",
    "AT" = "blue", "SQ" = "blue",
    "SM1" = "blue", "PA" = "blue",
    "PM1" = "blue"
  )) +
  ylab("Vidation RMSE for EVI") +
  xlab("Models") +
  guides(color = FALSE)




for(s in 1:1){
  for(m in 1:9){
    orig_paras <- df_pheno_paras %>% filter(id == s)
    pred_paras <- df_pheno_paras %>% mutate(m3_mean = df_model_pred %>% 
                                              filter(model == levels(df_model_pred$model)[m]) %>%
                                              pull(pred)) %>% filter(id == s)
    for(i in 1:22){
      df <- data.frame(t = 1:365,
                       id = s,
                       orig_evi = pred_evi(orig_paras[i,],1:365),
                       pred_evi = pred_evi(pred_paras[i,],1:365),
                       model = levels(df_model_pred$model)[m])
      df_evi_plot <- bind_rows(df_evi_plot,df)
    }
  }
}

