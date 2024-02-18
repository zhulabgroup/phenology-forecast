library(phenor)
library(greenbrown)
library(tidyverse)

# load data

# data("phenocam_DB")
# View(phenocam_DB)

evi_DB <- read_rds(str_c(.path$dat_proc, "evi_DB.rds"))

# load default parameters
# comma separated parameter file inlcuded in the package
# for all the included models, this file is used by default
# in all optimization routines
path <- sprintf("%s/extdata/parameter_ranges.csv", path.package("phenor"))
par_ranges <- read.table(path,
  header = TRUE,
  sep = ","
)

v_model <- c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")

cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

# this step still takes pretty long
ls_df_model <-
  foreach(
    modoi = v_model,
    .packages = c("tidyverse", "phenor")
  ) %dopar% {
    # get range of model parameters
    par_range <- par_ranges %>%
      filter(model == modoi) %>%
      dplyr::select(-model) %>%
      gather(key = "param", value = "value", -boundary) %>%
      drop_na() %>%
      spread(key = "boundary", value = "value") %>%
      mutate(mean = (lower + upper) / 2) %>%
      mutate(param = factor(param,
        levels = par_ranges %>% colnames()
      )) %>%
      arrange(param)

    # optimize model parameters
    set.seed(1234)
    optim.par <- phenor::optimize_parameters( # par = par_range$mean,
      data = evi_DB,
      cost = rmse,
      model = modoi,
      method = "GenSA",
      lower = par_range$lower,
      upper = par_range$upper,
      control = NULL
    )

    # run the model for all data in the nested list using the estimated parameters
    modelled <- phenor::estimate_phenology(
      par = optim.par$par,
      data = evi_DB,
      model = modoi
    )

    # compare predictions with observations
    ls_df_pred <- vector(mode = "list")
    for (s in names(evi_DB)) {
      ls_df_pred[[s]] <- data.frame(
        site = s,
        year = evi_DB[[s]]$year,
        doy = evi_DB[[s]]$transition_dates
      )
    }
    df_pred <- bind_rows(ls_df_pred) %>%
      mutate(pred = modelled) %>%
      mutate(model = modoi)

    df_pred
  }
stopCluster(cl)

df_model_pred <- bind_rows(ls_df_model) %>%
  mutate(model = factor(model,
    levels = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")
  ))


# code for generate plot
df_model_pred <- df_model_pred  %>% group_by(model) %>%
  mutate(doy = df_pheno_paras %>% filter(year <=2021) %>% 
           pull(m3_mean) %>% round())
df_model_pred_train <- df_model_pred %>% filter(year < 2018)
df_model_pred_test <- df_model_pred %>% filter(year > 2017)

  
ggplot(df_model_pred_train) +
  geom_jitter(aes(x = doy, y = pred, col = site),alpha = 0.4) +
  theme_classic() +
  guides(col = "none") +
  facet_wrap(. ~ model) +
  ylab("Training pred doy") +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_equal()

ggplot(df_model_pred_test) +
  geom_jitter(aes(x = doy, y = pred, col = site),alpha = 0.5) +
  theme_classic() +
  guides(col = "none") +
  facet_wrap(. ~ model) +
  ylab("Validation pred doy") +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_equal()


df_model_train_rmse <- df_model_pred_train %>%
  mutate(error = (pred - doy)^2) %>%
  group_by(model, site) %>%
  summarise(rmse = mean(error) %>% sqrt(), .groups = "drop")

df_model_test_rmse <- df_model_pred_test %>%
  mutate(error = (pred - doy)^2) %>%
  group_by(model, site) %>%
  summarise(rmse = mean(error) %>% sqrt(), .groups = "drop")

ggplot(df_model_train_rmse) +
  geom_boxplot(aes(x = model, y = rmse, color = model)) +
  theme_classic() +
  ylim(0, 20) +
  scale_x_discrete(limits = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")) +
  scale_color_manual(values = c(
    "LIN" = "black", "TT" = "orange",
    "PTT" = "orange", "M1" = "orange",
    "AT" = "blue", "SQ" = "blue",
    "SM1" = "blue", "PA" = "blue",
    "PM1" = "blue"
  )) +
  ylab("RMSE (days) for training") +
  xlab("Models") +
  guides(color = FALSE)

ggplot(df_model_test_rmse) +
  geom_boxplot(aes(x = model, y = rmse, color = model)) +
  theme_classic() +
  ylim(0, 20) +
  scale_x_discrete(limits = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")) +
  scale_color_manual(values = c(
    "LIN" = "black", "TT" = "orange",
    "PTT" = "orange", "M1" = "orange",
    "AT" = "blue", "SQ" = "blue",
    "SM1" = "blue", "PA" = "blue",
    "PM1" = "blue"
  )) +
  ylab("RMSE (days) for validation") +
  xlab("Models") +
  guides(color = FALSE)
