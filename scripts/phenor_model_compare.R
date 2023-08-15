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

df_model <- bind_rows(ls_df_model) %>%
  mutate(model = factor(model,
    levels = c("LIN", "TT", "PTT", "M1", "AT", "SQ", "SM1", "PA", "PM1")
  ))

#write_rds(df_model,str_c(.path$dat_proc,"model_pred.rds"))

ggplot(df_model) +
  geom_jitter(aes(x = doy, y = pred, col = site)) +
  theme_classic() +
  guides(col = "none") +
  facet_wrap(. ~ model) +
  ylab("pred doy") +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_equal()


df_model_rmse <- df_model %>%
  mutate(error = (pred - doy)^2) %>%
  group_by(model, site) %>%
  summarise(rmse = mean(error, na.rm = T) %>% sqrt(), .groups = "drop")

ggplot(df_model_rmse) +
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
  ylab("RMSE (days)") +
  xlab("Models") +
  guides(color = FALSE)


# Ziyu, I'm not using any of these below because I automated them, but feel free to make changes.
# SQ
c(180, 180, 0, 2.5, 2.5, 7.5, 1000, 180)
c(1, 1, -10, -5, -5, 0, 0, 0)
c(365, 365, 10, 10, 10, 15, 2000, 350)

# AT
c(180, 0, 250, 500, 2.5)
c(1, -10, 0, 0, 0)
c(365, 10, 500, 1000, 5)

# PA
c(180, 180, 0, 2.5, 2.5, 7.5, 0.5, 1000, 175)
c(1, 1, -10, -5, -5, 0, 0, 0, 0)
c(365, 365, 10, 10, 10, 15, 1, 2000, 350)

# UN
c(180, 5, 5, -10, -10, 7.5, 0, 50, 175)
c(1, 0, 0.1, -20, -20, -5, -10, 0, 0)
c(365, 10, 10, 0.1, 0.1, 20, 10, 100, 350)

# SM1
c(180, 180, 0, 2.5, 2.5, 7.5, 1000, 175)
c(1, 1, -10, -5, -5, 0, 0, 0)
c(365, 365, 10, 10, 10, 15, 2000, 350)

# PM1
c(180, 0, 2.5, 2.5, 7.5, 0.5, 1000, 175)
c(1, -10, -5, -5, 0, 0, 0, 0)
c(365, 10, 10, 10, 15, 1, 2000, 350)

# SGSI
c(-7.5, 22.5, 0.5, 1000, 3500, 10, 11)
c(-15, 0, 0, 0, 2000, 8, 10)
c(0, 45, 1, 2000, 5000, 11.5, 12)
