
evi_df <- read_rds(str_c(.path$rs_evi, area, ".ts.rds")) %>%
  filter(qa_good) %>%
  select(id, lon, lat, date, evi) %>%
  as_tibble()


#devtools::install_github("bluegreen-labs/phenor@v1.0")
#library(phenor)
#data("phenocam_DB")
#data("ndvi")


#optimize_parameters()
#estimate_phenology()


evi_df_max <- evi_df %>%
  group_by(id, date, year, month) %>%
  summarise(evi_max = max(evi)) %>%
  group_by(id, year) %>%
  filter(evi_max == max(evi_max)) %>%
  group_by(year) %>%
  count(month)

View(evi_df_max)


evi_df_max %>% ggplot(aes(x = month, y = n)) +
  geom_col()

evi_df_m <- evi_df %>%
  group_by(id, date, year, month) %>%
  summarise(evi_max = max(evi)) %>%
  group_by(id, year) %>%
  filter(evi_max == max(evi_max)) %>%
  group_by(id) %>%
  count(month)

View(evi_df_m)

evi_df_m %>% ggplot(aes(x = month, y = n, color = id)) +
  geom_point()

evi_df %>%
  filter(date < "2001-12-27") %>%
  ggplot() +
  geom_line(aes(x = month, y = evi, group = id),
    col = "darkgreen",
    alpha = 0.2
  ) +
  theme_classic()
