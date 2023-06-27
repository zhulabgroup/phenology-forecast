
evi_df <- read_rds(str_c(.path$rs_evi, area, ".ts.rds")) %>%
  filter(qa_good) %>%
  select(id, lon, lat, date, evi) %>%
  as_tibble()

View(evi_df)
# devtools::install_github("bluegreen-labs/phenor@v1.0")
# library(phenor)
# data("phenocam_DB")
# data("ndvi")
evi_df <- evi_df %>% mutate(
  year = as.numeric(format(date, format = "%Y")),
  month = as.numeric(format(date, format = "%m")),
  day = as.numeric(format(date, format = "%d")),
  doy = lubridate::yday(date)
)

# optimize_parameters()
# estimate_phenology()
View(evi_df)

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
  filter(date < "2007-12-27") %>%
  filter(date >= "2006-01-09") %>%
  ggplot() +
  geom_line(aes(x = date, y = evi, group = id),
    col = "darkgreen",
    alpha = 0.2
  ) +
  theme_classic()
