evi_df <- read_rds(str_c(.path$rs_evi, area, ".ts.rds")) %>%
  filter(qa_good) %>%
  select(id, lon, lat, date, evi) %>%
  as_tibble()

ggplot(evi_df) +
  geom_line(aes(x = date, y = evi, group = id), col = "darkgreen", alpha = 0.2) +
  theme_classic()
