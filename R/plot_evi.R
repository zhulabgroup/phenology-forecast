plot_evi_explore <- function(df_evi, n_coord = 3, seed = 1) {
  set.seed(seed)
  p <- df_evi %>%
    filter(id %in% (df_evi %>% pull(id) %>% unique() %>% sample(n_coord))) %>%
    ggplot() +
    geom_line(aes(x = date, y = evi, col = label),
      alpha = 1
    ) +
    facet_wrap(. ~ id, ncol = 1) +
    labs(
      x = "Date",
      y = "EVI"
    ) +
    theme_classic()

  return(p)
}
