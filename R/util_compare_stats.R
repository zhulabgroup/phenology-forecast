util_compare_stats <- function(obs_ori, pred_ori, obs = NULL, pred = NULL, range = NULL) {
  corr <- cor(obs_ori, pred_ori, use = "pairwise.complete.obs")
  R2 <- summary(lm(pred_ori ~ obs_ori))$r.squared
  RMSE <- sqrt(mean((obs_ori - pred_ori)^2, na.rm = T))
  
  if (!is.null(obs)) {
    nRMSE <- sqrt(mean((obs - pred)^2, na.rm = T))
  } else if (!is.null(range)) {
    nRMSE <- RMSE / range
  } else {
    nRMSE <- NA
  }
  
  out <- list(corr = corr, R2 = R2, nRMSE = nRMSE, RMSE = RMSE)
  return(out)
}