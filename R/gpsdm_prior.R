PrepareInformedPriors <- function(distMat, focalsite = NULL, lags, neighbors, vars) {
  Amat <- distMat
  
  # Calculate distance from focal site to neighboring sites
  if (is.null(focalsite)) {
    focalsite <- 1
  }
  Ind <- order(Amat[focalsite, ])
  
  dist <- array(NA, dim = c(length(lags), length(neighbors), length(vars)))
  for (v in 1:length(vars)) {
    var <- vars[v]
    for (n in 1:length(neighbors)) {
      neighbor <- neighbors[n]
      site <- Ind[neighbor]
      dist[, n, v] <- Amat[focalsite, site]
    }
  }
  dim(dist) <- c(1, length(lags) * length(neighbors) * length(vars))
  
  hnormvars <- matrix(NA, nrow = length(dist), ncol = 1)
  for (j in 1:length(dist)) {
    hnormvars[j, ] <- pi / 2 * (1 / sqrt(4 * v * (((j - 1) %% length(lags)) + 1)) * exp(-dist[j]^2 / (2 * v * (((j - 1) %% length(lags)) + 1))))^2
  }
  
  return(hnormvars)
}