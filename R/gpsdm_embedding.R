PrepareEmbedding <- function(x, start, end, focalsites = NULL, lags, neighbors, vars, distMat) {
  numPatch <- dim(x)[[1]]
  Amat <- distMat
  
  if (is.null(focalsites)) {
    focalsites <- seq(1, numPatch)
  }
  
  Xi_list <- Yi_list <- Pi_list <- Di_list <- vector(mode = "list", length = length(focalsites))
  for (i in focalsites) {
    Ind <- order(Amat[i, ])
    Ind <- c(i, setdiff(Ind, i)) # make sure focal site is always the first
    
    Xi <- matrix(NA, nrow = (end - start + 1), ncol = 0)
    for (v in 1:length(vars)) {
      var <- var_list[vars[v]]
      for (n in 1:length(neighbors[[v]])) {
        neighbor <- neighbors[[v]][n]
        site <- Ind[neighbor]
        if (!is.null(lags[[v]])) {
          for (period in 1:length(lags[[v]])) {
            period_lags <- lags[[v]][[period]]
            values <- matrix(NA, nrow = end - start + 1, ncol = length(period_lags))
            for (l in 1:length(period_lags)) {
              period_lag <- period_lags[l]
              values[, l] <- x[site, (start - period_lag):(end - period_lag), var]
            }
            Xi_add <- as.matrix(rowMeans(values, na.rm = T))
            colnames(Xi_add) <- paste0(var, "_", neighbor, "_", period)
            Xi <- cbind(Xi, Xi_add)
          }
        }
      }
    }
    Xi_list[[i]] <- Xi
    
    Yi <- matrix(x[i, start:end, 1, drop = F]) # Assuming the first var is always same as the var to predict
    Yi_list[[i]] <- Yi
    
    Pi <- matrix(i, nrow = (end - start + 1), ncol = 1)
    Pi_list[[i]] <- Pi
    
    Di <- matrix(dimnames(x[i, start:end, 1, drop = F])[[2]])
    Di_list[[i]] <- Di
  }
  
  X <- do.call(rbind, Xi_list)
  Y <- do.call(rbind, Yi_list)
  P <- do.call(rbind, Pi_list)
  D <- do.call(rbind, Di_list)
  
  output <- list(X = X, Y = Y, P = P, D = D)
  return(output)
}
