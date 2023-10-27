GPSDM <- function(pars, distMat, basisX, basisP, basisD, basisY = NULL, newX = NULL, newSigma = NULL, newP = NULL, newD = NULL, newY = NULL, priors = NULL, priors_tr = NULL, Cinv.fixed = NULL, m.fixed = NULL, xnew = NULL, vnew = NULL, P_forecast = NULL, focalsites = NULL, start = NULL, steps = NULL, lags = NULL, neighbors = NULL, vars = NULL, C_g = NULL, mode = c("basics", "optimize", "predict", "forecast", "update")) {
  X <- basisX
  Y <- basisY
  P <- basisP
  D <- basisD
  J <- date2doy(D)
  ndim <- ncol(X)
  
  # transform parameters from real line to constrained space
  phi <- (phimax - phimin) / (1 + exp(-pars[1:ndim, , drop = F])) + phimin
  ve <- (vemax - vemin) / (1 + exp(-pars[ndim + 1])) + vemin
  tau <- (taumax - taumin) / (1 + exp(-pars[ndim + 2])) + taumin
  gamma1 <- (gamma1max - gamma1min) / (1 + exp(-pars[ndim + 3])) + gamma1min
  gamma2 <- (gamma2max - gamma2min) / (1 + exp(-pars[ndim + 4])) + gamma2min
  
  if (!is.null(Cinv.fixed) & !is.null(m.fixed) & mode != "optimize") { # must recalculate when optimizing
    Cinv <- Cinv.fixed
    m <- m.fixed
  } else {
    # construct base covariance matrix
    # kXX
    lC0 <- 0
    DD1 <- vector(mode = "list")
    for (i in 1:ndim) {
      DD1[[i]] <- abs(X[, i, drop = FALSE] %*% matrix(1, nrow = 1, ncol = nrow(X)) - matrix(1, nrow = nrow(X), ncol = 1) %*% t(X[, i, drop = FALSE]))^2
      lC0 <- lC0 - 0.5 * phi[i] * DD1[[i]]
    }
    kXX <- tau * exp(lC0)
    Dist1 <- matrix(NA, nrow = nrow(X), ncol = nrow(X))
    for (i in 1:nrow(X)) {
      for (j in 1:nrow(X)) {
        Dist1[i, j] <- distMat[P[i], P[j]]
      }
    }
    kXX <- kXX * exp(-Dist1^2 * gamma1)
    tDist1 <- matrix(NA, nrow = nrow(X), ncol = nrow(X))
    for (i in 1:nrow(X)) {
      for (j in 1:nrow(X)) {
        tDist1[i, j] <- tdistMat[J[i], J[j]]
      }
    }
    kXX <- kXX * exp(-tDist1^2 * gamma2)
    
    Id <- diag(1, nrow = nrow(X), ncol = nrow(X))
    C <- kXX + ve * Id
    C <- (C + t(C)) / 2
    L <- chol(C)
    Linv <- solve(L)
    Cinv <- tcrossprod(Linv)
    m <- Cinv %*% Y
  }
  
  if (mode == "basics") {
    output <- list(Cinv = Cinv, m = m)
    return(output)
  }
  
  if (!is.null(newX) & !is.null(newP)) {
    Xt <- newX
    Pt <- newP
    Dt <- newD
    Jt <- date2doy(Dt)
    
    # kXtX
    lC0 <- 0
    DD2 <- vector(mode = "list")
    for (i in 1:ndim) {
      DD2[[i]] <- abs(Xt[, i, drop = FALSE] %*% matrix(1, nrow = 1, ncol = nrow(X)) - matrix(1, nrow = nrow(Xt), ncol = 1) %*% t(X[, i, drop = FALSE]))^2
      lC0 <- lC0 - 0.5 * phi[i] * DD2[[i]]
    }
    kXtX <- tau * exp(lC0)
    Dist2 <- matrix(NA, nrow = nrow(Xt), ncol = nrow(X))
    for (i in 1:nrow(Xt)) {
      for (j in 1:nrow(X)) {
        Dist2[i, j] <- distMat[Pt[i], P[j]]
      }
    }
    kXtX <- kXtX * exp(-Dist2^2 * gamma1)
    tDist2 <- matrix(NA, nrow = nrow(Xt), ncol = nrow(X))
    for (i in 1:nrow(Xt)) {
      for (j in 1:nrow(X)) {
        tDist2[i, j] <- tdistMat[Jt[i], J[j]] #+abs(rnorm(1,0,0.01))
      }
    }
    kXtX <- kXtX * exp(-tDist2^2 * gamma2)
    kXXt <- t(kXtX)
    
    # kXtXt
    lC0 <- 0
    DD3 <- vector(mode = "list")
    for (i in 1:ndim) {
      DD3[[i]] <- abs(Xt[, i, drop = FALSE] %*% matrix(1, nrow = 1, ncol = nrow(Xt)) - matrix(1, nrow = nrow(Xt), ncol = 1) %*% t(Xt[, i, drop = FALSE]))^2
      lC0 <- lC0 - 0.5 * phi[i] * DD3[[i]]
    }
    kXtXt <- tau * exp(lC0)
    Dist3 <- matrix(NA, nrow = nrow(Xt), ncol = nrow(Xt))
    for (i in 1:nrow(Xt)) {
      for (j in 1:nrow(Xt)) {
        Dist3[i, j] <- distMat[Pt[i], Pt[j]]
      }
    }
    kXtXt <- kXtXt * exp(-Dist3^2 * gamma1)
    tDist3 <- matrix(NA, nrow = nrow(Xt), ncol = nrow(Xt))
    for (i in 1:nrow(Xt)) {
      for (j in 1:nrow(Xt)) {
        tDist3[i, j] <- tdistMat[Jt[i], Jt[j]] #+abs(rnorm(1,0,0.01))
      }
    }
    kXtXt <- kXtXt * exp(-tDist3^2 * gamma2)
    
    Id <- diag(1, nrow = nrow(Xt), ncol = nrow(Xt))
    mt <- kXtX %*% m
    Ct <- kXtXt - kXtX %*% Cinv %*% kXXt + ve * Id
    Ct <- (Ct + t(Ct)) / 2
    
    Lt <- chol(Ct)
    Ltinv <- solve(Lt)
    Ctinv <- tcrossprod(Ltinv)
  }
  
  if (mode == "predict") {
    output <- list(mt = matrix(mt), Ct = matrix(diag(Ct)))
    return(output)
  }
  
  if (mode == "optimize") {
    if (!is.null(newY)) {
      Yt <- newY
      # likelihood
      like <- -.5 * t(Yt - mt) %*% Ctinv %*% (Yt - mt) - sum(log(diag(Lt)))
      
      # gradient
      dl <- matrix(0, nrow = ndim + 4, ncol = 1)
      vQ <- matrix(tcrossprod(Ctinv %*% (Yt - mt)) - Ctinv, nrow = 1)
      dC <- vector(mode = "list")
      for (i in 1:ndim) {
        dC[[i]] <- -0.5 * DD3[[i]] * kXtXt + 0.5 * (DD2[[i]] * kXtX) %*% Cinv %*% kXXt + 0.5 * kXtX %*% Cinv %*% (t(DD2[[i]]) * kXXt) - 0.5 * kXtX %*% Cinv %*% (DD1[[i]] * kXX) %*% Cinv %*% kXXt
        dl[i, 1] <- .5 * vQ %*% matrix(dC[[i]], ncol = 1)
      }
      dC[[ndim + 1]] <- diag(1, nrow = nrow(Xt), ncol = nrow(Xt)) + kXtX %*% Cinv %*% Cinv %*% kXXt
      dl[ndim + 1, 1] <- .5 * vQ %*% matrix(dC[[ndim + 1]], ncol = 1)
      dC[[ndim + 2]] <- kXtXt / tau - (kXtX / tau) %*% Cinv %*% kXXt - kXtX %*% Cinv %*% (kXXt / tau) + kXtX %*% Cinv %*% (kXX / tau) %*% Cinv %*% kXXt
      dl[ndim + 2, 1] <- .5 * vQ %*% matrix(dC[[ndim + 2]], ncol = 1)
      dC[[ndim + 3]] <- kXtXt * (-Dist3^2) - (kXtX * (-Dist2^2)) %*% Cinv %*% kXXt - kXtX %*% Cinv %*% (kXXt * (-t(Dist2)^2)) + kXtX %*% Cinv %*% (kXX * (-Dist1^2)) %*% Cinv %*% kXXt
      dl[ndim + 3, 1] <- .5 * vQ %*% matrix(dC[[ndim + 3]], ncol = 1)
      dC[[ndim + 4]] <- kXtXt * (-tDist3^2) - (kXtX * (-tDist2^2)) %*% Cinv %*% kXXt - kXtX %*% Cinv %*% (kXXt * (-t(tDist2)^2)) + kXtX %*% Cinv %*% (kXX * (-tDist1^2)) %*% Cinv %*% kXXt
      dl[ndim + 4, 1] <- .5 * vQ %*% matrix(dC[[ndim + 4]], ncol = 1)
    }
    
    # derivative for hyperparameters wrt transformed hyperparameters -- for gradient calculation
    dpars <- matrix(c(
      (phi - phimin) * (1 - (phi - phimin) / (phimax - phimin)),
      (ve - vemin) * (1 - (ve - vemin) / (vemax - vemin)),
      (tau - taumin) * (1 - (tau - taumin) / (taumax - taumin)),
      (gamma1 - gamma1min) * (1 - (gamma1 - gamma1min) / (gamma1max - gamma1min)),
      (gamma2 - gamma2min) * (1 - (gamma2 - gamma2min) / (gamma2max - gamma2min))
    ))
    
    if (is.null(priors) & is.null(priors_tr)) {
      lpost <- like
      neglpost <- -lpost
      
      # J is gradient in parameter space - need gradient in transformed parameters
      GradLpost <- (dl) * dpars
      neglgrad <- -GradLpost
    } else if (!is.null(priors) & is.null(priors_tr)) {
      # phi
      lp_phi <- -.5 * sum(((phi - priors$E_phi)^2) / priors$V_phi)
      dlp_phi <- -(phi - priors$E_phi)^1 / priors$V_phi
      if (!is.null(priors$E_ve)) {
        lp_tau <- -.5 * sum(((tau - priors$E_tau)^2) / priors$V_tau)
        dlp_tau <- -(tau - priors$E_tau)^1 / priors$V_tau
        lp_ve <- -.5 * sum(((ve - priors$E_ve)^2) / priors$V_ve)
        dlp_ve <- -(ve - priors$E_ve)^1 / priors$V_ve
        lp_gamma1 <- -.5 * sum(((gamma1 - priors$E_gamma1)^2) / priors$V_gamma1)
        dlp_gamma1 <- -(gamma1 - priors$E_gamma1)^1 / priors$V_gamma1
        lp_gamma2 <- -.5 * sum(((gamma2 - priors$E_gamma2)^2) / priors$V_gamma2)
        dlp_gamma2 <- -(gamma2 - priors$E_gamma2)^1 / priors$V_gamma2
      }
      
      lp <- (lp_phi +
               lp_ve +
               lp_tau +
               lp_gamma1 +
               lp_gamma2
      )
      dlp <- matrix(c(
        dlp_phi,
        dlp_ve,
        dlp_tau,
        dlp_gamma1,
        dlp_gamma2
      ))
      
      lpost <- like + lp
      neglpost <- -lpost
      
      # J is gradient in parameter space - need gradient in transformed parameters
      GradLpost <- (dl + dlp) * dpars
      neglgrad <- -GradLpost
    }
    output <- list(neglpost = neglpost, neglgrad = neglgrad)
    return(output)
  }
}