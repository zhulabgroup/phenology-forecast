fmingrad_Rprop <- function(fun, xinit, sample_n, maxcount) {
  # fun is a handle to a function that has grad as optional second output
  # this uses the sign of the gradient to determine descent directions and an adaptive step size - supposedly smoother convergence than conjugate gradients for GP optimization
  p <- length(xinit)
  
  # optimization parameters for Rprop
  Delta0 <- 0.1 * matrix(1, nrow = p, ncol = 1)
  Deltamin <- 1e-6 * matrix(1, nrow = p, ncol = 1)
  Deltamax <- 50 * matrix(1, nrow = p, ncol = 1)
  eta_minus <- 0.5
  eta_minus <- eta_minus - 1
  eta_plus <- 1.2
  eta_plus <- eta_plus - 1
  
  # initialize
  x <- xinit
  seed <- 1
  init_res <- fun(xinit, sample_n, seed)
  f <- init_res$neglpost
  g <- init_res$neglgrad
  s <- sqrt(t(g) %*% g)
  
  # loop starts here
  count <- 1
  del <- Delta0
  df <- 10
  
  while ((s > .0001) & (count <= maxcount) & (df > .0000001)) {
    # step 1-move
    xnew <- x - sign(g) * del
    new_res <- fun(xnew, sample_n, seed)
    fnew <- new_res$neglpost
    gnew <- new_res$neglgrad
    s <- sqrt(t(gnew) %*% gnew)
    df <- abs(fnew / f - 1)
    
    # step 2 - update step size
    gc <- g * gnew
    del_max <- matrix(apply(cbind(Deltamin, del * (1 + eta_plus * (gc > 0) + eta_minus * (gc < 0))), MARGIN = c(1), max))
    del <- matrix(apply(cbind(Deltamax, del_max), MARGIN = c(1), min))
    
    x <- xnew
    g <- gnew
    f <- fnew
    print(paste0("seed: ", seed, ", iteration: ", count))
    count <- count + 1
    if (count %% 10 == 1) {
      seed <- seed + 1
    }
  }
  
  res <- fun(x, sample_n, seed)
  output <- list(xopt = x, fopt = res$neglpost, gradopt = res$neglgrad)
  return(output)
}