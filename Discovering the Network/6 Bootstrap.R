# 步骤6：方法2 - Bootstrap阈值

B <- 100  # bootstrap
q <- 0.05  # FDR

# bootstrap
t_boot_array <- array(0, dim = c(N, N*K, B))

# wild bootstrap
for (b in 1:B) {
  # bootstrap扰动
  epsilon <- sample(c(-1, 1), nrow(Y_t), replace = TRUE)
  
  Y_boot <- X %*% t(lasso_coef) + U_hat * epsilon
  
  # bootstrap lasso
  for (i in 1:N) {
    y_boot_i <- Y_boot[, i]
    
    # lasso
    lasso_boot <- glmnet(X, y_boot_i, alpha = 1, 
                         lambda = lambda_vals[i], 
                         intercept = FALSE)
    beta_boot <- as.numeric(coef(lasso_boot))[-1]
    
    u_boot_i <- y_boot_i - X %*% beta_boot
    
    correction_boot <- (t(u_boot_i) %*% X %*% Omega_hat) / nrow(X)
    debiased_boot <- beta_boot + correction_boot
    
    # bootstrap t
    for (j in 1:(N*K)) {
      se_boot <- sqrt(sum(u_boot_i^2) / (nrow(X) - sum(beta_boot != 0)) * Omega_hat[j, j])
      t_boot_array[i, j, b] <- sqrt(nrow(X)) * debiased_boot[j] / se_boot
    }
  }
}

# bootstrap
t_null <- t_boot_array[abs(lasso_coef) < 1e-10]  

find_boot_threshold <- function(t_stats, t_null, alpha) {
  t_abs_sorted <- sort(abs(as.numeric(t_stats)), decreasing = FALSE)
  
  for (t_candidate in t_abs_sorted) {
    discoveries <- sum(abs(t_stats) >= t_candidate)
    
    # Q(t)
    Q_boot <- mean(abs(t_null) >= t_candidate)
    
    fdr_est <- (length(t_stats) * Q_boot) / max(discoveries, 1)
    
    if (fdr_est <= alpha) {
      return(t_candidate)
    }
  }
  
  return(max(abs(t_null)))  
}

t0_boot <- find_boot_threshold(t_stats, t_null, q)

discoveries_method2 <- abs(t_stats) >= t0_boot