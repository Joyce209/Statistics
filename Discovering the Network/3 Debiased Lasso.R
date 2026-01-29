# 步骤3：去偏lasso

library(glasso)

# X的协方差矩阵
Sigma_x_hat <- cov(X)

# 方法1 固定lambda
lambda_val <- 0.1  # 调整此值来控制稀疏性

# glasso估计
glasso_result <- glasso(Sigma_x_hat, rho = lambda_val, penalize.diagonal = FALSE)

# 精度矩阵估计
Omega_hat <- glasso_result$wi

# 确保矩阵对称
Omega_hat <- (Omega_hat + t(Omega_hat)) / 2

# 添加小的正定扰动
min_eigen <- min(eigen(Omega_hat, symmetric = TRUE, only.values = TRUE)$values)
if (min_eigen <= 1e-10) {
  Omega_hat <- Omega_hat + diag(1e-8, ncol(Omega_hat))
  cat("已添加小扰动\n")
}

# 去偏lasso
debiased_coef <- matrix(0, nrow = N, ncol = N*K)


for (i in 1:N) {
  # 残差
  u_i <- U_hat[, i]
  
  # 公式: correction = (u_i' X Ω_hat) / T
  correction <- (t(u_i) %*% X %*% Omega_hat) / nrow(X)
  
  # 去偏估计 = lasso估计 + 修正项
  debiased_coef[i, ] <- lasso_coef[i, ] + correction
  
  setTxtProgressBar(pb, i)
}
close(pb)

cat("原始lasso非零系数数量:", sum(lasso_coef != 0), "\n")
cat("去偏lasso非零系数数量:", sum(debiased_coef != 0), "\n")
cat("精度矩阵条件数:", kappa(Omega_hat), "\n")

# 可视化
if (N <= 50) { 
  par(mfrow = c(1, 2))
  
  Omega_nonzero <- Omega_hat
  Omega_nonzero[abs(Omega_nonzero) < 1e-10] <- 0
  
  image(Matrix(Omega_nonzero != 0), 
        main = "精度矩阵非零模式",
        xlab = "变量", ylab = "变量",
        col.regions = c("white", "blue"))
  
  plot(as.vector(lasso_coef), as.vector(debiased_coef),
       xlab = "Lasso系数", ylab = "去偏系数",
       main = "Lasso vs 去偏系数",
       pch = 19, cex = 0.5, col = rgb(0, 0, 1, 0.3))
  abline(0, 1, col = "red", lty = 2)
  abline(h = 0, col = "gray", lty = 2)
  abline(v = 0, col = "gray", lty = 2)
  
  par(mfrow = c(1, 1))
}

# 交叉验证
if (FALSE) {  
  cat("\n使用交叉验证\n")
  
  # lambda网格
  lambda_grid <- exp(seq(log(0.01), log(0.5), length.out = 20))
  
  # 最优lambda
  best_lambda <- lambda_grid[1]
  best_bic <- Inf
  
  for (lambda_i in lambda_grid) {
    tryCatch({
      
      glasso_cv <- glasso(Sigma_x_hat, rho = lambda_i, penalize.diagonal = FALSE)
      Omega_cv <- glasso_cv$wi
      
      # BIC
      loglik <- -log(det(Omega_cv)) + sum(diag(Omega_cv %*% Sigma_x_hat))
      df <- sum(Omega_cv != 0) / 2  
      bic <- -2 * loglik + df * log(nrow(X))
      
      if (bic < best_bic) {
        best_bic <- bic
        best_lambda <- lambda_i
      }
    }, error = function(e) {
      # 跳过
    })
  }
  
  cat("最优lambda:", best_lambda, "\n")
  
  # 重新估计
  glasso_result <- glasso(Sigma_x_hat, rho = best_lambda, penalize.diagonal = FALSE)
  Omega_hat <- glasso_result$wi
  Omega_hat <- (Omega_hat + t(Omega_hat)) / 2
}