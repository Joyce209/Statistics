# 步骤4：计算t-统计量

# 方差估计
sigma2_hat <- apply(U_hat, 2, function(u) sum(u^2) / (nrow(U_hat) - sum(lasso_coef[1,] != 0)))

# t
t_stats <- matrix(0, nrow = N, ncol = N*K)

for (i in 1:N) {
  for (j in 1:(N*K)) {
    se_ij <- sqrt(sigma2_hat[i] * Omega_hat[j, j])
    t_stats[i, j] <- sqrt(nrow(X)) * debiased_coef[i, j] / se_ij
  }
}