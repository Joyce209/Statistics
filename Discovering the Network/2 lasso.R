# 步骤2：wise lasso

lasso_coef <- matrix(0, nrow = N, ncol = N*K)
lambda_vals <- numeric(N)

# lasso回归
for (i in 1:N) {
  y_i <- Y_t[, i]
  
  # lambda
  cv_fit <- cv.glmnet(X, y_i, alpha = 1, intercept = FALSE)
  lambda_i <- cv_fit$lambda.min
  
  # 拟合lasso模型
  lasso_fit <- glmnet(X, y_i, alpha = 1, lambda = lambda_i, intercept = FALSE)
  lasso_coef[i, ] <- as.numeric(coef(lasso_fit))[-1]  # 去掉截距项
  
  lambda_vals[i] <- lambda_i
}

# 残差
U_hat <- Y_t - X %*% t(lasso_coef)