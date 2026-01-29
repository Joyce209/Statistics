# 步骤1：数据准备和VAR模型

library(glmnet)
library(BigVAR)
library(Matrix)

N <- 50  # 变量
T <- 200  # 时间点
K <- 1    # 滞后

# 模拟
set.seed(123)
Y <- matrix(rnorm(N * T), nrow = T, ncol = N)  # 时间序列矩阵

create_lag_matrix <- function(Y, K) {
  T <- nrow(Y)
  N <- ncol(Y)
  X_lag <- matrix(0, nrow = T - K, ncol = N * K)
  
  for (k in 1:K) {
    start_idx <- (k-1)*N + 1
    end_idx <- k*N
    X_lag[, start_idx:end_idx] <- Y[(K-k+1):(T-k), ]
  }
  
  Y_response <- Y[(K+1):T, ]
  list(X = X_lag, Y = Y_response)
}

data_lag <- create_lag_matrix(Y, K)
X <- data_lag$X  # 解释变量
Y_t <- data_lag$Y  # 响应变量