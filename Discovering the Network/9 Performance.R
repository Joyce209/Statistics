# 步骤9：性能评估

# 模拟
generate_true_network <- function(N, sparsity = 0.1) {
  true_net <- matrix(0, nrow = N, ncol = N)
  n_edges <- round(N^2 * sparsity)
  
  # 随机
  edges <- sample(1:(N^2), n_edges)
  for (e in edges) {
    i <- ((e-1) %% N) + 1
    j <- ceiling(e / N)
    true_net[i, j] <- 1
  }
  
  # 对角元0
  diag(true_net) <- 0
  
  return(true_net)  
}

evaluate_discoveries <- function(pred_net, true_net) {
  pred <- as.numeric(pred_net[, 1:ncol(true_net)])
  true <- as.numeric(true_net)
  
  TP <- sum(pred == 1 & true == 1)
  FP <- sum(pred == 1 & true == 0)
  TN <- sum(pred == 0 & true == 0)
  FN <- sum(pred == 0 & true == 1)
  
  fdr <- FP / max((TP + FP), 1)
  power <- TP / max((TP + FN), 1)
  accuracy <- (TP + TN) / length(pred)
  
  return(list(FDR = fdr, Power = power, Accuracy = accuracy,
              TP = TP, FP = FP, TN = TN, FN = FN))
}

# 生成网络
true_network <- generate_true_network(N, sparsity = 0.05)
eval_method1 <- evaluate_discoveries(discoveries_method1, true_network)
eval_method2 <- evaluate_discoveries(discoveries_method2, true_network)

cat("Method 1 - FDR:", eval_method1$FDR, " Power:", eval_method1$Power, "\n")
cat("Method 2 - FDR:", eval_method2$FDR, " Power:", eval_method2$Power, "\n")