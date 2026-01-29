# 步骤5：方法1 - 渐近正态分布阈值

alpha <- 0.05  # FDR控
H <- N * (N*K)  # 假设检验
a <- 3.001  

# 阈值
t_bar <- sqrt(2*log(H)) - a*log(log(H))

# t0
find_threshold <- function(t_stats, alpha, t_bar) {
  H <- length(t_stats)
  
  # t
  t_abs_sorted <- sort(abs(as.numeric(t_stats)), decreasing = FALSE)
  
  for (t_candidate in t_abs_sorted) {
    if (t_candidate > t_bar) break
    
    discoveries <- sum(abs(t_stats) >= t_candidate)
    
    # FDR估计
    Q_t <- 2 * (1 - pnorm(t_candidate))  # 双尾
    fdr_est <- (H * Q_t) / max(discoveries, 1)
    
    if (fdr_est <= alpha) {
      return(t_candidate)
    }
  }
  
  return(sqrt(2*log(H)))
}

t0 <- find_threshold(t_stats, alpha, t_bar)

discoveries_method1 <- abs(t_stats) >= t0