# 步骤7：方法3 - 稳健化（e-BH方法）

# t转换e-变量
transform_to_evalue <- function(t_stat, p = 2) {
  
  f_t <- abs(t_stat)^p
  
  # E[|Z|^]，Z~N(0,1)
  Ezp <- sqrt(2^p/pi) * gamma((p+1)/2)
  
  # e
  e_value <- f_t / Ezp
  
  return(e_value)
}

e_values <- apply(t_stats, c(1,2), function(x) transform_to_evalue(x, p = 2))

# e-BH
eBH_procedure <- function(e_values, alpha) {
  e_flat <- as.numeric(e_values)
  n_tests <- length(e_flat)
  
  # 排序
  e_sorted <- sort(e_flat, decreasing = TRUE)
  indices <- order(e_flat, decreasing = TRUE)
  
  h_star <- 0
  for (h in 1:n_tests) {
    if (1/e_sorted[h] <= alpha * h / n_tests) {
      h_star <- h
    } else {
      break
    }
  }
  
  if (h_star > 0) {
    discoveries_idx <- indices[1:h_star]
  } else {
    discoveries_idx <- integer(0)
  }
  
  # 索引
  discoveries_mat <- matrix(FALSE, nrow = nrow(e_values), ncol = ncol(e_values))
  if (h_star > 0) {
    for (idx in discoveries_idx) {
      i <- ((idx-1) %% nrow(e_values)) + 1
      j <- ceiling(idx / nrow(e_values))
      discoveries_mat[i, j] <- TRUE
    }
  }
  
  return(discoveries_mat)
}

discoveries_method3 <- eBH_procedure(e_values, alpha)