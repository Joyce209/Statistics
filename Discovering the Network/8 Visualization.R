# 步骤8：结果可视化

library(igraph)
library(ggplot2)

create_causality_network <- function(discoveries_matrix, variable_names = NULL) {
  N <- nrow(discoveries_matrix)
  
  # 邻接矩阵
  adj_matrix <- matrix(0, nrow = N, ncol = N)
  
  # 当期
  adj_matrix <- discoveries_matrix[, 1:N]
  
  # igraph
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
  
  if (is.null(variable_names)) {
    V(g)$label <- paste0("V", 1:N)
  } else {
    V(g)$label <- variable_names
  }
  
  V(g)$size <- 10
  V(g)$color <- "lightblue"
  E(g)$color <- "gray"
  E(g)$arrow.size <- 0.3
  
  return(g)
}

network_result <- create_causality_network(discoveries_method1)
plot(network_result, 
     layout = layout_with_fr,
     main = "Granger Causality Network (Method 1)")