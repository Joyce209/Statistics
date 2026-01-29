# save

results <- list(
  lasso_coef = lasso_coef,
  debiased_coef = debiased_coef,
  t_statistics = t_stats,
  threshold_asymptotic = t0,
  threshold_bootstrap = t0_boot,
  discoveries_asymptotic = discoveries_method1,
  discoveries_bootstrap = discoveries_method2,
  discoveries_robust = discoveries_method3,
  evaluation = list(
    method1 = eval_method1,
    method2 = eval_method2
  )
)

saveRDS(results, "granger_causality_results.rds")

# CSV
write.csv(discoveries_method1, "causality_network_method1.csv")
write.csv(discoveries_method2, "causality_network_method2.csv")