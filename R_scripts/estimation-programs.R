

########################### POIRIER FUNCTION ###########################

POIRIER <- function(data, X8, Z8, X5, Z5, FE, M, X, FE_vars, J_vars, cluster_var = NULL) {
  
  # Step 1: Estimate bivariate probit model
  formula_1 <- as.formula(paste("Mt8 ~", paste(c(X8, Z8, FE), collapse = " + ")))
  formula_2 <- as.formula(paste("Mt5 ~", paste(c(X5, Z5, FE), collapse = " + ")))
  
  biprobit_model <- selection(formula_1, formula_2, data = data, method = "ml")
  
  # Step 2: Generate linear predictions
  xb <- predict(biprobit_model, type = "link")  
  xb1 <- xb[, 1]  # Linear predictor for Mt8
  xb2 <- xb[, 2]  # Linear predictor for Mt5
  
  # Step 3: Compute inverse Mills ratios (Kappa)
  rho <- biprobit_model$estimate["rho"]
  
  kappa8 <- (dnorm(xb1) * pnorm((xb2 - rho * xb1) / sqrt(1 - rho^2))) /
    pmvnorm(lower = c(-Inf, -Inf), upper = c(xb1, xb2), corr = matrix(c(1, rho, rho, 1), 2, 2))
  
  kappa5 <- (dnorm(xb2) * pnorm((xb1 - rho * xb2) / sqrt(1 - rho^2))) /
    pmvnorm(lower = c(-Inf, -Inf), upper = c(xb1, xb2), corr = matrix(c(1, rho, rho, 1), 2, 2))
  
  data$kappa8 <- kappa8
  data$kappa5 <- kappa5
  
  # Step 4: Run final regression with kappa terms
  formula_reg <- as.formula(paste(M, "~", paste(c(X, "kappa8", "kappa5", FE_vars, J_vars), collapse = " + ")))
  
  model <- lm(formula_reg, data = data)
  
  if (!is.null(cluster_var)) {
    cluster_se <- vcovCL(model, cluster = ~data[[cluster_var]])
    summary_model <- coeftest(model, vcov = cluster_se)
  } else {
    summary_model <- summary(model)
  }
  
  return(summary_model)
}

########################### DNV FUNCTION ###########################

DNV <- function(data, X8, Z8, X5, Z5, FE, M, X, FE_vars, J_vars, kpowD = 2, cluster_var = NULL) {
  
  # Step 1: Estimate a Seemingly Unrelated Regression (SUR) model
  formula_1 <- as.formula(paste("Mt8 ~", paste(c(X8, Z8, FE), collapse = " + ")))
  formula_2 <- as.formula(paste("Mt5 ~", paste(c(X5, Z5, FE), collapse = " + ")))
  
  sureg_model <- systemfit(list(eq1 = formula_1, eq2 = formula_2), data = data, method = "SUR")
  
  # Step 2: Predict kappa terms
  data$kappa8 <- predict(sureg_model$eq1)  
  data$kappa5 <- predict(sureg_model$eq2)  
  
  # Step 3: Generate higher-order polynomial terms if kpowD > 2
  if (kpowD > 1) {
    for (j in 2:kpowD) {
      data[[paste0("kappa8_", j)]] <- data$kappa8^j
      data[[paste0("kappa5_", j)]] <- data$kappa5^j
      data[[paste0("kappa5", j, "X8")]] <- (data$kappa5^j) * data$kappa8
      data[[paste0("kappa5X8", j)]] <- data$kappa5 * (data$kappa8^j)
      data[[paste0("kappa5X8_", j)]] <- (data$kappa5^j) * (data$kappa8^j)
    }
  }
  
  # Step 4: Run final regression
  formula_reg <- as.formula(paste(M, "~", paste(c(X, grep("^kappa", names(data), value = TRUE), FE_vars, J_vars), collapse = " + ")))
  
  model <- lm(formula_reg, data = data)
  
  if (!is.null(cluster_var)) {
    cluster_se <- vcovCL(model, cluster = ~data[[cluster_var]])
    summary_model <- coeftest(model, vcov = cluster_se)
  } else {
    summary_model <- summary(model)
  }
  
  return(summary_model)
}

########################### BOOTtse FUNCTION ###########################

BOOTtse <- function(data, formula, cluster_var, reps = 1000, beta_vars) {
  
  model_original <- lm(formula, data = data)
  coef_original <- coef(model_original)
  se_original <- sqrt(diag(vcovCL(model_original, cluster = ~data[[cluster_var]])))  
  
  boot_fn <- function(data, indices) {
    data_boot <- data[indices, ]  
    model_boot <- lm(formula, data = data_boot)
    return(coef(model_boot))
  }
  
  boot_results <- boot(data, boot_fn, R = reps, strata = data[[cluster_var]])
  boot_coefs <- t(boot_results$t)
  
  boot_ci <- apply(boot_coefs, 1, function(x) quantile(x, probs = c(0.05, 0.95)))
  
  wald_stat <- coef_original / se_original
  sig_levels <- ifelse(wald_stat < boot_ci[1, ] | wald_stat > boot_ci[2, ], "*", "")
  
  results <- data.frame(
    Variable = beta_vars,
    Estimate = round(coef_original, 3),
    SE = paste0("(", round(se_original, 3), ")", sig_levels)
  )
  
  return(results)
}

########################### MKTAB FUNCTION ###########################

MKTAB <- function(file_path) {
  results_table <- read_csv(file_path)
  
  if ("row" %in% colnames(results_table)) {
    results_table <- results_table %>% select(-row)
  }
  
  if ("covariate" %in% colnames(results_table)) {
    results_table <- results_table %>% select(covariate, everything())
  }
  
  output_path <- sub("\\.csv$", ".xlsx", file_path)
  write.xlsx(results_table, file = output_path, overwrite = TRUE)
  
  cat("Table exported to:", output_path, "\n")
}

