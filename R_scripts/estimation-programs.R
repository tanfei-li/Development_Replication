

########################### POIRIER FUNCTION ###########################
#################################adapted from table 4###################
POIRIER <- function(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var) {
  library(dplyr)
  library(sampleSelection)  # For biprobit estimation
  library(sandwich)         # For clustered SEs
  library(lmtest)           # For robust inference
  library(pbivnorm)         # For bivariate normal CDF
  
  # Step 0: Clean Data (Remove Missing Values)
  required_vars <- c(X5, Z5, X8, Z8, M, "Mt5", "Mt8", FE_vars, cluster_var)
  data <- data[complete.cases(data[, required_vars]), ]  # Remove rows with NA values
  
  # Count number of observations after removing missing values
  num_rows <- nrow(data)
  cat("--> Number of observations after dropping missing values:", num_rows, "\n")
  
  
  # Step 1: Try Bivariate Probit Estimation
  cat("--> Estimating Bivariate Probit Selection Model...\n")
  bvp_formula <- as.formula(paste(
    "Mt8 + Mt5 ~", 
    paste(c(X8, Z8, paste0("factor(", FE_vars, ")")), collapse = " + "), 
    "|", 
    paste(c(X5, Z5, paste0("factor(", FE_vars, ")")), collapse = " + ")
  ))
  
  bvp_model <- tryCatch(
    selection(bvp_formula, data = data, method = "ml"),
    error = function(e) {
      cat("Warning: Bivariate Probit failed. Estimating separate Probit models instead.\n")
      
      probit_Mt8 <- glm(data$Mt8 ~ ., data = data[, c(X8, Z8, FE_vars)], family = binomial(link = "probit"))
      probit_Mt5 <- glm(data$Mt5 ~ ., data = data[, c(X5, Z5, FE_vars)], family = binomial(link = "probit"))
      
      return(list(probit_Mt8 = probit_Mt8, probit_Mt5 = probit_Mt5))
    }
  )
  
  # Step 2: Extract Linear Predictors
  if ("selection" %in% class(bvp_model)) {
    xb <- predict(bvp_model, type = "xb")
    xb1 <- xb[, 1]  # Fitted values for Mt8
    xb2 <- xb[, 2]  # Fitted values for Mt5
    rho <- coef(bvp_model)["rho"]
  } else {
    xb1 <- predict(bvp_model$probit_Mt8, type = "link")
    xb2 <- predict(bvp_model$probit_Mt5, type = "link")
    rho <- cor(xb1, xb2, use = "complete.obs")  # Approximate correlation if biprobit failed
  }
  
  # Step 3: Compute Selection Correction Terms (Kappa8 and Kappa5)
  phi_xb1 <- dnorm(xb1)  # Normal density
  phi_xb2 <- dnorm(xb2)
  Phi_biv <- pbivnorm(xb1, xb2, rho)
  
  # Ensure stability by preventing division by near-zero values
  Phi_biv[Phi_biv < 1e-6] <- 1e-6
  
  data$kappa8 <- (phi_xb1 * pnorm((xb2 - rho * xb1) / sqrt(1 - rho^2))) / Phi_biv
  data$kappa5 <- (phi_xb2 * pnorm((xb1 - rho * xb2) / sqrt(1 - rho^2))) / Phi_biv
  
  # Step 4: Run Final Regression with Selection Correction
  cat("--> Running Final Regression with Selection Correction...\n")
  reg_formula <- as.formula(paste(M, "~", paste(c(X, "kappa8", "kappa5", FE_vars, J_vars), collapse = " + ")))
  
  final_model <- tryCatch(
    lm(reg_formula, data = data),
    error = function(e) return(NULL)
  )
  
  # Compute Clustered Standard Errors
  clustered_se <- tryCatch(
    coeftest(final_model, vcov = vcovCL(final_model, cluster = ~get(cluster_var))),
    error = function(e) return(NULL)
  )
  
  cat("--> POIRIER Estimation Complete.\n")
  
  return(list(
    biprobit_model = bvp_model,
    final_model = final_model,
    clustered_se = clustered_se
  ))
}



###################with scaled kappa terms######################
DNV <- function(data, Mt8, Mt5, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var) {
  library(systemfit)
  library(dplyr)
  library(lmtest)
  library(sandwich)
  
  # Set high decimal precision
  options(digits = 10)
  
  # Ensure all required variables exist and remove missing values
  data <- data %>% select(all_of(c(X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var))) %>% na.omit()
  
  # Run SUR estimation
sur_model <- systemfit(
  list(
    eq1 = as.formula(paste("Mt8 ~", paste(c(X8, Z8, paste0("factor(", FE_vars, ")")), collapse = " + "))),
    eq2 = as.formula(paste("Mt5 ~", paste(c(X5, Z5, paste0("factor(", FE_vars, ")")), collapse = " + ")))
  ),
  data = data, 
  method = "SUR"
)

  
  # Extract fitted values
  data <- data %>%
    mutate(kappa8 = fitted(sur_model$eq[[1]]),
           kappa5 = fitted(sur_model$eq[[2]]),
           kappa5X8 = kappa5 * kappa8)
  
  # Standardize all kappa terms (before polynomial transformation) because perfect separation issue keeps popping upS
  ##data <- data %>%
    #mutate(kappa8 = scale(kappa8, center = TRUE, scale = TRUE)[, 1],
           #kappa5 = scale(kappa5, center = TRUE, scale = TRUE)[, 1],
           #kappa5X8 = scale(kappa5X8, center = TRUE, scale = TRUE)[, 1])
  
  # Generate polynomial terms for kpowD = 3
  for (j in 2:kpowD) {
    data <- data %>%
      mutate(!!paste0("kappa8_", j) := kappa8^j,
             !!paste0("kappa5_", j) := kappa5^j,
             !!paste0("kappa5", j, "X8") := (kappa5^j) * kappa8,
             !!paste0("kappa5X8", j) := kappa5 * (kappa8^j),
             !!paste0("kappa5X8_", j) := (kappa5^j) * (kappa8^j))
  }
  
  # Run final regression including all kappa polynomial terms
  final_formula <- as.formula(paste(M, "~", paste(c(X, grep("^kappa", names(data), value = TRUE), FE_vars, J_vars), collapse = " + ")))
  
  # Run regression
  reg_model <- lm(final_formula, data = data)
  
  # Compute clustered standard errors
  cluster_se <- coeftest(reg_model, vcov = vcovCL(reg_model, cluster = ~get(cluster_var)))
  
  return(list(model = reg_model, cluster_se = cluster_se, data = data))
}

########################### BOOTtse FUNCTION ###########################
BOOTtse <- function(data, EST, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var, BETA, REPS = 1000, output_file = "BOOT_DATA.csv") {
  library(dplyr)
  library(boot)
  
  # Store original results
  cat("--> Running estimation on original sample...\n")
  original_model <- EST(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)
  original_coefs <- coef(original_model$model)
  original_se <- sqrt(diag(vcov(original_model$model)))
  
  # Define bootstrap function
  boot_fn <- function(data, indices) {
    unique_clusters <- unique(data[[cluster_var]])  # Get all unique clusters
    sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)  # Sample clusters
    boot_data <- data %>% filter(!!as.name(cluster_var) %in% sampled_clusters)  # Keep all rows of sampled clusters
    
    # Run estimation function
    boot_model <- tryCatch({
      EST(boot_data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)
    }, error = function(e) return(NULL))
    
    if (is.null(boot_model$model)) return(rep(NA, length(BETA) * 2))
    
    # Extract coefficients & standard errors
    boot_coefs <- coef(boot_model$model)
    boot_se <- sqrt(diag(vcov(boot_model$model)))
    
    return(c(boot_coefs[BETA], boot_se[BETA]))
  }
  
  # Run bootstrap resampling
  cat("--> Bootstrapping", REPS, "iterations...\n")
  set.seed(10101)
  boot_output <- boot(data, boot_fn, R = REPS, strata = data[[cluster_var]])
  
  # Store bootstrap estimates in a dataframe
  boot_results <- lapply(1:REPS, function(i) {
    data.frame(iter = rep(i, length(BETA)),
               beta = boot_output$t[i, 1:length(BETA)],
               se = boot_output$t[i, (length(BETA) + 1):(2 * length(BETA))],
               variable = BETA)
  }) %>% bind_rows()
  
  # Save bootstrap data
  write.csv(boot_results, output_file, row.names = FALSE)
  
  # Compute percentile-t confidence intervals
  conf_levels <- c(0.05, 0.025, 0.005)
  boot_ci <- boot_results %>%
    group_by(variable) %>%
    summarise(BETA = mean(beta, na.rm = TRUE),
              SE = mean(se, na.rm = TRUE),
              lower_95 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[1], na.rm = TRUE),
              upper_95 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[1], na.rm = TRUE),
              lower_975 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[2], na.rm = TRUE),
              upper_975 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[2], na.rm = TRUE),
              lower_995 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[3], na.rm = TRUE),
              upper_995 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[3], na.rm = TRUE)) %>%
    mutate(sig = case_when(
      original_coefs[variable] / original_se[variable] < lower_95 | original_coefs[variable] / original_se[variable] > upper_95 ~ "*",
      original_coefs[variable] / original_se[variable] < lower_975 | original_coefs[variable] / original_se[variable] > upper_975 ~ "**",
      original_coefs[variable] / original_se[variable] < lower_995 | original_coefs[variable] / original_se[variable] > upper_995 ~ "***",
      TRUE ~ ""
    ))
  
  # Save formatted results
  write.csv(boot_ci, "BOOT_CI.csv", row.names = FALSE)
  
  cat("--> Bootstrapping complete. Results saved to", output_file, "\n")
  
  return(list(bootstrap_results = boot_results, bootstrap_ci = boot_ci, original_model = original_model))
}


###############################a different BOOT regime to debug#########################
PBOOTtse <- function(data, EST, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var, BETA, REPS = 1000, output_file = "BOOT_DATA.csv") {
  library(dplyr)
  library(boot)
  
  # Check if `EST` requires `cluster_var`
  est_args <- names(formals(EST))  # Get function arguments of `EST`
  if ("cluster_var" %in% est_args) {
    cat("--> Running estimation on original sample (including cluster_var)...\n")
    original_model <- EST(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var)
  } else {
    cat("--> Running estimation on original sample (without cluster_var)...\n")
    original_model <- EST(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars)  # Omit `cluster_var`
  }
  
  original_coefs <- coef(original_model$final_model)
  original_se <- sqrt(diag(vcov(original_model$final_model)))
  
  # Define bootstrap function
  boot_fn <- function(data, indices) {
    unique_clusters <- unique(data[[cluster_var]][indices])
    boot_data <- data %>% filter(!!as.name(cluster_var) %in% unique_clusters)
    
    boot_model <- tryCatch({
      if ("cluster_var" %in% est_args) {
        EST(boot_data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var)
      } else {
        EST(boot_data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars)
      }
    }, error = function(e) return(NULL))
    
    if (is.null(boot_model$final_model)) return(rep(NA, length(BETA) * 2))
    
    boot_coefs <- coef(boot_model$final_model)
    boot_se <- sqrt(diag(vcov(boot_model$final_model)))
    
    return(c(boot_coefs[BETA], boot_se[BETA]))
  }
  
  # Run bootstrap resampling
  cat("--> Bootstrapping", REPS, "iterations...\n")
  set.seed(10101)
  boot_output <- boot(data, boot_fn, R = REPS, strata = data[[cluster_var]])
  
  # Save bootstrap data
  boot_results <- data.frame(iter = numeric(), beta = numeric(), se = numeric(), variable = character())
  for (i in 1:REPS) {
    iter_data <- data.frame(iter = rep(i, length(BETA)),
                            beta = boot_output$t[i, 1:length(BETA)],
                            se = boot_output$t[i, (length(BETA) + 1):(2 * length(BETA))],
                            variable = BETA)
    boot_results <- bind_rows(boot_results, iter_data)
  }
  write.csv(boot_results, output_file, row.names = FALSE)
  
  cat("--> Bootstrapping complete. Results saved to", output_file, "\n")
  
  return(list(bootstrap_results = boot_results, original_model = original_model))
}


######################################setup table 6###########################

DNV_6 <- function(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var) {
  library(systemfit)
  library(dplyr)
  library(lmtest)
  library(sandwich)
  
  # Set high decimal precision
  options(digits = 10)
  
  # Convert data to tibble to avoid select() errors
  data <- as_tibble(data) 
  
  # Ensure all required variables exist in the dataset before selecting
  required_vars <- c(X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var)
  required_vars <- required_vars[required_vars %in% names(data)]
  
  # Select only available variables and remove missing values
  data <- data %>% dplyr::select(all_of(required_vars)) %>% na.omit()
  
  # Run SUR estimation
  sur_model <- systemfit(
    list(eq1 = as.formula(paste(paste(c(X8, Z8), collapse = " + "), "~", paste(FE_vars, collapse = " + "))),
         eq2 = as.formula(paste(paste(c(X5, Z5), collapse = " + "), "~", paste(FE_vars, collapse = " + ")))),
    data = data, method = "SUR"
  )
  
  # Extract fitted values
  data <- data %>%
    mutate(kappa8 = fitted(sur_model$eq[[1]]),
           kappa5 = fitted(sur_model$eq[[2]]),
           kappa5X8 = kappa5 * kappa8)
  
  # Standardize all kappa terms (before polynomial transformation)
  data <- data %>%
    mutate(kappa8 = scale(kappa8, center = TRUE, scale = TRUE)[, 1],
           kappa5 = scale(kappa5, center = TRUE, scale = TRUE)[, 1],
           kappa5X8 = scale(kappa5X8, center = TRUE, scale = TRUE)[, 1])
  
  # Generate polynomial terms for kpowD = 3
  for (j in 2:kpowD) {
    data <- data %>%
      mutate(!!paste0("kappa8_", j) := kappa8^j,
             !!paste0("kappa5_", j) := kappa5^j,
             !!paste0("kappa5", j, "X8") := (kappa5^j) * kappa8,
             !!paste0("kappa5X8", j) := kappa5 * (kappa8^j),
             !!paste0("kappa5X8_", j) := (kappa5^j) * (kappa8^j))
  }
  
  # Run final regression including all kappa polynomial terms
  final_formula <- as.formula(paste(M, "~", paste(c(X, grep("^kappa", names(data), value = TRUE), FE_vars, J_vars), collapse = " + ")))
  
  # Run regression
  reg_model <- lm(final_formula, data = data)
  
  # Compute clustered standard errors
  cluster_se <- coeftest(reg_model, vcov = vcovCL(reg_model, cluster = ~get(cluster_var)))
  
  return(list(model = reg_model, cluster_se = cluster_se, data = data))
}




##########################################################################
BOOTtse_6 <- function(data, EST, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var, BETA, REPS = 1000, output_file = "BOOT_DATA.csv") {
  library(dplyr)
  library(boot)
  
  # Convert dataset to tibble (ensures compatibility with dplyr functions)
  data <- as_tibble(data)
  
  # Store original results
  cat("--> Running estimation on original sample...\n")
  original_model <- EST(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)
  original_coefs <- coef(original_model$model)
  original_se <- sqrt(diag(vcov(original_model$model)))
  
  # Define bootstrap function
  boot_fn <- function(data, indices) {
    unique_clusters <- unique(data[[cluster_var]])  # Get all unique clusters
    sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)  # Sample clusters
    boot_data <- data %>% filter(!!as.name(cluster_var) %in% sampled_clusters)  # Keep all rows of sampled clusters
    
    # Run estimation function
    boot_model <- tryCatch({
      EST(boot_data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)
    }, error = function(e) return(NULL))
    
    if (is.null(boot_model$model)) return(rep(NA, length(BETA) * 2))
    
    # Extract coefficients & standard errors
    boot_coefs <- coef(boot_model$model)
    boot_se <- sqrt(diag(vcov(boot_model$model)))
    
    # Ensure BETA variables exist in the model
    beta_values <- sapply(BETA, function(var) if (var %in% names(boot_coefs)) boot_coefs[var] else NA)
    beta_se <- sapply(BETA, function(var) if (var %in% names(boot_se)) boot_se[var] else NA)
    
    return(c(beta_values, beta_se))
  }
  
  # Run bootstrap resampling
  cat("--> Bootstrapping", REPS, "iterations...\n")
  set.seed(10101)
  boot_output <- boot(data, boot_fn, R = REPS, strata = data[[cluster_var]])
  
  # Store bootstrap estimates in a dataframe
  boot_results <- lapply(1:REPS, function(i) {
    data.frame(iter = rep(i, length(BETA)),
               beta = boot_output$t[i, 1:length(BETA)],
               se = boot_output$t[i, (length(BETA) + 1):(2 * length(BETA))],
               variable = BETA)
  }) %>% bind_rows()
  
  # Save bootstrap data
  write.csv(boot_results, output_file, row.names = FALSE)
  
  # Compute percentile-t confidence intervals
  conf_levels <- c(0.05, 0.025, 0.005)
  boot_ci <- boot_results %>%
    group_by(variable) %>%
    summarise(BETA = mean(beta, na.rm = TRUE),
              SE = mean(se, na.rm = TRUE),
              lower_95 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[1], na.rm = TRUE),
              upper_95 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[1], na.rm = TRUE),
              lower_975 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[2], na.rm = TRUE),
              upper_975 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[2], na.rm = TRUE),
              lower_995 = quantile((beta - original_coefs[variable]) / se, probs = conf_levels[3], na.rm = TRUE),
              upper_995 = quantile((beta - original_coefs[variable]) / se, probs = 1 - conf_levels[3], na.rm = TRUE)) %>%
    mutate(sig = case_when(
      original_coefs[variable] / original_se[variable] < lower_95 | original_coefs[variable] / original_se[variable] > upper_95 ~ "*",
      original_coefs[variable] / original_se[variable] < lower_975 | original_coefs[variable] / original_se[variable] > upper_975 ~ "**",
      original_coefs[variable] / original_se[variable] < lower_995 | original_coefs[variable] / original_se[variable] > upper_995 ~ "***",
      TRUE ~ ""
    ))
  
  # Save formatted results
  write.csv(boot_ci, "BOOT_CI.csv", row.names = FALSE)
  
  cat("--> Bootstrapping complete. Results saved to", output_file, "\n")
  
  return(list(bootstrap_results = boot_results, bootstrap_ci = boot_ci, original_model = original_model))
}











########################### MKTAB FUNCTION ###########################

MKTAB <- function(file_path) {
  results_table <- read_csv(file_path, show_col_types = FALSE)
  
  # Ensure correct column ordering
  selected_columns <- c("covariate", setdiff(names(results_table), c("covariate", "row")))
  results_table <- results_table %>% select(all_of(selected_columns))
  
  # Save to Excel
  output_path <- sub("\\.csv$", ".xlsx", file_path)
  write.xlsx(results_table, file = output_path, overwrite = TRUE)
  
  cat("Table exported to:", output_path, "\n")
}
