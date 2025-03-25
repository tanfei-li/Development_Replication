

########################### POIRIER FUNCTION ###########################
#################################adapted from table 4###################
POIRIER <- function(data, Mt8, X8, Z8, Mt5, X5, Z5, X, M, FE_vars, J_vars, BETA = BETA) {
  # Step 0: Clean Data (Remove Missing Values)
  required_vars <- c(Z5, X5, Z8, X8, Mt5, Mt8, FE_vars, J_vars)
  data_cleaned <- data[complete.cases(data[, required_vars]), ]
  
  data_cleaned$XZ5 <- as.matrix(data_cleaned[, c(X5,Z5)])
  data_cleaned$XZ8 <- as.matrix(data_cleaned[, c(X8,Z8)])
  data_cleaned$FE_vars <- as.factor(data_cleaned$prop)
  
  
  cat("--> Estimating Bivariate Probit Selection Model...\n")
  
  biprobit_model <- biprobit(
    Mt8 ~ XZ8 + FE_vars,  # First equation (for Mt8)
    Mt5 ~ XZ5 + FE_vars,  # Second equation (for Mt5)
    data = data_cleaned           # Data frame
  )
  
  # calculating fitted values 
  
  coefficients <- biprobit_model$estimate
  
  # Separate coefficients for each equation
  coeff1 <- coefficients[grepl("^1", names(coefficients))] # First equation
  coeff2 <- coefficients[!grepl("^1", names(coefficients)) & !grepl("tau", names(coefficients))] # Second equation
  tau <- coefficients[grepl("tau", names(coefficients))]
  rho <- 1 - 2 / (exp(tau) + 1)
  
  
  # Step 2: Create design matrices
  X8_matrix <- model.matrix(~ XZ8 + FE_vars, data = data_cleaned)
  X5_matrix <- model.matrix(~ XZ5 + FE_vars, data = data_cleaned)
  
  # Step 3: Compute linear predictors
  xb1 <- as.vector(X8_matrix %*% coeff1)
  xb2 <- as.vector(X5_matrix %*% coeff2)
  
  data_cleaned$kappa8 <- (dnorm(xb1) * pnorm((xb2 - rho * xb1) / sqrt(1 - rho^2))) /
    pbivnorm(xb1, xb2, rho)
  
  data_cleaned$kappa5 <- (dnorm(xb2) * pnorm((xb1 - rho * xb2) / sqrt(1 - rho^2))) /
    pbivnorm(xb1, xb2, rho)
  
  
  #### Step 5: Clean Data for Final Regression ####
  required_vars_2 <- unique(c(Z5, X5, Z8, X8, Mt5, Mt8, FE_vars, J_vars, M, X))
  data_second_stage <- data_cleaned[complete.cases(data_cleaned[, required_vars_2]), ]
  
  #### Step 6: Run Final Regression ####
  final_formula <- as.formula(paste(M, "~", 
                                    paste(c(X, "kappa8", "kappa5"), collapse = " + "), 
                                    "|", FE_vars, "| 0 |", J_vars))  
  
  
  final_model <- felm(final_formula, data = data_second_stage)
  
  cat("--> POIRIER Estimation Complete.\n")
  
  return(list(
    model = final_model,             # for BOOTtse compatibility
    coef_table = summary(final_model)$coefficients,
    data = data_second_stage)
  )
}



###################DNV######################
library(systemfit)
library(lfe)
library(dplyr)
library(lmtest)
library(sandwich)

DNV <- function(data, 
            X5, X8, Z5, Z8,        # Controls for SUR
            Mt5, Mt8,              # Outcomes for SUR
            M,                     # Final regression outcome
            X,                     # Controls for final regression
            FE_vars,
            J_vars,
            kpowD = 3,
            BETA = BETA
) {
  
  #### Step 1: Clean Data for SUR ####
  required_vars <- c(Z5, X5, Z8, X8, Mt5, Mt8, FE_vars, J_vars)
  data_cleaned <- data[complete.cases(data[, required_vars]), ]
  
  # Convert FE to factor
  data_cleaned$FE_vars <- as.factor(data_cleaned[[FE_vars]])
  
  # Define matrices explicitly
  data_cleaned$X5 <- as.matrix(data_cleaned[, X5])
  data_cleaned$X8 <- as.matrix(data_cleaned[, X8])
  data_cleaned$Z5 <- as.matrix(data_cleaned[, Z5])
  data_cleaned$Z8 <- as.matrix(data_cleaned[, Z8])
  
  #### Step 2: Run SUR model ####
  
  eq1 <- Mt8 ~ X8 + Z8 + FE_vars
  eq2 <- Mt5 ~ X5 + Z5 + FE_vars
  system <- list(eq1 = eq1, eq2 = eq2)
  
  
  sureg_model <- systemfit(system, method = "SUR", data = data_cleaned)
  
  #### Step 3: Predict fitted values ####
  data_cleaned$kappa8 <- fitted(sureg_model$eq[[1]])
  data_cleaned$kappa5 <- fitted(sureg_model$eq[[2]])
  data_cleaned$kappa5X8 <- data_cleaned$kappa5 * data_cleaned$kappa8
  
  #### Step 4: Higher-order kappa terms ####
  kappa_vars <- c("kappa8", "kappa5", "kappa5X8")
  
  for (j in 2:kpowD) {
    data_cleaned[[paste0("kappa8_", j)]]    <- data_cleaned$kappa8^j
    data_cleaned[[paste0("kappa5_", j)]]    <- data_cleaned$kappa5^j
    data_cleaned[[paste0("kappa5", j, "X8")]] <- (data_cleaned$kappa5^j) * data_cleaned$kappa8
    data_cleaned[[paste0("kappa5X8", j)]]     <- data_cleaned$kappa5 * (data_cleaned$kappa8^j)
    data_cleaned[[paste0("kappa5X8_", j)]]    <- (data_cleaned$kappa5^j) * (data_cleaned$kappa8^j)
    
    kappa_vars <- c(kappa_vars, 
                    paste0("kappa8_", j), 
                    paste0("kappa5_", j), 
                    paste0("kappa5", j, "X8"), 
                    paste0("kappa5X8", j), 
                    paste0("kappa5X8_", j))
  }
  
  #### Step 5: Clean Data for Final Regression ####
  required_vars_2 <- unique(c(Z5, X5, Z8, X8, Mt5, Mt8, FE_vars, J_vars, M, X))
  data_second_stage <- data_cleaned[complete.cases(data_cleaned[, required_vars_2]), ]
  
  #### Step 6: Run Final Regression ####
  final_formula <- as.formula(paste(M, "~", 
                                    paste(c(X, kappa_vars), collapse = " + "), 
                                    "|", FE_vars, "| 0 |", J_vars))  
  
  
  final_model <- felm(final_formula, data = data_second_stage)
  
  
  
  #### Step 7: Return same structure ####
  return(list(
    model = final_model,             # for BOOTtse compatibility
    coef_table = summary(final_model)$coefficients,
    data = data_second_stage)
    )
}


########################### BOOTtse FUNCTION ###########################
BOOTtse <- function(data, EST, 
                    X8, Z8, X5, Z5, X, kpowD, M, 
                    FE_vars, J_vars, 
                    Mt5 = "Mt5", Mt8 = "Mt8", BETA = BETA,
                    REPS = 1000, output_file = "BOOT_DATA.csv",
                    progress_step = 50) {  
  
  library(dplyr)
  
  # Store original results
  cat("--> Running estimation on original sample...\n")
  original_model <- EST(data, 
                        X5 = X5, X8 = X8, Z5 = Z5, Z8 = Z8, 
                        Mt5 = Mt5, Mt8 = Mt8, 
                        M = M, X = X, 
                        FE_vars = FE_vars, J_vars = J_vars, 
                        kpowD = kpowD, BETA = BETA)
  
  # Extract original coefficients & clustered SEs directly from felm summary
  original_summary <- summary(original_model$model)$coefficients
  original_coefs <- original_summary[BETA, "Estimate"]
  original_se <- original_summary[BETA, "Cluster s.e."]
  
  #### Bootstrap Function ####
  cat("--> Bootstrapping", REPS, "iterations...\n")
  
  set.seed(10101)
  boot_t <- matrix(NA, nrow = REPS, ncol = length(BETA) * 2)
  
  unique_clusters <- unique(data[[J_vars]])
  
  start_time <- Sys.time()
  
  for (i in 1:REPS) {
    
    # Cluster bootstrap
    sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    boot_data <- data %>% filter(!!as.name(J_vars) %in% sampled_clusters)
    
    # Run estimation
    boot_model <- tryCatch({
      EST(boot_data, 
          X5 = X5, X8 = X8, Z5 = Z5, Z8 = Z8, 
          Mt5 = Mt5, Mt8 = Mt8, 
          M = M, X = X, 
          FE_vars = FE_vars, J_vars = J_vars, 
          kpowD = kpowD, 
          BETA = BETA)
    }, error = function(e) return(NULL))
    
    if (!is.null(boot_model$model)) {
      boot_summary <- summary(boot_model$model)$coefficients
      boot_t[i, 1:length(BETA)] <- boot_summary[BETA, "Estimate"]
      boot_t[i, (length(BETA)+1):(2*length(BETA))] <- boot_summary[BETA, "Cluster s.e."]
    }
    
    # Progress Message
    if (i %% progress_step == 0) {
      cat("--> Completed", i, "of", REPS, "iterations at", format(Sys.time() - start_time), "elapsed\n")
      flush.console()
    }
  }
  
  #### Store Bootstrap Results ####
  boot_results <- lapply(1:REPS, function(i) {
    data.frame(iter = rep(i, length(BETA)),
               beta = boot_t[i, 1:length(BETA)],
               se = boot_t[i, (length(BETA) + 1):(2 * length(BETA))],
               variable = BETA)
  }) %>% bind_rows()
  
  # Save raw bootstrap estimates
  write.csv(boot_results, output_file, row.names = FALSE)
  
  #### Compute Percentile-t Confidence Intervals ####
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
  
  #### Save Final Results ####
  write.csv(boot_ci, "BOOT_CI.csv", row.names = FALSE)
  
  cat("--> Bootstrapping complete. Results saved to", output_file, "\n")
  
  return(list(bootstrap_results = boot_results, bootstrap_ci = boot_ci, original_model = original_model))
}


#####################################PBOOTtse without kpowD#####################
PBOOTtse <- function(data, EST, 
                    X8, Z8, X5, Z5, X, M, 
                    FE_vars, J_vars, 
                    Mt5 = "Mt5", Mt8 = "Mt8", BETA = BETA,
                    REPS = 1000, output_file = "BOOT_DATA.csv",
                    progress_step = 50) {  
  
  library(dplyr)
  
  # Store original results
  cat("--> Running estimation on original sample...\n")
  original_model <- EST(data, 
                        X5 = X5, X8 = X8, Z5 = Z5, Z8 = Z8, 
                        Mt5 = Mt5, Mt8 = Mt8, 
                        M = M, X = X, 
                        FE_vars = FE_vars, J_vars = J_vars, 
                        BETA = BETA)
  
  # Extract original coefficients & clustered SEs directly from felm summary
  original_summary <- summary(original_model$model)$coefficients
  original_coefs <- original_summary[BETA, "Estimate"]
  original_se <- original_summary[BETA, "Cluster s.e."]
  
  #### Bootstrap Function ####
  cat("--> Bootstrapping", REPS, "iterations...\n")
  
  set.seed(10101)
  boot_t <- matrix(NA, nrow = REPS, ncol = length(BETA) * 2)
  
  unique_clusters <- unique(data[[J_vars]])
  
  start_time <- Sys.time()
  
  for (i in 1:REPS) {
    
    # Cluster bootstrap
    sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    boot_data <- data %>% filter(!!as.name(J_vars) %in% sampled_clusters)
    
    # Run estimation
    boot_model <- tryCatch({
      EST(boot_data, 
          X5 = X5, X8 = X8, Z5 = Z5, Z8 = Z8, 
          Mt5 = Mt5, Mt8 = Mt8, 
          M = M, X = X, 
          FE_vars = FE_vars, J_vars = J_vars, 
          BETA = BETA)
    }, error = function(e) return(NULL))
    
    if (!is.null(boot_model$model)) {
      boot_summary <- summary(boot_model$model)$coefficients
      boot_t[i, 1:length(BETA)] <- boot_summary[BETA, "Estimate"]
      boot_t[i, (length(BETA)+1):(2*length(BETA))] <- boot_summary[BETA, "Cluster s.e."]
    }
    
    # Progress Message
    if (i %% progress_step == 0) {
      cat("--> Completed", i, "of", REPS, "iterations at", format(Sys.time() - start_time), "elapsed\n")
      flush.console()
    }
  }
  
  #### Store Bootstrap Results ####
  boot_results <- lapply(1:REPS, function(i) {
    data.frame(iter = rep(i, length(BETA)),
               beta = boot_t[i, 1:length(BETA)],
               se = boot_t[i, (length(BETA) + 1):(2 * length(BETA))],
               variable = BETA)
  }) %>% bind_rows()
  
  # Save raw bootstrap estimates
  write.csv(boot_results, output_file, row.names = FALSE)
  
  #### Compute Percentile-t Confidence Intervals ####
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
  
  #### Save Final Results ####
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
