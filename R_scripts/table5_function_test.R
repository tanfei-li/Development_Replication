
###################panel A########################################
DNV <- function(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var) {
  library(systemfit)
  library(dplyr)
  library(lmtest)
  library(sandwich)
  
  # Ensure all required variables exist in data and remove missing values
  data <- data %>% select(all_of(c(X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var))) %>% na.omit()
  
  # Define SUR equations where X8 + Z8 and X5 + Z5 are dependent variables
  formula_eq1 <- as.formula(paste(paste(c(X8, Z8), collapse = " + "), "~", paste(FE_vars, collapse = " + ")))
  formula_eq2 <- as.formula(paste(paste(c(X5, Z5), collapse = " + "), "~", paste(FE_vars, collapse = " + ")))
  
  # Run SUR estimation
  sur_model <- systemfit(list(eq1 = formula_eq1, eq2 = formula_eq2), data = data, method = "SUR")
  
  # Extract residual sample (equivalent to `if e(sample)`)
  valid_sample <- !is.na(fitted(sur_model$eq[[1]])) & !is.na(fitted(sur_model$eq[[2]]))
  data <- data[valid_sample, ]
  
  # Extract fitted values correctly
  data <- data %>% 
    mutate(kappa8 = fitted(sur_model$eq[[1]]),
           kappa5 = fitted(sur_model$eq[[2]]),
           kappa5X8 = kappa5 * kappa8)
  
  # Normalize kappa terms to avoid large polynomial values
  data <- data %>%
    mutate(kappa8_norm = (kappa8 - mean(kappa8)) / sd(kappa8),
           kappa5_norm = (kappa5 - mean(kappa5)) / sd(kappa5))
  
  # Store kappa variable names
  kappa_vars <- c("kappa8", "kappa5", "kappa5X8")
  
  # Generate multiple polynomial terms (matching Stata `forvalues j=2/$kpowD`)
  for (j in 2:kpowD) {
    data <- data %>% 
      mutate(!!paste0("kappa8_", j) := kappa8_norm^j,
             !!paste0("kappa5_", j) := kappa5_norm^j,
             !!paste0("kappa5", j, "X8") := (kappa5_norm^j) * kappa8_norm,
             !!paste0("kappa5X8", j) := kappa5_norm * (kappa8_norm^j),
             !!paste0("kappa5X8_", j) := (kappa5_norm^j) * (kappa8_norm^j))
    
    # Append new kappa variables
    kappa_vars <- c(kappa_vars, 
                    paste0("kappa8_", j), 
                    paste0("kappa5_", j), 
                    paste0("kappa5", j, "X8"), 
                    paste0("kappa5X8", j), 
                    paste0("kappa5X8_", j))
  }
  
  # Run final regression including all kappa polynomial terms
  final_formula <- as.formula(paste(M, "~", paste(c(X, kappa_vars, FE_vars, J_vars), collapse = " + ")))
  
  # Run regression
  reg_model <- lm(final_formula, data = data)
  
  # Compute clustered standard errors
  cluster_se <- coeftest(reg_model, vcov = vcovCL(reg_model, cluster = ~get(cluster_var)))
  
  return(list(model = reg_model, cluster_se = cluster_se, data = data))
}

# Run DNV function
dnv_result <- DNV(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)

# Check if multiple polynomial kappa terms are generated
head(dnv_result$data[c("kappa8", "kappa5", "kappa5X8", "kappa8_2", "kappa5_2", "kappa5X8_2")])



###################with scaled kappa terms######################
DNV <- function(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var) {
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
    list(eq1 = as.formula(paste(paste(c(X8, Z8), collapse = " + "), "~", paste(FE_vars, collapse = " + "))),
         eq2 = as.formula(paste(paste(c(X5, Z5), collapse = " + "), "~", paste(FE_vars, collapse = " + ")))),
    data = data, method = "SUR"
  )
  
  # Extract fitted values
  data <- data %>%
    mutate(kappa8_raw = fitted(sur_model$eq[[1]]),
           kappa5_raw = fitted(sur_model$eq[[2]]),
           kappa5X8_raw = kappa5_raw * kappa8_raw)
  
  # Standardize all kappa terms (before polynomial transformation)
  data <- data %>%
    mutate(kappa8 = scale(kappa8_raw, center = TRUE, scale = TRUE)[, 1],
           kappa5 = scale(kappa5_raw, center = TRUE, scale = TRUE)[, 1],
           kappa5X8 = scale(kappa5X8_raw, center = TRUE, scale = TRUE)[, 1])
  
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

# Run DNV function
dnv_result <- DNV(data, X8, Z8, X5, Z5, X, 3, M, FE_vars, J_vars, cluster_var)

# Check polynomial kappa terms
head(dnv_result$data[c("kappa8", "kappa5", "kappa5X8", "kappa8_2", "kappa5_2", "kappa5X8_2", "kappa8_3", "kappa5_3", "kappa5X8_3")])
summary(dnv_result$data[c("kappa8", "kappa5", "kappa5X8", "kappa8_2", "kappa5_2", "kappa5X8_2", "kappa8_3", "kappa5_3", "kappa5X8_3")])






BOOTtse <- function(data, EST = DNV, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var, BETA, REPS = 1000, output_file = "BOOT_DATA.csv") {
  library(dplyr)
  library(boot)
  
  # Store original results
  cat("--> Running estimation on original sample...\n")
  original_model <- EST(data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var)
  original_coefs <- coef(original_model$model)
  original_se <- sqrt(diag(vcov(original_model$model)))
  
  # Create empty storage for bootstrap results
  boot_results <- data.frame(iter = numeric(), beta = numeric(), se = numeric(), variable = character())
  
  # Define bootstrap function
  boot_fn <- function(data, indices) {
    # Resample at the **cluster** level
    unique_clusters <- unique(data[[cluster_var]][indices])
    boot_data <- data %>% filter(!!as.name(cluster_var) %in% unique_clusters)
    
    # Run estimation function (e.g., `DNV()`)
    boot_model <- tryCatch(EST(boot_data, X8, Z8, X5, Z5, X, kpowD, M, FE_vars, J_vars, cluster_var),
                           error = function(e) return(NULL))
    
    if (is.null(boot_model)) return(rep(NA, length(BETA) * 2))  # Handle estimation failure
    
    # Extract coefficients & standard errors
    boot_coefs <- coef(boot_model$model)
    boot_se <- sqrt(diag(vcov(boot_model$model)))
    
    return(c(boot_coefs[BETA], boot_se[BETA]))  # Only store relevant coefficients
  }
  
  # Run bootstrap resampling
  cat("--> Bootstrapping", REPS, "iterations...\n")
  set.seed(10101)  # Ensure reproducibility
  boot_output <- boot(data, boot_fn, R = REPS, strata = data[[cluster_var]])
  
  # Store bootstrap estimates in a dataframe
  for (i in 1:REPS) {
    iter_data <- data.frame(iter = rep(i, length(BETA)),
                            beta = boot_output$t[i, 1:length(BETA)],
                            se = boot_output$t[i, (length(BETA) + 1):(2 * length(BETA))],
                            variable = BETA)
    boot_results <- bind_rows(boot_results, iter_data)
  }
  
  # Save bootstrap data
  write.csv(boot_results, output_file, row.names = FALSE)
  
  # Compute percentile-t confidence intervals
  conf_levels <- c(0.05, 0.025, 0.005)  # 95%, 97.5%, 99.5% levels
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





library(lfe)  # For felm()
library(dplyr)
library(boot)

# ---------------------------
# Define Variable Groups
# ---------------------------
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

TABOUT <- file.path(output, "table5intensive.csv")
BFILE <- "table5intensive"

# ---------------------------
# Column 1: Baseline Regression
# ---------------------------
# Dependent variable
M <- "d_ln_emig_shr"  # Ensure this is a string

# Define independent variables for Column 1
X <- c(R, C, C2, S, pDr, rainD)

# Construct the formula correctly
formula <- as.formula(paste(M, "~", paste(X, collapse = " + "), "|", FE_vars, "| 0 |", cluster_var))

# Run Fixed Effects Model with Clustering
cat("\n--> Running Column 1: Baseline Regression\n")
model1 <- felm(formula, data = data)

# Store results
results <- list()
results[[1]] <- model1  # Store Column 1 results
print(results[[1]])

# ---------------------------
# Column 2: Bootstrapped DNV Model
# ---------------------------
X5_col2 <- c(R, p5r, rainS5, C, C2, S)  
X8_col2 <- c(R, p8r, rainS8, C, C2, S)  
X_col2  <- c(R, C, C2, S, pDr, rainD)  
BETA_col2 <- c(pDr, rainD)

cat("\n--> Running BOOTtse() for Column 2\n")
boot_col2 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col2, Z8 = Z8, X5 = X5_col2, Z5 = Z5, 
                     X = X_col2, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_col2,
                     REPS = 100, output_file = "BOOT_DATA_col2.csv")

results[[2]] <- boot_col2

# ---------------------------
# Column 3: Bootstrapped DNV Model with Rain Shock Lags
# ---------------------------
X5_col3 <- c(R, p5r, rainS5L, C, C2, S)  
X8_col3 <- c(R, p8r, rainS8L, C, C2, S)  
X_col3  <- c(R, C, C2, S, pDr, rainDL)  
BETA_col3 <- c(pDr, rainDL)

cat("\n--> Running BOOTtse() for Column 3\n")
boot_col3 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col3, Z8 = Z8, X5 = X5_col3, Z5 = Z5, 
                     X = X_col3, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_col3,
                     REPS = 500, output_file = "BOOT_DATA_col3.csv")

results[[3]] <- boot_col3

# ---------------------------
# Merge Results into Final Table
# ---------------------------
cat("\n--> Merging Results for Table Output\n")

# Load bootstrapped results
boot_col2_data <- read.csv("BOOT_DATA_col2.csv")
boot_col3_data <- read.csv("BOOT_DATA_col3.csv")

# Format results for output table
boot_col2_data <- boot_col2_data %>% select(variable, beta) %>% rename(column2 = beta)
boot_col3_data <- boot_col3_data %>% select(variable, beta) %>% rename(column3 = beta)

# Merge results into final table
final_table <- boot_col2_data %>%
  full_join(boot_col3_data, by = "variable")

# Add Column 1 results manually
col1_results <- data.frame(variable = names(coef(model1)), column1 = coef(model1))
final_table <- full_join(final_table, col1_results, by = "variable")

# Save final formatted table
write.csv(final_table, TABOUT, row.names = FALSE)

cat("\n--> Final table saved as:", TABOUT, "\n")

######################better selectin of results###################
library(lfe)  # For felm()
library(dplyr)
library(boot)

# ---------------------------
# Define Variable Groups
# ---------------------------
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

TABOUT <- file.path(output, "table5intensive.csv")
BFILE <- "table5intensive"

library(lfe)  # For felm()
library(dplyr)
library(boot)

# ---------------------------
# Define Variable Groups
# ---------------------------
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

TABOUT <- file.path(output, "table5intensive.csv")
BFILE <- "table5intensive"

# ---------------------------
# Column 1: Baseline Regression
# ---------------------------
# Dependent variable
M <- "d_ln_emig_shr"

# Define independent variables for Column 1
X <- c(R, C, C2, S, pDr, rainD)

# Construct the formula correctly
formula <- as.formula(paste(M, "~", paste(X, collapse = " + "), "|", FE_vars, "| 0 |", cluster_var))

# Run Fixed Effects Model with Clustering
cat("\n--> Running Column 1: Baseline Regression\n")
model1 <- felm(formula, data = data)

# Extract coefficients and SE for rainD and pDr only
coef1 <- coef(model1)[c(rainD, pDr)]
se1 <- sqrt(diag(vcov(model1)))[c(rainD, pDr)]

# Store results in a named data frame
results <- list()
results[[1]] <- data.frame(variable = names(coef1), coefficient = coef1, standard_error = se1)

print(results[[1]])

# ---------------------------
# Column 2: Bootstrapped DNV Model
# ---------------------------
X5_col2 <- c(R, p5r, rainS5, C, C2, S)  
X8_col2 <- c(R, p8r, rainS8, C, C2, S)  
X_col2  <- c(R, C, C2, S, pDr, rainD)  
BETA_col2 <- c(pDr, rainD)

cat("\n--> Running BOOTtse() for Column 2\n")
boot_col2 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col2, Z8 = Z8, X5 = X5_col2, Z5 = Z5, 
                     X = X_col2, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_col2,
                     REPS = 100, output_file = "BOOT_DATA_col2.csv")

# Store only BETA coefficients and SE in a named data frame
results[[2]] <- data.frame(variable = BETA_col2, 
                           coefficient = boot_col2$bootstrap_ci$BETA, 
                           standard_error = boot_col2$bootstrap_ci$SE)

# ---------------------------
# Column 3: Bootstrapped DNV Model with Rain Shock Lags
# ---------------------------
X5_col3 <- c(R, p5r, rainS5L, C, C2, S)  
X8_col3 <- c(R, p8r, rainS8L, C, C2, S)  
X_col3  <- c(R, C, C2, S, pDr, rainDL)  
BETA_col3 <- c(pDr, rainDL)

cat("\n--> Running BOOTtse() for Column 3\n")
boot_col3 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col3, Z8 = Z8, X5 = X5_col3, Z5 = Z5, 
                     X = X_col3, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_col3,
                     REPS = 500, output_file = "BOOT_DATA_col3.csv")

# Store only BETA coefficients and SE in a named data frame
results[[3]] <- data.frame(variable = BETA_col3, 
                           coefficient = boot_col3$bootstrap_ci$BETA, 
                           standard_error = boot_col3$bootstrap_ci$SE)

# ---------------------------
# Merge Results into Final Table
# ---------------------------
cat("\n--> Merging Results for Table Output\n")

# Load bootstrapped results
boot_col2_data <- read.csv("BOOT_DATA_col2.csv")
boot_col3_data <- read.csv("BOOT_DATA_col3.csv")

# Format results for output table
boot_col2_data <- boot_col2_data %>% select(variable, beta) %>% rename(column2 = beta)
boot_col3_data <- boot_col3_data %>% select(variable, beta) %>% rename(column3 = beta)

# Merge results into final table
final_table <- boot_col2_data %>%
  full_join(boot_col3_data, by = "variable")

# Add Column 1 results manually
col1_results <- results[[1]] %>% select(variable, coefficient) %>% rename(column1 = coefficient)
final_table <- full_join(final_table, col1_results, by = "variable")

# Save final formatted table
write.csv(final_table, TABOUT, row.names = FALSE)

cat("\n--> Final table saved as:", TABOUT, "\n")

################panel B###########################################
POIRIER <- function(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var) {
  
  # Step 1: Handle missing values
  data <- na.omit(data)
  
  # Step 2: Estimate Bivariate Probit Selection Model
  cat("--> Running bivariate probit selection model...\n")
  
  bvp_formula <- as.formula(paste(
    "Mt8 + Mt5 ~", 
    paste(c(X8, Z8, paste0("factor(", FE_vars, ")")), collapse = " + "), 
    "|", 
    paste(c(X5, Z5, paste0("factor(", FE_vars, ")")), collapse = " + ")
  ))
  
  bvp_model <- tryCatch(
    selection(bvp_formula, data = data, method = "ml", control = list(maxit = 500, trace = TRUE)),
    error = function(e) return(NULL)
  )

  
  # Extract predicted values (xb1, xb2) and rho
  xb1 <- predict(bvp_model, type = "xb")[, 1]  # Predicted linear index for Mt8
  xb2 <- predict(bvp_model, type = "xb")[, 2]  # Predicted linear index for Mt5
  rho <- coef(bvp_model)["rho"]
  
  # Compute selection correction terms (kappa8, kappa5)
  cat("--> Computing selection correction terms...\n")
  phi_xb1 <- dnorm(xb1)  # Normal density
  phi_xb2 <- dnorm(xb2)
  Phi_biv <- pbivnorm(xb1, xb2, rho)  # Bivariate normal CDF
  
  kappa8 <- (phi_xb1 * pnorm((xb2 - rho * xb1) / sqrt(1 - rho^2))) / Phi_biv
  kappa5 <- (phi_xb2 * pnorm((xb1 - rho * xb2) / sqrt(1 - rho^2))) / Phi_biv
  
  # Step 3: Run Regression with Selection Correction
  data <- data %>%
    mutate(kappa8 = kappa8, kappa5 = kappa5)
  
  reg_formula <- as.formula(paste(
    M, "~", paste(c(X, "kappa8", "kappa5", paste0("factor(", FE_vars, ")"), J_vars), collapse = " + ")
  ))
  
  cat("--> Running final regression with selection correction...\n")
  final_model <- tryCatch(
    lm(reg_formula, data = data),
    error = function(e) return(NULL)
  )
  
  
  return(list(
    bvp_model = bvp_model,
    final_model = final_model,
    clustered_se = clustered_se
  ))
}

poirier_results <- POIRIER(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var)

# Print results
cat("\n=== Bivariate Probit Summary ===\n")
if (!is.null(poirier_results$bvp_model)) {
  print(summary(poirier_results$bvp_model))
} else {
  cat("Bivariate Probit estimation failed.\n")
}

cat("\n=== Final Regression Summary ===\n")
if (!is.null(poirier_results$final_model)) {
  print(summary(poirier_results$final_model))
} else {
  cat("Final regression estimation failed.\n")
}

cat("\n=== Clustered Standard Errors ===\n")
if (!is.null(poirier_results$clustered_se)) {
  print(poirier_results$clustered_se)
} else {
  cat("Clustered SE calculation failed.\n")
}



#################################adapted from table 4###########################################
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




POIRIER <- function(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var) {
  library(dplyr)
  library(sampleSelection)  # For biprobit estimation
  library(sandwich)         # For clustered & robust SEs
  library(lmtest)           # For robust inference
  library(pbivnorm)         # For bivariate normal CDF
  
  # Step 0: Clean Data (Remove Missing Values)
  required_vars <- c(X5, Z5, X8, Z8, M, "Mt5", "Mt8", FE_vars, cluster_var)
  data <- data[complete.cases(data[, required_vars]), ]  # Remove rows with NA values
  
  # If Mt5 has no variation, just print a warning but continue
  if (length(unique(data$Mt5)) == 1) {
    cat("Warning: Mt5 has no variation (all values are the same). Skipping selection model.\n")
  }
  
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
  
  # Step 2: Extract Linear Predictors & Correlation Term
  if ("selection" %in% class(bvp_model)) {
    xb <- predict(bvp_model, type = "xb")
    xb1 <- xb[, 1]  # Fitted values for Mt8
    xb2 <- xb[, 2]  # Fitted values for Mt5
    rho <- coef(bvp_model)["rho"]  # Correlation between Mt5 and Mt8
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
  
  # Step 5: Compute Correct Standard Errors
  if (!is.null(final_model)) {
    vcov_matrix <- vcovCL(final_model, cluster = ~get(cluster_var))  # Clustered SEs
    robust_se <- sqrt(diag(vcov_matrix))  # Extract standard errors
  } else {
    robust_se <- NULL
  }
  
  cat("--> POIRIER Estimation Complete.\n")
  
  return(list(
    biprobit_model = bvp_model,
    final_model = final_model,
    clustered_se = robust_se
  ))
}


poirier_result <-POIRIER(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var)