# ---------------------------
# POIRIER Model (Two-Step Bivariate Probit)
# ---------------------------

POIRIER <- function(data) {
  library(sampleSelection)  # For Bivariate Probit (heckit-style correction)
  library(dplyr)
  
  # Ensure required variables exist
  all_vars <- c("Mt8", "Mt5", X8, Z8, X5, Z5, FE_vars, "district")
  data <- data %>% drop_na(any_of(all_vars))
  
  # Define Bivariate Probit Model
  formula_Mt8 <- as.formula(paste("Mt8 ~", paste(c(X8, Z8, FE_vars), collapse = " + ")))
  formula_Mt5 <- as.formula(paste("Mt5 ~", paste(c(X5, Z5, FE_vars), collapse = " + ")))
  
  # Estimate the model
  model <- try(selection(formula_Mt8, formula_Mt5, data = data, method = "ml"), silent = TRUE)
  if (inherits(model, "try-error")) {
    stop("Bivariate probit model failed to converge.")
  }
  
  # Extract linear predictions (xb1 and xb2)
  xb1 <- predict(model, type = "link", equation = 1)  # Linear prediction for Mt8
  xb2 <- predict(model, type = "link", equation = 2)  # Linear prediction for Mt5
  
  # Compute selection terms (Inverse Mills Ratios)
  rho <- coef(model)["rho"]  # Estimated correlation
  norm_density <- function(x) dnorm(x)
  norm_cdf <- function(x) pnorm(x)
  bi_cdf <- function(x, y, r) pbivnorm::pbivnorm(x, y, rho = r)  # Bivariate CDF
  
  data$kappa8 <- (norm_density(xb1) * norm_cdf((xb2 - rho * xb1) / sqrt(1 - rho^2))) / 
    bi_cdf(xb1, xb2, rho)
  data$kappa5 <- (norm_density(xb2) * norm_cdf((xb1 - rho * xb2) / sqrt(1 - rho^2))) / 
    bi_cdf(xb1, xb2, rho)
  
  # Generate interaction term
  data$kappa5X8 <- data$kappa5 * data$kappa8
  
  # Run final regression with selection correction
  final_model <- felm(
    as.formula(paste(M, "~", paste(c(X, "kappa8", "kappa5", "kappa5X8", FE_vars), collapse = " + "),
                     "|", FE_vars, "| 0 | district")),
    data = data
  )
  
  return(final_model)
}


# ---------------------------
# DNV
# ---------------------------

DNV <- function(data) {
  library(systemfit)
  library(dplyr)
  
  # Ensure all required variables are present
  all_vars <- c("Mt8", "Mt5", X8, Z8, X5, Z5, FE_vars, "district", "prop")
  data <- data %>% drop_na(any_of(all_vars))
  data$prop <- as.factor(data$prop)  # Convert to factor for fixed effects
  
  # Define SUR model
  formula_Mt8 <- as.formula(paste("Mt8 ~", paste(c(X8, Z8, FE_vars), collapse = " + ")))
  formula_Mt5 <- as.formula(paste("Mt5 ~", paste(c(X5, Z5, FE_vars), collapse = " + ")))
  
  # Estimate SUR Model
  model <- try(systemfit(list(eq1 = formula_Mt8, eq2 = formula_Mt5), data = data, method = "SUR"), silent = TRUE)
  if (inherits(model, "try-error")) {
    warning("DNV failed: returning NA")
    return(rep(NA, length(X) + 3))
  }
  
  # Extract predicted selection terms
  kappa8 <- predict(model)[, 1]
  kappa5 <- predict(model)[, 2]
  
  # Handle NaNs
  kappa8[is.nan(kappa8)] <- 0
  kappa5[is.nan(kappa5)] <- 0
  
  # Generate interaction terms
  data$kappa8 <- kappa8
  data$kappa5 <- kappa5
  data$kappa5X8 <- kappa5 * kappa8
  
  # Higher-order selection terms
  for (j in 2:kpowD) {
    data[[paste0("kappa8_", j)]] <- kappa8^j
    data[[paste0("kappa5_", j)]] <- kappa5^j
    data[[paste0("kappa5", j, "X8")]] <- (kappa5^j) * kappa8
    data[[paste0("kappa5X8", j)]] <- kappa5 * (kappa8^j)
    data[[paste0("kappa5X8_", j)]] <- (kappa5^j) * (kappa8^j)
  }
  
  # Run final regression with fixed effects for `prop`
  final_model <- try(felm(
    as.formula(paste(M, "~", paste(c(X, names(data)[grepl("kappa", names(data))]), collapse = " + "), "|", paste(FE_vars, "prop", sep = " + "), "| 0 | district")),
    data = data
  ), silent = TRUE)
  
  if (inherits(final_model, "try-error")) {
    warning("Final regression in DNV failed: returning NA")
    return(rep(NA, length(X) + 3))
  }
  
  return(final_model)
}

# ---------------------------
# BOOTtse
# ---------------------------


BOOTtse <- function(data, estimator, reps = 1000, result_index) {
  library(boot)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  
  # Ensure required variables exist
  all_vars <- c(M, X, X5, X8, "Mt5", "Mt8", FE_vars, "district", "prop")
  data <- data %>% drop_na(any_of(all_vars))
  data$prop <- as.factor(data$prop)  # Convert to factor for fixed effects
  
  # Create storage for bootstrap data
  boot_file <- file.path(output, paste0("BOOT_DATA_", BFILE, ".xlsx"))
  
  # Extract variable names from the model
  model_test <- try(estimator(data), silent = TRUE)
  if (inherits(model_test, "try-error")) {
    stop("Estimator failed on full dataset.")
  }
  variable_names <- names(coef(model_test))  # Get variable names from the model
  
  # Bootstrap function: Resample clusters (districts) with replacement
  boot_fn <- function(data, indices) {
    # Select a sample of districts with replacement
    sampled_districts <- sample(unique(data$district), length(unique(data$district)), replace = TRUE)
    
    # Keep only rows corresponding to sampled districts
    boot_sample <- data %>% filter(district %in% sampled_districts)
    
    # Run the estimator on the resampled data
    result <- tryCatch({
      model <- estimator(boot_sample)
      coefs <- as.numeric(coef(model))  # Extract coefficients safely
      return(coefs)
    }, error = function(e) {
      return(rep(NA, length(variable_names)))  # Return NA if model fails
    })
    
    return(result)
  }
  
  # Run bootstrapping
  set.seed(2024)  # Ensure reproducibility
  boot_results <- boot(data, statistic = boot_fn, R = reps)  # No need for `strata` since we manually cluster
  
  # Extract Bootstrap Coefficients
  boot_coefs <- boot_results$t
  colnames(boot_coefs) <- variable_names  # Assign correct variable names
  
  # Convert to data frame
  boot_summary <- data.frame(
    Variable = variable_names,
    BETA = colMeans(boot_coefs, na.rm = TRUE),
    SE = apply(boot_coefs, 2, sd, na.rm = TRUE)
  )
  
  # Store in results list with correct index
  results[[result_index]] <- setNames(boot_summary$BETA, boot_summary$Variable)
  
  # Save raw bootstrap draws
  write.xlsx(boot_coefs, boot_file, rowNames = FALSE)
  
  return(results[[result_index]])  # Return named numeric vector
}




# ---------------------------
# MKTAB: Generate Summary Table
# ---------------------------
MKTAB <- function(input_file, output_file = "formatted_results.xlsx") {
  library(readxl)
  library(openxlsx)
  
  # Load the bootstrap table
  data <- read_excel(input_file)
  
  # Sort columns
  data <- data %>%
    arrange(Variable, Row) %>%
    select(Variable, Statistic, Value)
  
  # Save formatted table
  write.xlsx(data, output_file, rowNames = FALSE)
  
  return(data)
}


