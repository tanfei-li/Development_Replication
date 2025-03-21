###################################################################
# Load the dataset

data <- read_dta(file.path(rep_files, "mainvillageregs.dta"))

# Keep villages with area planted lambda
data <- data %>% filter(`_samplePlant` == 1)



# Define the output table file
TABOUT <- file.path(output, "table4.xlsx")

# Define the variables
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")

ZX5 <- c(C, C2, S, p5r, rainS5, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

# Define the SUR model equations
formula_Mt8 <- as.formula(paste("Mt8 ~", paste(ZX8, collapse = " + "), "+", FE_vars))
formula_Mt5 <- as.formula(paste("Mt5 ~", paste(ZX5, collapse = " + "), "+", FE_vars))

# Function for bootstrapping the SUR model with clustering at the district level
# Define a function for bootstrapping with clustering
boot_sureg_run1 <- function(data, indices) {
  d <- data[indices, ]  # Resample data
  model <- systemfit(list(eq1 = formula_Mt8, eq2 = formula_Mt5), data = d, method = "SUR")
  return(coef(model))
}

# Perform bootstrapping with clustering at the district level
set.seed(10101)  # Ensure reproducibility
boot_run1 <- boot(data, boot_sureg_run1, R = 500, strata = data$district)

######arranging output###############################################
#####################################################################
# Extract coefficients from the bootstrapped results and attach names
boot_coefs_1 <-as.data.frame(boot_run1$t)
colnames(boot_coefs_1) <- names(boot_run1$t0)
  
# Select variables for output
vars_2008_run1 <- c("eq1_lnplant_sawahMaxP1", "eq1_lnplant_sawahMinP1", 
                 "eq1_lnN8inSD", "eq1_lnAinSD", 
                 "eq1_ln_dist_keccap", "eq1_ln_ddist_emigctr", 
                 "eq1_P_rice_y5m4_y8m3", "eq1_rain_cumdev678")

# Remove the "eq1_" prefix from all variable names
vars_2008_run1_renamed <- gsub("^eq1_", "", vars_2008_run1)



vars_2005_run1 <- c("eq2_lnplant_sawahMaxP1", "eq2_lnplant_sawahMinP1", 
               "eq2_lnN5inSD", "eq2_lnAinSD", 
               "eq2_ln_dist_keccap", "eq2_ln_ddist_emigctr", 
               "eq2_P_rice_y2m1_y5m3", "eq2_rain_cumdev345")

# extracting var labels of the vars_2008_run1_renamed
var_labels_2008_run1 <- sapply(vars_2008_run1_renamed, function(var) {
  attr(data[[var]], "label")
})
var_labels_2008_run1 <- unlist(var_labels_2008_run1, use.names = FALSE)

# construct 2008 and 2005 datasets - I can't use output from bootstrap directly?
boot_summary_2008 <- data.frame(
  var_name = var_labels_2008_run1 ,  # Use variable names directly
  Estimate_2008_Run1 = sprintf("%.3f", colMeans(boot_coefs_1[, vars_2008_run1], na.rm = TRUE)),
  SE_2008_Run1 = sprintf("(%.3f)", apply(boot_coefs_1[, vars_2008_run1], 2, sd, na.rm = TRUE))
)


boot_summary_2005 <- data.frame(
  Estimate_2005_Run1 = sprintf("%.3f", colMeans(boot_coefs_1[, vars_2005_run1], na.rm = TRUE)),
  SE_2005_Run1 = sprintf("(%.3f)", apply(boot_coefs_1[, vars_2005_run1], 2, sd, na.rm = TRUE))
)


# Repeat variable labels for stacking SE underneath estimates
var_name_repeated <- rep(var_labels_2008_run1, each = 2)

# Replace every second row (SE rows) with an empty string
#var_name_repeated[seq(2, length(var_name_repeated), by = 2)] <- ""

# Create the final boot_summary data frame by merging the 2008 and 2005 data
column_1 <- data.frame(
  var_name = var_name_repeated,  # Now has blanks for SE rows
  `2008` = c(t(boot_summary_2008[, c("Estimate_2008_Run1", "SE_2008_Run1")])),
  `2005` = c(t(boot_summary_2005[, c("Estimate_2005_Run1", "SE_2005_Run1")]))
)

vars_col1 <- unique(c("Mt8", "Mt5", ZX8, ZX5, FE_vars))
n_col1 <- data %>% 
    dplyr::select(all_of(vars_col1)) %>% 
    drop_na() %>% 
     nrow()
print(n_col1)
################################column 2#################
################################ Column 2 #################

# Define variable groups
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

ZX5 <- c(C, C2, S, p5r, rainS5, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

# Define the SUR model equations
formula_Mt8 <- as.formula(paste("Mt8 ~", paste(ZX8, collapse = " + "), "+", FE_vars))
formula_Mt5 <- as.formula(paste("Mt5 ~", paste(ZX5, collapse = " + "), "+", FE_vars))

# Estimate the Seemingly Unrelated Regression (SUR) model
sureg_model_run2 <- systemfit(list(eq1 = formula_Mt8, eq2 = formula_Mt5), data = data, method = "SUR")

# Define a function for bootstrapping the SUR model with clustering at the district level
boot_sureg_run2 <- function(data, indices) {
  d <- data[indices, ]  # Resample data
  model <- systemfit(list(eq1 = formula_Mt8, eq2 = formula_Mt5), data = d, method = "SUR")
  return(coef(model))
}

# Perform bootstrapping with clustering at the district level
set.seed(10101)  # Ensure reproducibility
boot_run2 <- boot(data, boot_sureg_run2, R = 500, strata = data$district)

# Extract coefficients from the bootstrapped results and attach names
boot_coefs_2 <- as.data.frame(boot_run2$t)
colnames(boot_coefs_2) <- names(boot_run2$t0)

# Select specific variables for output
vars_2008_run2 <- c("eq1_Rlambda", "eq1_lnpop8", "eq1_lnN8inSD", 
                     "eq1_lnAinSD", "eq1_ln_dist_keccap", 
                     "eq1_ln_ddist_emigctr", "eq1_P_rice_y5m4_y8m3", 
                     "eq1_rain_cumdev678")

vars_2005_run2 <- c("eq2_Rlambda", "eq2_lnpop5", "eq2_lnN5inSD",
                     "eq2_lnAinSD", "eq2_ln_dist_keccap",
                     "eq2_ln_ddist_emigctr", "eq2_P_rice_y2m1_y5m3",
                     "eq2_rain_cumdev345")

# Remove the equation prefix ("eq1_" and "eq2_")
vars_2008_run2_clean <- gsub("^eq1_", "", vars_2008_run2)
vars_2005_run2_clean <- gsub("^eq2_", "", vars_2005_run2)

# Extract variable labels
var_labels_2008_run2 <- sapply(vars_2008_run2_clean, function(var) {
  attr(data[[var]], "label")
})
var_labels_2008_run2 <- unlist(var_labels_2008_run2, use.names = FALSE)

# Construct 2008 and 2005 datasets
boot_summary_2008 <- data.frame(
  var_name = var_labels_2008_run2,  # Use variable names directly
  Estimate_2008_Run2 = sprintf("%.3f", colMeans(boot_coefs_2[, vars_2008_run2], na.rm = TRUE)),
  SE_2008_Run2 = sprintf("(%.3f)", apply(boot_coefs_2[, vars_2008_run2], 2, sd, na.rm = TRUE))
)

boot_summary_2005 <- data.frame(
  Estimate_2005_Run2 = sprintf("%.3f", colMeans(boot_coefs_2[, vars_2005_run2], na.rm = TRUE)),
  SE_2005_Run2 = sprintf("(%.3f)", apply(boot_coefs_2[, vars_2005_run2], 2, sd, na.rm = TRUE))
)

# Repeat variable labels for stacking SE underneath estimates
var_name_repeated <- rep(var_labels_2008_run2, each = 2)

# Create the final boot_summary data frame by merging the 2008 and 2005 data
column_2 <- data.frame(
  var_name = var_name_repeated,  # Now has blanks for SE rows
  `2008` = c(t(boot_summary_2008[, c("Estimate_2008_Run2", "SE_2008_Run2")])),
  `2005` = c(t(boot_summary_2005[, c("Estimate_2005_Run2", "SE_2005_Run2")]))
)

vars_col2 <- unique(c("Mt8", "Mt5", ZX8, ZX5, FE_vars))
n_col2 <- data %>% 
  dplyr::select(all_of(vars_col2)) %>% 
  drop_na() %>% 
  nrow()
print(n_col2)

##############################column 3############################
##################################################################
############################## Column 3 ############################
####################################################################

# Load Required Libraries
library(glmnet)
library(sandwich)
library(lmtest)
library(dplyr)

#### Step 1: Define Variables ####
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")

ZX5 <- c(C, C2, S, p5r, rainS5, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

#### Step 2: Clean Data ####
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop")
data <- data[complete.cases(data[, required_vars]), ]

#### Step 3: Estimate First Probit Model (Mt5) ####
cat("--> Estimating Probit Model for Mt5...\n")
probit_Mt5_run3 <- glm(as.formula(paste("Mt5 ~", paste(ZX5, collapse = " + "), "+ prop")),
                       data = data, family = binomial(link = "probit"))

# Compute Inverse Mills Ratio (IMR)
data$pred_Mt5_run3 <- predict(probit_Mt5_run3, type = "link")  
data$lambda_Mt5_run3 <- dnorm(data$pred_Mt5_run3) / pnorm(data$pred_Mt5_run3)  

#### Step 4: Estimate Second Probit Model (Mt8) ####
cat("--> Estimating Probit Model for Mt8...\n")
probit_Mt8_run3 <- glm(as.formula(paste("Mt8 ~", paste(ZX8, collapse = " + "), "+ prop + lambda_Mt5_run3")),
                       data = data, family = binomial(link = "probit"))

#### Step 5: Variance-Covariance and Residual Correlation ####
vcov_Mt5_run3 <- vcov(probit_Mt5_run3)
vcov_Mt8_run3 <- vcov(probit_Mt8_run3)

# Residual Correlation (ρ)
data$resid_Mt5_run3 <- residuals(probit_Mt5_run3, type = "response")
data$resid_Mt8_run3 <- residuals(probit_Mt8_run3, type = "response")
rho_estimate_run3 <- cor(data$resid_Mt5_run3, data$resid_Mt8_run3, use = "complete.obs")
print(paste("Residual correlation (ρ):", rho_estimate_run3))

# Cross-equation covariance terms
cross_terms_run3 <- rho_estimate_run3 * sqrt(diag(vcov_Mt5_run3) %*% t(diag(vcov_Mt8_run3)))

# Construct Joint Variance-Covariance Matrix
k_Mt5_run3 <- length(coef(probit_Mt5_run3))
k_Mt8_run3 <- length(coef(probit_Mt8_run3))

joint_vcov_run3 <- matrix(0, nrow = k_Mt5_run3 + k_Mt8_run3, ncol = k_Mt5_run3 + k_Mt8_run3)
joint_vcov_run3[1:k_Mt5_run3, 1:k_Mt5_run3] <- vcov_Mt5_run3
joint_vcov_run3[(k_Mt5_run3+1):(k_Mt5_run3+k_Mt8_run3), (k_Mt5_run3+1):(k_Mt5_run3+k_Mt8_run3)] <- vcov_Mt8_run3
joint_vcov_run3[1:k_Mt5_run3, (k_Mt5_run3+1):(k_Mt5_run3+k_Mt8_run3)] <- cross_terms_run3
joint_vcov_run3[(k_Mt5_run3+1):(k_Mt5_run3+k_Mt8_run3), 1:k_Mt5_run3] <- t(cross_terms_run3)

# Assign names
names_Mt5 <- names(coef(probit_Mt5_run3))
names_Mt8 <- names(coef(probit_Mt8_run3))
joint_names <- c(names_Mt5, names_Mt8)
rownames(joint_vcov_run3) <- joint_names
colnames(joint_vcov_run3) <- joint_names

#### Step 6: Prepare Results Table ####
vars_2008_run3 <- c("lnplant_sawahMaxP1", "lnplant_sawahMinP1", "lnN8inSD", "lnAinSD", 
                    "ln_dist_keccap", "ln_ddist_emigctr", p8r, rainS8)

vars_2005_run3 <- c("lnplant_sawahMaxP1", "lnplant_sawahMinP1", "lnN5inSD", "lnAinSD", 
                    "ln_dist_keccap", "ln_ddist_emigctr", p5r, rainS5)

# Extract coefficients and SEs properly
est_mt8_run3 <- coef(probit_Mt8_run3)[vars_2008_run3]
est_mt5_run3 <- coef(probit_Mt5_run3)[vars_2005_run3]

se_mt8_run3 <- sqrt(diag(joint_vcov_run3))[match(vars_2008_run3, colnames(joint_vcov_run3))]
se_mt5_run3 <- sqrt(diag(joint_vcov_run3))[match(vars_2005_run3, colnames(joint_vcov_run3))]

# If you have var_labels_2008_run1:
# var_labels_2008_run1 <- c(...) # Add appropriate variable labels!

var_name_repeated <- rep(var_labels_2008_run1, each = 2)  # reuse from Run1

#### Step 7: Format Output ####
column_3 <- data.frame(
  var_name = var_name_repeated,  
  `2008` = c(rbind(sprintf("%.3f", est_mt8_run3), sprintf("(%.3f)", se_mt8_run3))),
  `2005` = c(rbind(sprintf("%.3f", est_mt5_run3), sprintf("(%.3f)", se_mt5_run3)))
)

#### Step 8: Print Final Table ####
print(column_3)
print(paste("Residual correlation (ρ):", round(rho_estimate_run3, 3)))



#########################run 4#####################
####################################################
#########################run 4#####################
###################################################

Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

ZX5 <- c(C, C2, S, p5r, rainS5, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

# Step 0: Identify rows without NA in required variables
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop")  # Variables needed for the models
data <- data[complete.cases(data[, required_vars]), ]  # Retain only complete observations

# Step 1: Estimate First Probit Model (Mt5) Without Firth's Penalty
cat("--> Estimating Probit Model for Mt5...\n")
probit_Mt5_run4 <- tryCatch(
  glm(as.formula(paste("Mt5 ~", paste(ZX5, collapse = " + "), "+ prop")),
      data = data, family = binomial(link = "probit")),
  error = function(e) {
    cat("Warning: Standard probit failed. Switching to regularized probit.\n")
    cv_model <- cv.glmnet(as.matrix(data[, ZX5]), data$Mt5, family = "binomial", alpha = 0, lambda = 0.01)
    glmnet_model <- glmnet(as.matrix(data[, ZX5]), data$Mt5, family = "binomial", alpha = 0, lambda = cv_model$lambda.min)
    return(glmnet_model)
  }
)

# Compute the Inverse Mills Ratio (IMR) Using Mt5
data$pred_Mt5_run4 <- predict(probit_Mt5_run4, type = "link")  
data$lambda_Mt5_run4 <- dnorm(data$pred_Mt5_run4) / pnorm(data$pred_Mt5_run4)  

# Step 2: Estimate Second Probit Model (Mt8) Including IMR
cat("--> Estimating Probit Model for Mt8...\n")
probit_Mt8_run4 <- tryCatch(
  glm(as.formula(paste("Mt8 ~", paste(ZX8, collapse = " + "), "+ prop + lambda_Mt5_run4")),
      data = data, family = binomial(link = "probit")),
  error = function(e) {
    cat("Warning: Standard probit failed. Switching to regularized probit.\n")
    cv_model <- cv.glmnet(as.matrix(data[, ZX8]), data$Mt8, family = "binomial", alpha = 0, lambda = 0.01)
    glmnet_model <- glmnet(as.matrix(data[, ZX8]), data$Mt8, family = "binomial", alpha = 0, lambda = cv_model$lambda.min)
    return(glmnet_model)
  }
)

# Compute Variance-Covariance Matrices
vcov_Mt5_run4 <- vcov(probit_Mt5_run4)  
vcov_Mt8_run4 <- vcov(probit_Mt8_run4)  

# Compute Residual Correlation (ρ)
data$resid_Mt5_run4 <- residuals(probit_Mt5_run4, type = "response")
data$resid_Mt8_run4 <- residuals(probit_Mt8_run4, type = "response")

rho_estimate_run4 <- cor(data$resid_Mt5_run4, data$resid_Mt8_run4, use = "complete.obs")
print(paste("Residual correlation (ρ):", rho_estimate_run4))

# Compute cross-equation covariance terms using ρ
cross_terms_run4 <- rho_estimate_run4 * sqrt(diag(vcov_Mt5_run4) %*% t(diag(vcov_Mt8_run4)))

# Construct Corrected Variance-Covariance Matrix
k_Mt5_run4 <- length(coef(probit_Mt5_run4))
k_Mt8_run4 <- length(coef(probit_Mt8_run4))

joint_vcov_run4 <- matrix(0, nrow = k_Mt5_run4 + k_Mt8_run4, ncol = k_Mt5_run4 + k_Mt8_run4)

# Fill diagonal blocks with individual variance-covariance matrices
joint_vcov_run4[1:k_Mt5_run4, 1:k_Mt5_run4] <- vcov_Mt5_run4
joint_vcov_run4[(k_Mt5_run4+1):(k_Mt5_run4+k_Mt8_run4), (k_Mt5_run4+1):(k_Mt5_run4+k_Mt8_run4)] <- vcov_Mt8_run4

# Insert cross-equation covariance terms
joint_vcov_run4[1:k_Mt5_run4, (k_Mt5_run4+1):(k_Mt5_run4+k_Mt8_run4)] <- cross_terms_run4
joint_vcov_run4[(k_Mt5_run4+1):(k_Mt5_run4+k_Mt8_run4), 1:k_Mt5_run4] <- t(cross_terms_run4)

# Apply names for clarity
names_Mt5_run4 <- names(coef(probit_Mt5_run4))
names_Mt8_run4 <- names(coef(probit_Mt8_run4))
rownames(joint_vcov_run4) <- c(names_Mt5_run4, names_Mt8_run4)
colnames(joint_vcov_run4) <- c(names_Mt5_run4, names_Mt8_run4)

# Step 7: Prepare Results Table
vars_2008_run4 <- c("Rlambda", "lnpop8", "lnN8inSD", "lnAinSD", 
                    "ln_dist_keccap", "ln_ddist_emigctr", "P_rice_y5m4_y8m3", "rain_cumdev678")

vars_2005_run4 <- c("Rlambda", "lnpop5", "lnN5inSD", "lnAinSD", 
                    "ln_dist_keccap", "ln_ddist_emigctr", "P_rice_y2m1_y5m3", "rain_cumdev345")

# Extract available coefficients
vars_mt8_available_run4 <- intersect(vars_2008_run4, rownames(joint_vcov_run4))
vars_mt5_available_run4 <- intersect(vars_2005_run4, rownames(joint_vcov_run4))

est_mt8_run4 <- coef(probit_Mt8_run4)[vars_mt8_available_run4]
est_mt5_run4 <- coef(probit_Mt5_run4)[vars_mt5_available_run4]

# Extract Corrected Standard Errors
se_mt8_run4 <- sqrt(diag(joint_vcov_run4))[vars_mt8_available_run4]
se_mt5_run4 <- sqrt(diag(joint_vcov_run4))[vars_mt5_available_run4]

var_name_repeated <- rep(var_labels_2008_run2, each = 2)

# Step 8: Create a Wide-Format Results Table
column_4 <- data.frame(
  var_name = var_name_repeated,  
  `2008` = c(rbind(sprintf("%.3f", est_mt8_run4), sprintf("(%.3f)", se_mt8_run4))),
  `2005` = c(rbind(sprintf("%.3f", est_mt5_run4), sprintf("(%.3f)", se_mt5_run4)))
)


#################################################################3

# Store data frames in a list
df_list <- list(column_1, column_2, column_3, column_4)

# Iterate through the list and modify each data frame correctly
df_list <- lapply(df_list, function(df) {
  df$id <- rep(c(1, 2), times = nrow(df) / 2)  # Assign "id" (1 = estimate, 2 = SE)
  df$group <- rep(1:(nrow(df) / 2), each = 2)  # Assign "group" to pairs
  return(df)
})

# Reassign modified data frames back to their original names
column_1 <- df_list[[1]]
column_2 <- df_list[[2]]
column_3 <- df_list[[3]]
column_4 <- df_list[[4]]


# Efficient merging of all columns into a final dataset
final_merged <- Reduce(function(x, y) full_join(x, y, by = c("var_name", "group", "id")), 
                       list(column_1, column_2, column_3, column_4))

# Ensure numeric values for sorting
final_merged <- final_merged %>%
  mutate(group = as.numeric(group),
         id = as.numeric(id))

# Assign priority to selected variables using `grepl()`
final_merged <- final_merged %>%
  mutate(var_priority = case_when(
    grepl("log maximum landholdings|log minimum landholdings", var_name) ~ 1,
    grepl("Pareto exponent|log village population", var_name) ~ 2,
    TRUE ~ 3
  ))


# Ensure correct column ordering
final_merged$column_priority <- match(final_merged$var_name, final_merged$var_priority)

# Order dataset correctly
final_merged <- final_merged %>%
  arrange(var_priority, group, column_priority) %>%
  dplyr::select(-var_priority, -column_priority, -group, -id)  # Drop auxiliary sorting columns

# Generate alternating column names (assuming 8 columns)
year_labels <- rep(c(2008, 2005), length.out = ncol(final_merged) - 1)  # Excluding 'var_name'

# Assign new column names
colnames(final_merged) <- c("var_name", year_labels)


village_counts_row <- data.frame(
  var_name = "Number of Villages",
  `(1)` = n_col1,
  `(2)` = n_col2,
  `(3)` = n_col1,
  `(4)` = n_col2,
  `(5)` = n_col1,
  `(6)` = n_col2,
  `(7)` = n_col1,
  `(8)` = n_col2,
  check.names = FALSE
)


# Ensure no remaining NA values
final_merged[is.na(final_merged)] <- ""
# Ensure no repeated `var_name` for standard errors
final_merged$var_name[seq(2, nrow(final_merged), by = 2)] <- ""

# Define the first row: Model Names spanning across respective columns
model_names <- c("", "SU-LPM", "SU-LPM", "SU-LPM", "SU-LPM", 
                 "Bivariate probit", "Bivariate probit", "Bivariate probit", "Bivariate probit")

# Define the second row: (1), (2), (3), (4) labels spanning two columns each
group_labels <- c("", "(1)", "(1)", "(2)", "(2)", "(3)", "(3)", "(4)", "(4)")

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

# Write header rows manually
writeData(wb, sheet = 1, x = as.data.frame(t(model_names)), startRow = 1, colNames = FALSE)
writeData(wb, sheet = 1, x = as.data.frame(t(group_labels)), startRow = 2, colNames = FALSE)
writeData(wb, sheet = 1, x = as.data.frame(t(year_labels)), startRow = 3, colNames = FALSE)

# Write actual data starting from row 4
writeData(wb, sheet = 1, x = final_merged, startRow = 4, colNames = FALSE)

# Find the next empty row (immediately after the data ends)
last_row <- 4 + nrow(final_merged)
empty_row <- last_row + 1

# Add an empty row explicitly
writeData(wb, sheet = 1, x = rep("", ncol(final_merged)), startRow = empty_row, colNames = FALSE)

# Add village count row at the row following the empty row
writeData(wb, sheet = 1, x = village_counts_row, startRow = empty_row + 1, colNames = FALSE)

# Merge cells for village counts explicitly: (2-3), (4-5), (6-7), (8-9)
mergeCells(wb, sheet = 1, cols = 2:3, rows = empty_row + 1)
mergeCells(wb, sheet = 1, cols = 4:5, rows = empty_row + 1)
mergeCells(wb, sheet = 1, cols = 6:7, rows = empty_row + 1)
mergeCells(wb, sheet = 1, cols = 8:9, rows = empty_row + 1)

# Merge first row for model names
mergeCells(wb, sheet = 1, cols = 2:5, rows = 1)  # SU-LPM spans columns 2-5
mergeCells(wb, sheet = 1, cols = 6:9, rows = 1)  # Bivariate probit spans columns 6-9

# Merge second row for (1), (2), (3), (4)
mergeCells(wb, sheet = 1, cols = 2:3, rows = 2)
mergeCells(wb, sheet = 1, cols = 4:5, rows = 2)
mergeCells(wb, sheet = 1, cols = 6:7, rows = 2)
mergeCells(wb, sheet = 1, cols = 8:9, rows = 2)

# Save workbook clearly
saveWorkbook(wb, TABOUT, overwrite = TRUE)

cat("Table 4 replicated (with village counts) to:", TABOUT, "\n")


############latex#############################################


final_merged[is.na(final_merged)] <- ""

# Convert empty row into a long-format structure **without column names**
empty_row_long <- as.data.frame(matrix("", nrow = 2, ncol = ncol(final_merged)))  # Two empty rows
colnames(empty_row_long) <- colnames(final_merged)  # Ensure the column names remain unchanged

# Merge empty rows while keeping final_merged column names intact
final_table <- bind_rows(final_merged, empty_row_long)


# Ensure column names remain consistent for LaTeX
colnames(final_table) <- c("Variable", rep(c("2008", "2005"), 4))

# Generate LaTeX table
latex_table <- final_table %>%
  kable("latex", booktabs = TRUE, linesep = "", escape = FALSE) %>%
  add_header_above(c(" " = 1, "(1)" = 2, "(2)" = 2, "(3)" = 2, "(4)" = 2)) %>%
  add_header_above(c(" " = 1, "SU-LPM" = 4, "Bivariate probit" = 4)) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  column_spec(1, width = "2.8cm") %>%
  row_spec(nrow(final_table), bold = TRUE)

# Explicitly merge columns for the **village count row**
village_row_idx <- nrow(final_table)
latex_table <- latex_table %>%
  row_spec(village_row_idx, extra_latex_after = paste0(
    "\\\\[-1.8ex]\n",
    "\\multicolumn{1}{l}{Number of Villages}",
    " & \\multicolumn{2}{c}{", village_counts_row$`(1)`, "}",
    " & \\multicolumn{2}{c}{", village_counts_row$`(2)`, "}",
    " & \\multicolumn{2}{c}{", village_counts_row$`(3)`, "}",
    " & \\multicolumn{2}{c}{", village_counts_row$`(4)`, "} \\\\\n\\bottomrule"
  ))

# Save LaTeX output
TABOUT_TEX <- gsub("\\.xlsx$", ".tex", TABOUT)
writeLines(latex_table, TABOUT_TEX)

cat("Table 4 tex replicated clearly to:", TABOUT_TEX, "\n")  