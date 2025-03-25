data <- read_dta(file.path(rep_files, "mainvillageregs.dta"))

# Keep villages with area planted lambda
data <- data %>% filter(`_samplePlant` == 1)


# ---------------------column 1--------------------------------#
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


# -----------------------Panel A---------------------------------#
EST <- DNV

# ---------------------------
# Column 2: Bootstrapped DNV Model
# ---------------------------
X5_col2 <- c(R, p5r, rainS5, C, C2, S)  
X8_col2 <- c(R, p8r, rainS8, C, C2, S)  
X_col2  <- c(R, C, C2, S, pDr, rainD)  
BETA_col2 <- c(pDr, rainD)
FE_vars <- c("prop")


cat("\n--> Running BOOTtse() for Column 2\n")
boot_col2 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col2, Z8 = Z8, X5 = X5_col2, Z5 = Z5, 
                     X = X_col2, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = "district",
                     BETA = BETA_col2,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col2.csv"))

# Store only BETA coefficients and SE in a named data frame
results[[2]] <- data.frame(variable = BETA_col2, 
                           coefficient = boot_col2$bootstrap_ci$BETA, 
                           standard_error = boot_col2$bootstrap_ci$SE)



#####################joint significance test########################
library(car)  # For linearHypothesis()


kappa_sig <- function(DNV_result) {
  
  final_model <- DNV_result$model
  
  # Extract kappa variable names
  kappa_vars <- grep("^kappa", names(DNV_result$data), value = TRUE)
  
  # Run joint significance test
  joint_test <- linearHypothesis(final_model, kappa_vars, vcov = vcov(final_model, cluster = DNV_result$data$district))
  
  # Extract p-value
  p_val <- joint_test[2, "Pr(>Chisq)"]
  
  # Assign significance stars
  sig_star <- ifelse(p_val < 0.001, "***",
                     ifelse(p_val < 0.01, "**",
                            ifelse(p_val < 0.05, "*", "")))
  
  return(sig_star)
}


#
DNV_result <- DNV(data = data,
                  X5 = X5_col2,
                  X8 = X8_col2,
                  Z5 = Z5,
                  Z8 = Z8,
                  Mt5 = "Mt5",
                  Mt8 = "Mt8",
                  M = "d_ln_emig_shr",
                  X = X_col2,
                  FE_vars = "prop",
                  J_vars = "district",
                  kpowD = 3,
                  BETA = BETA_col2)

sig_col2 <- kappa_sig(DNV_result)
print(sig_col2)





# ---------------------------
# Column 3: Bootstrapped DNV Model with Rain Shock Lags
# ---------------------------
X5_col3 <- c(R, p5r, rainS5L, C, C2, S)  
X8_col3 <- c(R, p8r, rainS8L, C, C2, S)  
X_col3  <- c(R, C, C2, S, pDr, rainDL)  
BETA_col3 <- c(pDr, rainDL)
FE_vars <- c("prop")
cat("\n--> Running BOOTtse() for Column 3\n")
boot_col3 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col3, Z8 = Z8, X5 = X5_col3, Z5 = Z5, 
                     X = X_col3, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col3,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col3.csv"))

# Store only BETA coefficients and SE in a named data frame
results[[3]] <- data.frame(variable = BETA_col3, 
                           coefficient = boot_col3$bootstrap_ci$BETA, 
                           standard_error = boot_col3$bootstrap_ci$SE)


# After running DNV:
DNV_result <- DNV(data = data,
                  X5 = X5_col3,
                  X8 = X8_col3,
                  Z5 = Z5,
                  Z8 = Z8,
                  Mt5 = "Mt5",
                  Mt8 = "Mt8",
                  M = "d_ln_emig_shr",
                  X = X_col3,
                  FE_vars = "prop",
                  J_vars = "district",
                  kpowD = 3,
                  BETA = BETA_col3)

sig_col3 <- kappa_sig(DNV_result)



# ---------------------------
# Column 4: Bootstrapped DNV Model with Lagged Prices
# ---------------------------
X5_col4 <- c(R, p5rL, rainS5L, C, C2, S)  
X8_col4 <- c(R, p8rL, rainS8L, C, C2, S)  
X_col4  <- c(R, C, C2, S, pDrL, rainDL)  
BETA_col4 <- c(pDrL, rainDL)

cat("\n--> Running BOOTtse() for Column 4\n")
boot_col4 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col4, Z8 = Z8, X5 = X5_col4, Z5 = Z5, 
                     X = X_col4, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col4,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col4.csv"))

results[[4]] <- data.frame(variable = BETA_col4, 
                           coefficient = boot_col4$bootstrap_ci$BETA, 
                           standard_error = boot_col4$bootstrap_ci$SE)

# After running DNV:
DNV_result <- DNV(data = data,
                  X5 = X5_col4,
                  X8 = X8_col4,
                  Z5 = Z5,
                  Z8 = Z8,
                  Mt5 = "Mt5",
                  Mt8 = "Mt8",
                  M = "d_ln_emig_shr",
                  X = X_col4,
                  FE_vars = "prop",
                  J_vars = "district",
                  kpowD = 3,
                  BETA = BETA_col4)

sig_col4 <- kappa_sig(DNV_result)


# ---------------------------
# Column 5: Bootstrapped DNV Model with Full Controls
# ---------------------------
X5_col5 <- c(R, C, C2, S, p5rF, rainS5F)  
X8_col5 <- c(R, C, C2, S, p8rF, rainS8F)  
X_col5  <- c(R, C, C2, S, pDrF, rainDF)  
BETA_col5 <- c(rainDF, pDrF)

cat("\n--> Running BOOTtse() for Column 5\n")
boot_col5 <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col5, Z8 = Z8, X5 = X5_col5, Z5 = Z5, 
                     X = X_col5, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col5,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col5.csv"))

results[[5]] <- data.frame(variable = BETA_col5, 
                           coefficient = boot_col5$bootstrap_ci$BETA, 
                           standard_error = boot_col5$bootstrap_ci$SE)




DNV_result <- DNV(data = data,
                  X5 = X5_col5,
                  X8 = X8_col5,
                  Z5 = Z5,
                  Z8 = Z8,
                  Mt5 = "Mt5",
                  Mt8 = "Mt8",
                  M = "d_ln_emig_shr",
                  X = X_col5,
                  FE_vars = "prop",
                  J_vars = "district",
                  kpowD = 3,
                  BETA = BETA_col5)

sig_col5 <- kappa_sig(DNV_result)

# -----------------------Panel B---------------------------------#
EST <- POIRIER
# Define variable sets for each column


# ---------------------------
# Column 6
# ---------------------------
X5_6 <- c(R, p5r, rainS5, C, C2, S)
X8_6 <- c(R, p8r, rainS8, C, C2, S)
X_6  <- c(R, C, C2, S, pDr, rainD)
BETA_col6 <- c(pDr, rainD)
FE_vars <- c("prop")
cat("\n--> Running BOOTtse() for Column 6\n")

boot_col6 <- PBOOTtse(data, EST = POIRIER,
                     X8 = X8_6, Z8 = Z8, X5 = X5_6, Z5 = Z5, 
                     X = X_6, M = M, Mt5 = "Mt5", Mt8 = "Mt8",
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col6, 
                     REPS = 500, output_file = file.path(output, "BOOT_DATA_col6.csv"))

results[[6]] <- data.frame(variable = BETA_col6, 
                           coefficient = boot_col6$bootstrap_ci$BETA, 
                           standard_error = boot_col6$bootstrap_ci$SE)


#####################joint significance test########################
library(car)  # For linearHypothesis()


kappa_sig <- function(P_result) {
  
  final_model <- P_result$model
  
  # Extract kappa variable names
  kappa_vars <- grep("^kappa", names(P_result$data), value = TRUE)
  
  # Run joint significance test
  joint_test <- linearHypothesis(final_model, kappa_vars, vcov = vcov(final_model, cluster = P_result$data$district))
  
  # Extract p-value
  p_val <- joint_test[2, "Pr(>Chisq)"]
  
  # Assign significance stars
  sig_star <- ifelse(p_val < 0.001, "***",
                     ifelse(p_val < 0.01, "**",
                            ifelse(p_val < 0.05, "*", "")))
  
  return(sig_star)
}


#
P_result <- POIRIER(data = data,
                  X5 = X5_6,
                  X8 = X8_6,
                  Z5 = Z5,
                  Z8 = Z8,
                  Mt5 = "Mt5",
                  Mt8 = "Mt8",
                  M = "d_ln_emig_shr",
                  X = X_6,
                  FE_vars = "prop",
                  J_vars = "district",
                  BETA = BETA_col6)

sig_col2 <- kappa_sig(P_result)

print(sig_col6)


# ---------------------------
# Column 7
# ---------------------------
X5_7 <- c(R, p5r, rainS5L, C, C2, S)
X8_7 <- c(R, p8r, rainS8L, C, C2, S)
X_7  <- c(R, C, C2, S, pDr, rainDL)
BETA_col7 <- c(pDr, rainDL)

cat("\n--> Running BOOTtse() for Column 7\n")
boot_col7 <- PBOOTtse(data, EST=POIRIER, 
                     X8 = X8_7, Z8 = Z8, X5 = X5_7, Z5 = Z5, 
                     X = X_7, M = M, Mt5 = "Mt5", Mt8 = "Mt8",
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col7, 
                     REPS = 500, output_file = file.path(output, "BOOT_DATA_col7.csv"))



results[[7]] <- data.frame(variable = BETA_col7, 
                           coefficient = boot_col5$bootstrap_ci$BETA, 
                           standard_error = boot_col5$bootstrap_ci$SE)



#
P_result <- POIRIER(data = data,
                    X5 = X5_7,
                    X8 = X8_7,
                    Z5 = Z5,
                    Z8 = Z8,
                    Mt5 = "Mt5",
                    Mt8 = "Mt8",
                    M = "d_ln_emig_shr",
                    X = X_7,
                    FE_vars = "prop",
                    J_vars = "district",
                    BETA = BETA_col7)

sig_col7 <- kappa_sig(P_result)

print(sig_col7)
# ---------------------------
# Column 8
# ---------------------------
X5_8 <- c(R, p5rL, rainS5L, C, C2, S)
X8_8 <- c(R, p8rL, rainS8L, C, C2, S)
X_8  <- c(R, C, C2, S, pDrL, rainDL)
BETA_col8 <- c(pDrL, rainDL)

cat("\n--> Running BOOTtse() for Column 8\n")
boot_col8 <- PBOOTtse(data, EST = POIRIER, 
                     X8 = X8_8, Z8 = Z8, X5 = X5_8, Z5 = Z5, 
                     X = X_8, M = M, Mt5 = "Mt5", Mt8 = "Mt8",
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col8, 
                     REPS = 500, output_file = file.path(output, "BOOT_DATA_col8.csv"))



results[[8]] <- data.frame(variable = BETA_col8, 
                           coefficient = boot_col5$bootstrap_ci$BETA, 
                           standard_error = boot_col5$bootstrap_ci$SE)

P_result <- POIRIER(data = data,
                    X5 = X5_8,
                    X8 = X8_8,
                    Z5 = Z5,
                    Z8 = Z8,
                    Mt5 = "Mt5",
                    Mt8 = "Mt8",
                    M = "d_ln_emig_shr",
                    X = X_8,
                    FE_vars = "prop",
                    J_vars = "district",
                    BETA = BETA_col8)

sig_col8 <- kappa_sig(P_result)

print(sig_col8)

# ---------------------------
# Column 9
# ---------------------------
X5_9 <- c(R, C, C2, S, p5rF, rainS5F)
X8_9 <- c(R, C, C2, S, p8rF, rainS8F)
X_9  <- c(R, C, C2, S, pDrF, rainDF)
BETA_col9 <- c(rainDF, pDrF)

cat("\n--> Running BOOTtse() for Column 9\n")
boot_col9 <- PBOOTtse(data, EST= POIRIER, 
                     X8 = X8_9, Z8 = Z8, X5 = X5_9, Z5 = Z5, 
                     X = X_9, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     BETA = BETA_col9, 
                     REPS = 500, output_file = file.path(output, "BOOT_DATA_col9.csv"))


results[[9]] <- data.frame(variable = BETA_col9, 
                           coefficient = boot_col5$bootstrap_ci$BETA, 
                           standard_error = boot_col5$bootstrap_ci$SE)

P_result <- POIRIER(data = data,
                    X5 = X5_9,
                    X8 = X8_9,
                    Z5 = Z5,
                    Z8 = Z8,
                    Mt5 = "Mt5",
                    Mt8 = "Mt8",
                    M = "d_ln_emig_shr",
                    X = X_9,
                    FE_vars = "prop",
                    J_vars = "district",
                    BETA = BETA_col9)

sig_col9 <- kappa_sig(P_result)

#################readjusting result format##################################
# ----------------------
# Flatten variable columns in results
# ----------------------

for (i in seq_along(results)) {
  if (!is.null(results[[i]]) && "variable" %in% names(results[[i]])) {
    # Unlist variable column if it's a list
    if (is.list(results[[i]]$variable)) {
      results[[i]]$variable <- unlist(results[[i]]$variable)
    }
    # Ensure result is a data frame
    results[[i]] <- as.data.frame(results[[i]])
  }
}


#####################################
#####################################
#----------------------
# Panel A: Semi-parametric
#----------------------

# Flatten 'variable' columns in results for Panel A
for (i in 1:5) {
  if ("variable" %in% names(results[[i]])) {
    results[[i]]$variable <- unlist(results[[i]]$variable)
  }
}

# Extract all variable names from columns 1-5
all_vars_A <- unique(unlist(lapply(results[1:5], function(df) df$variable)))

# Create base data frame
panelA_table <- data.frame(variable = all_vars_A, stringsAsFactors = FALSE)

# Format and merge results
for (i in 1:5) {
  formatted_results <- results[[i]] %>%
    mutate(
      coefficient = sprintf("%.3f", coefficient),
      standard_error = sprintf("(%.3f)", standard_error)
    ) %>%
    select(variable, coefficient, standard_error)
  names(formatted_results) <- c("variable", paste0("Col", i, "_coef"), paste0("Col", i, "_se"))
  panelA_table <- full_join(panelA_table, formatted_results, by = "variable")
}

panelA_table <- panelA_table %>%
  mutate(across(starts_with("Col"), ~ replace_na(.x, "")))

panelA_table_long <- panelA_table %>%
  pivot_longer(cols = starts_with("Col"), names_to = "column", values_to = "value") %>%
  separate(column, into = c("column", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = "column", values_from = "value") %>%
  arrange(variable, match(type, c("coef", "se"))) %>%
  select(-type) %>%
  mutate(variable = ifelse(row_number() %% 2 == 0, "", variable))

colnames(panelA_table_long)[1] <- "Panel A. Semiparametric correction procedure"
colnames(panelA_table_long)[-1] <- paste0("(", 1:5, ")")

#----------------------
# Panel B: Parametric
#----------------------

# Flatten 'variable' columns in results for Panel B
for (i in 6:9) {
  if ("variable" %in% names(results[[i]])) {
    results[[i]]$variable <- unlist(results[[i]]$variable)
  }
}

all_vars_B <- unique(unlist(lapply(results[6:9], function(df) df$variable)))

panelB_table <- data.frame(variable = all_vars_B, stringsAsFactors = FALSE)

for (i in 6:9) {
  formatted_results <- results[[i]] %>%
    mutate(
      coefficient = sprintf("%.3f", coefficient),
      standard_error = sprintf("(%.3f)", standard_error)
    ) %>%
    select(variable, coefficient, standard_error)
  names(formatted_results) <- c("variable", paste0("Col", i, "_coef"), paste0("Col", i, "_se"))
  panelB_table <- full_join(panelB_table, formatted_results, by = "variable")
}

panelB_table <- panelB_table %>%
  mutate(across(starts_with("Col"), ~ replace_na(.x, ""))) %>%
  mutate(empty_col = "")

panelB_table_long <- panelB_table %>%
  pivot_longer(cols = starts_with("Col"), names_to = "column", values_to = "value") %>%
  separate(column, into = c("column", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = "column", values_from = "value") %>%
  arrange(variable, match(type, c("coef", "se"))) %>%
  select(-type) %>%
  mutate(variable = ifelse(row_number() %% 2 == 0, "", variable))

colnames(panelB_table_long)[1] <- "Panel B. Parametric correction procedure"
colnames(panelB_table_long)[2] <- "-"
colnames(panelB_table_long)[3:6] <- paste0("(", 6:9, ")")



##############joint significance test###############
#-------------panel A significance-----------#
# Create correct joint significance row
kappa_significance_row <- as.data.frame(matrix("", nrow = 1, ncol = length(panelA_colnames)))
colnames(kappa_significance_row) <- panelA_colnames

# Set first column
kappa_significance_row[1, 1] <- "\\text{Joint significance of selection correction terms}"

# Assign sig_col values correctly
kappa_significance_row[1, 2:6] <- c("-", sig_col2, sig_col3, sig_col4, sig_col5)

# Add two empty rows and the joint significance row
empty_row_long <- as.data.frame(matrix("", nrow = 2, ncol = ncol(panelA_table_long)))
colnames(empty_row_long) <- panelA_colnames
panelA_table_long <- rbind(panelA_table_long, empty_row_long)
panelA_table_long <- rbind(panelA_table_long, kappa_significance_row)

#-------------panel B significance-----------#

# Create correct joint significance row
kappa_significance_B <- as.data.frame(matrix("", nrow = 1, ncol = length(panelB_colnames)))
colnames(kappa_significance_B) <- panelB_colnames

# Set first column
kappa_significance_B[1, 1] <- "\\text{Joint significance of selection correction terms}"

# Assign sig_col values correctly
kappa_significance_B[1, 2:6] <- c("-", sig_col6, sig_col7, sig_col8, sig_col9)

# Add two empty rows and the joint significance row
empty_row_long <- as.data.frame(matrix("", nrow = 2, ncol = ncol(panelB_table_long)))
colnames(empty_row_long) <- panelB_colnames
panelB_table_long <- rbind(panelB_table_long, empty_row_long)
panelB_table_long <- rbind(panelB_table_long, kappa_significance_B)

##############village count################################
# Function to count the number of valid observations (villages used)
count_valid_rows <- function(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var) {
  required_vars <- unique(c(X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var))
  required_vars <- required_vars[required_vars %in% names(data)]  # Keep only variables in dataset
  valid_data <- na.omit(data[, required_vars])  # Remove rows with NA values
  return(nrow(valid_data))  # Return number of valid rows
}

# Count villages for each column in Panel A
num_villages_panelA <- c(
  count_valid_rows(data, Z8, Z5, X5, X8, X, M, FE_vars, J_vars, cluster_var),
  count_valid_rows(data, Z8, Z5, X5_col2, X8_col2, X_col2, M, FE_vars, J_vars, cluster_var),
  count_valid_rows(data, Z8, Z5, X5_col3, X8_col3, X_col3, M, FE_vars, J_vars, cluster_var),
  count_valid_rows(data, Z8, Z5, X5_col4, X8_col4, X_col4, M, FE_vars, J_vars, cluster_var),
  count_valid_rows(data, Z8, Z5, X5_col5, X8_col5, X_col5, M, FE_vars, J_vars, cluster_var)
)


# Create "Number of Villages" row for Panel A
num_villages_row_panelA <- data.frame(
  "Panel A. Semiparametric correction procedure" = "\\textbf{Number of villages}",
  `(1)` = as.character(num_villages_panelA[1]),
  `(2)` = as.character(num_villages_panelA[2]),
  `(3)` = as.character(num_villages_panelA[3]),
  `(4)` = as.character(num_villages_panelA[4]),
  `(5)` = as.character(num_villages_panelA[5])
)
# Extract column names from Panel B to match structure
panelB_colnames <- colnames(panelB_table_long)
# Ensure "Number of villages" row has the same column names as Panel B
num_villages_row_panelA <- as.data.frame(matrix("", nrow = 1, ncol = length(panelB_colnames)))
colnames(num_villages_row_panelA) <- panelB_colnames  # Assign column names from Panel B
# Set the first column to the label
num_villages_row_panelA[1, 1] <- "\\textbf{Number of villages}"
# Assign village counts to the correct columns
for (i in 2:length(panelB_colnames)) {
  num_villages_row_panelA[1, i] <- as.character(num_villages_panelA[i - 1])  # Offset by 1 due to first column being label
}
# Display structure for debugging
print(num_villages_row_panelA)


empty_row_long <- as.data.frame(matrix("", nrow = 2, ncol = ncol(panelB_table_long)))
colnames(empty_row_long) <- panelB_colnames
panelB_table_long <- rbind(panelB_table_long, empty_row_long)
panelB_table_long <- rbind(panelB_table_long, num_villages_row_panelA)
# -------------------------------
# Save Panel A and B Table to Excel
# -------------------------------

write.xlsx(panelA_table_long, file = output_panelA_path, overwrite = TRUE)

write.xlsx(panelB_table_long, file = output_panelA_path, overwrite = TRUE)

#######################tex output#############################

# Generate LaTeX table for Panel A (without bottom rule)
latex_output_A <- panelA_table_long %>%
  kable("latex", booktabs = TRUE, align = "lccccc", escape = FALSE) %>%
  add_header_above(c(" " = 1, "OLS" = 5)) %>%
  kable_styling(latex_options = c("HOLD_position"), font_size = 10) %>%
  str_replace("\\\\bottomrule", "")  # Remove bottom rule from Panel A

# Generate LaTeX table for Panel B (without header, keeping structure)
latex_output_B <- panelB_table_long %>%
  kable("latex", booktabs = TRUE, align = "lccccc", escape = FALSE) %>%
  kable_styling(latex_options = c("HOLD_position"), font_size = 10) %>%
  str_replace(".*?\\\\midrule", "")  # Remove header line (everything before \midrule)

# Combine Panel A and Panel B, adding only one bottom rule
writeLines(c(latex_output_A, latex_output_B, "\\bottomrule"), file.path(output, "table5.tex"))
