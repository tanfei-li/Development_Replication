data <- read_dta(file.path(rep_files, "mainvillageregs.dta"))

# Keep villages with area planted lambda
data <- data %>% filter(`_samplePlant` == 1)



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

# -----------------------Panel A---------------------------------#
EST <- DNV

# ---------------------------
# Column 2: Bootstrapped DNV Model
# ---------------------------
X5_col2 <- c(R, p5r, rainS5, C, C2, S)  
X8_col2 <- c(R, p8r, rainS8, C, C2, S)  
X_col2  <- c(R, C, C2, S, pDr, rainD)  
BETA_col2 <- c(pDr, rainD)

cat("\n--> Running BOOTtse() for Column 2\n")
boot_col2_test <- BOOTtse(data, EST = DNV, 
                     X8 = X8_col2, Z8 = Z8, X5 = X5_col2, Z5 = Z5, 
                     X = X_col2, kpowD = 3, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_col2,
                     REPS = 10, output_file = file.path(output, "BOOT_DATA_col2.csv"))

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
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col3.csv"))

# Store only BETA coefficients and SE in a named data frame
results[[3]] <- data.frame(variable = BETA_col3, 
                           coefficient = boot_col3$bootstrap_ci$BETA, 
                           standard_error = boot_col3$bootstrap_ci$SE)



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
                     cluster_var = "district", BETA = BETA_col4,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col4.csv"))

results[[4]] <- data.frame(variable = BETA_col4, 
                           coefficient = boot_col4$bootstrap_ci$BETA, 
                           standard_error = boot_col4$bootstrap_ci$SE)

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
                     cluster_var = "district", BETA = BETA_col5,
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col5.csv"))

results[[5]] <- data.frame(variable = BETA_col5, 
                           coefficient = boot_col5$bootstrap_ci$BETA, 
                           standard_error = boot_col5$bootstrap_ci$SE)


# -----------------------Panel B---------------------------------#
EST <- POIRIER
# Define variable sets for each column


# ---------------------------
# Column 6
# ---------------------------
X5_6 <- c(R, p5r, rainS5, C, C2, S)
X8_6 <- c(R, p8r, rainS8, C, C2, S)
X_6  <- c(R, C, C2, S, pDr, rainD)
BETA_6 <- c(pDr, rainD)

cat("\n--> Running BOOTtse() for Column 6\n")
boot_col6 <- PBOOTtse(data, EST = POIRIER,
                     X8 = X8_6, Z8 = Z8, X5 = X5_6, Z5 = Z5, 
                     X = X_6, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var="district", BETA = BETA_6, 
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col6.csv"))

results[[6]] <- boot_col6$bootstrap_results %>%
  filter(variable %in% BETA_6) %>%
  group_by(variable) %>%
  summarise(
    coefficient = mean(beta, na.rm = TRUE),
    standard_error = mean(se, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()  # Convert to data frame

# ---------------------------
# Column 7
# ---------------------------
X5_7 <- c(R, p5r, rainS5L, C, C2, S)
X8_7 <- c(R, p8r, rainS8L, C, C2, S)
X_7  <- c(R, C, C2, S, pDr, rainDL)
BETA_7 <- c(pDr, rainDL)

cat("\n--> Running BOOTtse() for Column 7\n")
boot_col7 <- PBOOTtse(data, EST, 
                     X8 = X8_7, Z8 = Z8, X5 = X5_7, Z5 = Z5, 
                     X = X_7, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_7, 
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col7.csv"))


results[[7]] <- boot_col7$bootstrap_results %>%
  filter(variable %in% BETA_7) %>%
  group_by(variable) %>%
  summarise(
    coefficient = mean(beta, na.rm = TRUE),
    standard_error = mean(se, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()

# ---------------------------
# Column 8
# ---------------------------
X5_8 <- c(R, p5rL, rainS5L, C, C2, S)
X8_8 <- c(R, p8rL, rainS8L, C, C2, S)
X_8  <- c(R, C, C2, S, pDrL, rainDL)
BETA_8 <- c(pDrL, rainDL)

cat("\n--> Running BOOTtse() for Column 8\n")
boot_col8 <- PBOOTtse(data, EST, 
                     X8 = X8_8, Z8 = Z8, X5 = X5_8, Z5 = Z5, 
                     X = X_8, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_8, 
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col8.csv"))




results[[8]] <- boot_col8$bootstrap_results %>%
  filter(variable %in% BETA_8) %>%
  group_by(variable) %>%
  summarise(
    coefficient = mean(beta, na.rm = TRUE),
    standard_error = mean(se, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()


# ---------------------------
# Column 9
# ---------------------------
X5_9 <- c(R, C, C2, S, p5rF, rainS5F)
X8_9 <- c(R, C, C2, S, p8rF, rainS8F)
X_9  <- c(R, C, C2, S, pDrF, rainDF)
BETA_9 <- c(rainDF, pDrF)

cat("\n--> Running BOOTtse() for Column 9\n")
boot_col9 <- PBOOTtse(data, EST, 
                     X8 = X8_9, Z8 = Z8, X5 = X5_9, Z5 = Z5, 
                     X = X_9, M = M, 
                     FE_vars = FE_vars, J_vars = J_vars, 
                     cluster_var = "district", BETA = BETA_9, 
                     REPS = 1000, output_file = file.path(output, "BOOT_DATA_col9.csv"))




results[[9]] <- boot_col9$bootstrap_results %>%
  filter(variable %in% BETA_9) %>%
  group_by(variable) %>%
  summarise(
    coefficient = mean(beta, na.rm = TRUE),
    standard_error = mean(se, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()



#################readjusting result format##################################
# Function to extract variable labels from 'data'
get_var_labels <- function(data) {
  labels <- setNames(sapply(names(data), function(x) attr(data[[x]], "label")), names(data))
  labels[!is.na(labels)]  # Remove NA labels
}

# Extract variable labels from 'data'
var_labels <- get_var_labels(data)

# Function to update variable names to their labels in results
update_results_with_labels <- function(results, var_labels) {
  lapply(results, function(df) {
    if (!is.null(df) && "variable" %in% names(df)) {
      df <- df %>%
        mutate(variable = ifelse(variable %in% names(var_labels), var_labels[variable], variable))
    }
    return(as.data.frame(df))  # Ensure it's a data frame
  })
}

# Apply the function to update all results with variable labels
results <- update_results_with_labels(results, var_labels)

#####################formatting col6 to 9################################

# Function to extract variable labels from 'data'
get_var_labels <- function(data) {
  labels <- setNames(sapply(names(data), function(x) attr(data[[x]], "label")), names(data))
  labels[!is.na(labels)]  # Remove NA labels
}

# Extract variable labels from 'data'
var_labels <- get_var_labels(data)

# Function to format and store bootstrapped results
store_boot_results <- function(boot_results, beta_vars, var_labels) {
  boot_results %>%
    filter(variable %in% beta_vars) %>%
    group_by(variable) %>%
    summarise(
      boot_coefficient = mean(beta, na.rm = TRUE),
      standard_error = mean(se, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      variable = ifelse(variable %in% names(var_labels), var_labels[variable], variable),  # Replace with labels
      variable = as.character(variable)  # Ensure it's stored as character
    ) %>%
    as.data.frame()  # Ensure it's a data frame
}


# Function to extract variable labels from 'data'
get_var_labels <- function(data) {
  labels <- setNames(sapply(names(data), function(x) attr(data[[x]], "label")), names(data))
  labels[!is.na(labels)]  # Remove NA labels
}

# Extract variable labels from 'data'
var_labels <- get_var_labels(data)

# Function to update variable names to their labels in results & ensure character type
update_results_with_labels <- function(results, var_labels) {
  lapply(results, function(df) {
    if (!is.null(df) && "variable" %in% names(df)) {
      df <- df %>%
        mutate(
          variable = ifelse(variable %in% names(var_labels), var_labels[variable], variable),
          variable = as.character(variable)  # Ensure character type
        )
    }
    return(as.data.frame(df))  # Ensure it's a data frame
  })
}

# Apply the function to update all results
results <- update_results_with_labels(results, var_labels)



#####################################
#####################################
#----------------------
# Panel A 
#----------------------
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Set output file path
output_panelA_path <- file.path(output, "Panel_A.xlsx")

# Ensure all variables from columns 1-5 are included
all_vars <- unique(unlist(lapply(results[1:5], function(df) df$variable)))

# Create an empty data frame with variable names as the first column
panelA_table <- data.frame(variable = all_vars, stringsAsFactors = FALSE)

# Function to extract and format coefficient and SE from a results column
format_results <- function(df, col_num) {
  df <- df %>%
    mutate(
      coefficient = sprintf("%.3f", coefficient),  # Round to 3 decimal places
      standard_error = sprintf("(%.3f)", standard_error)  # Format SE in brackets
    ) %>%
    select(variable, coefficient, standard_error)  # Keep only relevant columns
  
  names(df) <- c("variable", paste0("Col", col_num, "_coef"), paste0("Col", col_num, "_se"))
  return(df)
}

# Merge results from columns 1 to 5
for (i in 1:5) {
  formatted_results <- format_results(results[[i]], i)
  panelA_table <- full_join(panelA_table, formatted_results, by = "variable")
}

# Ensure missing values are replaced with blanks
panelA_table <- panelA_table %>%
  mutate(across(starts_with("Col"), ~ replace_na(.x, "")))

# Reshape so each variable appears twice: first for coefficient, then for SE
panelA_table_long <- panelA_table %>%
  pivot_longer(cols = starts_with("Col"), names_to = "column", values_to = "value") %>%
  separate(column, into = c("column", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = "column", values_from = "value") %>%
  arrange(variable, match(type, c("coef", "se"))) %>%
  select(-type)  # Remove the "coef" and "se" label column

# Duplicate variable names for proper alignment of estimates and SE
panelA_table_long <- panelA_table_long %>%
  mutate(variable = ifelse(row_number() %% 2 == 0, "", variable))  # Remove label for SE rows

# Rename first column to "Panel A. Semiparametric correction procedure"
colnames(panelA_table_long)[1] <- "Panel A. Semiparametric correction procedure"

# Rename regression column headers to (1), (2), (3), (4), (5)
colnames(panelA_table_long)[-1] <- paste0("(", 1:5, ")")

# Define the new row order based on your request
new_order <- c(7,8, 1,2,9,10,3,4,11,12,5,6)

# Apply the new order to Panel A and Panel B
panelA_table_long <- panelA_table_long %>%
  slice(new_order)



######################################
#----------Panel B-------------------
#####################################
#----------------------
# Panel B 
#----------------------
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(xtable)

# Set output file paths
output_panelB_xlsx <- file.path(output, "Panel_B.xlsx")
output_panelB_tex <- file.path(output, "Panel_B.tex")

# Ensure all variables from columns 6-9 are included
all_vars <- unique(unlist(lapply(results[6:9], function(df) df$variable)))

# Create an empty data frame with variable names as the first column
panelB_table <- data.frame(variable = all_vars, stringsAsFactors = FALSE)

# Function to extract and format coefficient and SE from a results column
format_results <- function(df, col_num) {
  df <- df %>%
    mutate(
      coefficient = sprintf("%.3f", coefficient),  # Round to 3 decimal places
      standard_error = sprintf("(%.3f)", standard_error)  # Format SE in brackets
    ) %>%
    select(variable, coefficient, standard_error)  # Keep only relevant columns
  
  names(df) <- c("variable", paste0("Col", col_num, "_coef"), paste0("Col", col_num, "_se"))
  return(df)
}

# Merge results from columns 6 to 9
for (i in 6:9) {
  formatted_results <- format_results(results[[i]], i)
  panelB_table <- full_join(panelB_table, formatted_results, by = "variable")
}

# Ensure missing values are replaced with blanks
panelB_table <- panelB_table %>%
  mutate(across(starts_with("Col"), ~ replace_na(.x, "")))

# Insert an **empty column** after the variable names
panelB_table <- panelB_table %>%
  mutate(empty_col = "")

# Reshape so each variable appears twice: first for coefficient, then for SE
panelB_table_long <- panelB_table %>%
  pivot_longer(cols = starts_with("Col"), names_to = "column", values_to = "value") %>%
  separate(column, into = c("column", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = "column", values_from = "value") %>%
  arrange(variable, match(type, c("coef", "se"))) %>%
  select(-type)  

# Duplicate variable names for proper alignment of estimates and SE
panelB_table_long <- panelB_table_long %>%
  mutate(variable = ifelse(row_number() %% 2 == 0, "", variable))  

# Rename first column to "Panel B. Parametric correction procedure"
colnames(panelB_table_long)[1] <- "Panel B. Parametric correction procedure"

# Rename second column to an **empty title** (for spacing)
colnames(panelB_table_long)[2] <- "-"

# Rename regression column headers as (6), (7), (8), (9)
colnames(panelB_table_long)[3:6] <- paste0("(", 6:9, ")")

# Define the new row order based on your request
new_order <- c(7,8,1,2,9,10,3,4,11,12,5,6)

# Apply the new order to Panel A and Panel B
panelB_table_long <- panelB_table_long %>%
  slice(new_order)


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

write.xlsx(panelB_table_long, file = output_panelB_xlsx, overwrite = TRUE)

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
