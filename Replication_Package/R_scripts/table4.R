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


####Clean Data ####
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop", "district")
data_cleaned <- data[complete.cases(data[, required_vars]), ]
n_col1 = nrow(data_cleaned) #save village count for col 1

data_cleaned$ZX5 <- as.matrix(data_cleaned[, c(ZX5)])
data_cleaned$ZX8 <- as.matrix(data_cleaned[, c(ZX8)])

# Convert 'prop' to a factor for FE_vars
data_cleaned$FE_vars <- as.factor(data_cleaned$prop)

# Define the two equations (Mt8 and Mt5 models)
eq1 <- Mt8 ~ ZX8 + FE_vars
eq2 <- Mt5 ~ ZX5 + FE_vars

# Combine the equations into a system
system <- list(eq1 = eq1, eq2 = eq2)


# Define the bootstrap function
sureg_boot_function <- function(data, indices) {
  data_boot <- data[indices, ]
  data_boot$ZX5 <- as.matrix(data_boot[, c(ZX5)])
  data_boot$ZX8 <- as.matrix(data_boot[, c(ZX8)])
  data_boot$FE_vars <- as.factor(data_boot$prop)
  
  eq1 <- Mt8 ~ ZX8 + FE_vars
  eq2 <- Mt5 ~ ZX5 + FE_vars
  system <- list(eq1 = eq1, eq2 = eq2)
  
  sureg_model_boot <- systemfit(system, method = "SUR", data = data_boot)
  return(coef(sureg_model_boot))  # Return coefficients or any other desired statistic
}

# Perform bootstrapping with resampling at the village level within each district
set.seed(10101)  # Ensure reproducibility
boot_run1 <- boot(data_cleaned, sureg_boot_function, R = 500, strata = data_cleaned$district)



######arranging output###############################################
#####################################################################
# Extract coefficients from the bootstrapped results and attach names
boot_coefs_1 <-as.data.frame(boot_run1$t)
colnames(boot_coefs_1) <- names(boot_run1$t0)
  
# Select variables for output
vars_2008_run1 <- c("eq1_ZX8lnplant_sawahMaxP1", "eq1_ZX8lnplant_sawahMinP1", 
                 "eq1_ZX8lnN8inSD", "eq1_ZX8lnAinSD", 
                 "eq1_ZX8ln_dist_keccap", "eq1_ZX8ln_ddist_emigctr", 
                 "eq1_ZX8P_rice_y5m4_y8m3", "eq1_ZX8rain_cumdev678")

# Remove the "eq1_" prefix from all variable names
vars_2008_run1_renamed <- gsub("^eq1_ZX8", "", vars_2008_run1)



vars_2005_run1 <- c("eq2_ZX5lnplant_sawahMaxP1", "eq2_ZX5lnplant_sawahMinP1", 
               "eq2_ZX5lnN5inSD", "eq2_ZX5lnAinSD", 
               "eq2_ZX5ln_dist_keccap", "eq2_ZX5ln_ddist_emigctr", 
               "eq2_ZX5P_rice_y2m1_y5m3", "eq2_ZX5rain_cumdev345")

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


################################column 2#################
################################ Column 2 #################

# Define variable groups
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

ZX5 <- c(C, C2, S, p5r, rainS5, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

# Clean Data 
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop", "district")
data_cleaned <- data[complete.cases(data[, required_vars]), ]
n_col2 = nrow(data_cleaned) #save village count for col 2

data_cleaned$ZX5 <- as.matrix(data_cleaned[, c(ZX5)])
data_cleaned$ZX8 <- as.matrix(data_cleaned[, c(ZX8)])

# Convert 'prop' to a factor for FE_vars
data_cleaned$FE_vars <- as.factor(data_cleaned$prop)

# Define the two equations (Mt8 and Mt5 models)
eq1 <- Mt8 ~ ZX8 + FE_vars
eq2 <- Mt5 ~ ZX5 + FE_vars

# Combine the equations into a system
system <- list(eq1 = eq1, eq2 = eq2)


# Perform bootstrapping with clustering at the district level
set.seed(10101)  # Ensure reproducibility
boot_run2 <- boot(data_cleaned, sureg_boot_function, R = 50, strata = data_cleaned$district)

# View the bootstrap results
print(boot_run2)



# Extract coefficients from the bootstrapped results and attach names
boot_coefs_2 <- as.data.frame(boot_run2$t)
colnames(boot_coefs_2) <- names(boot_run2$t0)

# Select specific variables for output
vars_2008_run2 <- c("eq1_ZX8Rlambda", "eq1_ZX8lnpop8", "eq1_ZX8lnN8inSD", 
                     "eq1_ZX8lnAinSD", "eq1_ZX8ln_dist_keccap", 
                     "eq1_ZX8ln_ddist_emigctr", "eq1_ZX8P_rice_y5m4_y8m3", 
                     "eq1_ZX8rain_cumdev678")

vars_2005_run2 <- c("eq2_ZX5Rlambda", "eq2_ZX5lnpop5", "eq2_ZX5lnN5inSD",
                     "eq2_ZX5lnAinSD", "eq2_ZX5ln_dist_keccap",
                     "eq2_ZX5ln_ddist_emigctr", "eq2_ZX5P_rice_y2m1_y5m3",
                     "eq2_ZX5rain_cumdev345")

# Remove the equation prefix ("eq1_ZX8" and "eq2_ZX5")
vars_2008_run2_clean <- gsub("^eq1_ZX8", "", vars_2008_run2)
vars_2005_run2_clean <- gsub("^eq2_ZX5", "", vars_2005_run2)

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




############################## Column 3 ############################
####################################################################
library(endogeneity)

# Define the variables
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnplant_sawahMinP1", "lnplant_sawahMaxP1")

ZX5 <- c(C, C2, S, p5r, rainS5, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)


####Clean Data ####
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop", "district")
data_cleaned <- data[complete.cases(data[, required_vars]), ]

####setting independent variables####

data_cleaned$ZX5 <- as.matrix(data_cleaned[, c(ZX5)])
data_cleaned$ZX8 <- as.matrix(data_cleaned[, c(ZX8)])

# Convert 'prop' to a factor for FE_vars
data_cleaned$FE_vars <- as.factor(data_cleaned$prop)

# Fit the bivariate probit model
biprobit_model_3 <- biprobit(
  Mt8 ~ ZX8 + FE_vars,  # First equation (for Mt8)
  Mt5 ~ ZX5 + FE_vars,  # Second equation (for Mt5)
  data = data_cleaned           # Data frame
)



#### Step 6: Prepare Results Table ####
vars_2008_run3 <- c("1ZX8lnplant_sawahMaxP1", "1ZX8lnplant_sawahMinP1", "1ZX8lnN8inSD", "1ZX8lnAinSD", 
                    "1ZX8ln_dist_keccap", "1ZX8ln_ddist_emigctr",  "1ZX8P_rice_y5m4_y8m3", "1ZX8rain_cumdev678")


vars_2005_run3 <- c("ZX5lnplant_sawahMaxP1", "ZX5lnplant_sawahMinP1", 
                    "ZX5lnN5inSD", "ZX5lnAinSD", 
                    "ZX5ln_dist_keccap", "ZX5ln_ddist_emigctr", 
                    "ZX5P_rice_y2m1_y5m3", "ZX5rain_cumdev345")

# Directly extract the variance-covariance matrix from the model object
vcov_biprobit <- biprobit_model_3$var

# Check the structure of the variance-covariance matrix
print(vcov_biprobit)

# Now extract the coefficients and standard errors
vars_2008_run3_match <- match(vars_2008_run3, names(coef(biprobit_model_3)))
vars_2005_run3_match <- match(vars_2005_run3, names(coef(biprobit_model_3)))

# Extract coefficients
est_mt8_run3 <- coef(biprobit_model_3)[vars_2008_run3_match]
est_mt5_run3 <- coef(biprobit_model_3)[vars_2005_run3_match]

# Compute the standard errors using the square root of the diagonal of the variance-covariance matrix
se_mt8_run3 <- sqrt(diag(vcov_biprobit)[vars_2008_run3_match])
se_mt5_run3 <- sqrt(diag(vcov_biprobit)[vars_2005_run3_match])

var_name_repeated <- rep(var_labels_2008_run1, each = 2)
# Construct the final results table
column_3 <- data.frame(
  var_name = var_name_repeated,  
  `2008` = c(rbind(sprintf("%.3f", est_mt8_run3), sprintf("(%.3f)", se_mt8_run3))),
  `2005` = c(rbind(sprintf("%.3f", est_mt5_run3), sprintf("(%.3f)", se_mt5_run3)))
)

# Print the final table
print(column_3)


#########################run 4#####################
####################################################
#########################run 4#####################
###################################################

Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

ZX5 <- c(C, C2, S, p5r, rainS5, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

####Clean Data ####
required_vars <- c(ZX5, ZX8, "Mt5", "Mt8", "prop", "district")
data_cleaned <- data[complete.cases(data[, required_vars]), ]

####setting independent variables####

data_cleaned$ZX5 <- as.matrix(data_cleaned[, c(ZX5)])
data_cleaned$ZX8 <- as.matrix(data_cleaned[, c(ZX8)])

# Convert 'prop' to a factor for FE_vars
data_cleaned$FE_vars <- as.factor(data_cleaned$prop)

# Fit the bivariate probit model
biprobit_model_4 <- biprobit(
  Mt8 ~ ZX8 + FE_vars,  # First equation (for Mt8)
  Mt5 ~ ZX5 + FE_vars,  # Second equation (for Mt5)
  data = data_cleaned           # Data frame
)

#### Step 6: Prepare Results Table ####


vars_2008_run4 <- c("1ZX8Rlambda", "1ZX8lnpop8", "1ZX8lnN8inSD", 
                    "1ZX8lnAinSD", "1ZX8ln_dist_keccap", 
                    "1ZX8ln_ddist_emigctr", "1ZX8P_rice_y5m4_y8m3", 
                    "1ZX8rain_cumdev678")

vars_2005_run4 <- c("ZX5Rlambda", "ZX5lnpop5", "ZX5lnN5inSD",
                    "ZX5lnAinSD", "ZX5ln_dist_keccap",
                    "ZX5ln_ddist_emigctr", "ZX5P_rice_y2m1_y5m3",
                    "ZX5rain_cumdev345")

# Directly extract the variance-covariance matrix from the model object
vcov_biprobit <- biprobit_model_4$var

# Check the structure of the variance-covariance matrix
print(vcov_biprobit)

# Now extract the coefficients and standard errors
vars_2008_run4_match <- match(vars_2008_run4, names(coef(biprobit_model_4)))
vars_2005_run4_match <- match(vars_2005_run4, names(coef(biprobit_model_4)))

# Extract coefficients
est_mt8_run4 <- coef(biprobit_model_4)[vars_2008_run4_match]
est_mt5_run4 <- coef(biprobit_model_4)[vars_2005_run4_match]

# Compute the standard errors using the square root of the diagonal of the variance-covariance matrix
se_mt8_run4 <- sqrt(diag(vcov_biprobit)[vars_2008_run4_match])
se_mt5_run4 <- sqrt(diag(vcov_biprobit)[vars_2005_run4_match])

var_name_repeated <- rep(var_labels_2008_run2, each = 2)
# Construct the final results table
column_4 <- data.frame(
  var_name = var_name_repeated,  
  `2008` = c(rbind(sprintf("%.3f", est_mt8_run4), sprintf("(%.3f)", se_mt8_run4))),
  `2005` = c(rbind(sprintf("%.3f", est_mt5_run4), sprintf("(%.3f)", se_mt5_run4)))
)

# Print the final table
print(column_4)



#################################################################

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

library(dplyr)
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

library(kableExtra)
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