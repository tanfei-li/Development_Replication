
#################using DNV_6 and BOOTtse_6 which uses data as tibble to avoid error##############
# Load dataset
data <- read_dta(file.path(rep_files, "mainvillageregs.dta")) %>%
  filter(`_samplePlant` == 1)  # Keep villages with area planted (IFEXT)


data <- data %>%
  mutate(
    # Interaction terms for pptkis
    P345Xpptkis = pptkis * P_rice_y2m1_y5m3,
    P678Xpptkis = pptkis * P_rice_y5m4_y8m3,
    PDXpptkis   = pptkis * delta_RP_diff,
    
    rain345Xpptkis = pptkis * rain_cumdev345,
    rain678Xpptkis = pptkis * rain_cumdev678,
    rainDXpptkis   = pptkis * rain_cumdev_diff
  )


for (j in 0:1) {
  data <- data %>%
    mutate(
      # Binary indicator for pptkis
      !!paste0("pptkis_q", j) := as.integer(pptkis == j),
      
      # Interactions
      !!paste0("rain345Xpptkis_q", j) := rain_cumdev345 * as.integer(pptkis == j),
      !!paste0("rain678Xpptkis_q", j) := rain_cumdev678 * as.integer(pptkis == j),
      !!paste0("rainDXpptkis_q", j)   := rain_cumdev_diff * as.integer(pptkis == j),
      !!paste0("P345Xpptkis_q", j)    := P_rice_y2m1_y5m3 * as.integer(pptkis == j),
      !!paste0("P678Xpptkis_q", j)    := P_rice_y5m4_y8m3 * as.integer(pptkis == j),
      !!paste0("PDXpptkis_q", j)      := delta_RP_diff * as.integer(pptkis == j)
    )
}


for (j in 1:4) {
  data <- data %>%
    mutate(
      # Binary indicator for gdpQt
      !!paste0("gdpQt_q", j) := as.integer(gdpQt == j),
      
      # Interactions
      !!paste0("rain345XgdpQt_q", j) := rain_cumdev345 * as.integer(gdpQt == j),
      !!paste0("rain678XgdpQt_q", j) := rain_cumdev678 * as.integer(gdpQt == j),
      !!paste0("rainDXgdpQt_q", j)   := rain_cumdev_diff * as.integer(gdpQt == j),
      !!paste0("P345XgdpQt_q", j)    := P_rice_y2m1_y5m3 * as.integer(gdpQt == j),
      !!paste0("P678XgdpQt_q", j)    := P_rice_y5m4_y8m3 * as.integer(gdpQt == j),
      !!paste0("PDXgdpQt_q", j)      := delta_RP_diff * as.integer(gdpQt == j)
    )
}


# Instrument sets
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")



# Define covariate sets (X5, X8, X) for Column 1

X5_1 <- c("pptkis", R, C, C2, S, "P345Xpptkis_q0", "P345Xpptkis_q1", "rain345Xpptkis_q0","rain345Xpptkis_q1")
X8_1 <- c("pptkis", R, C, C2, S, "P678Xpptkis_q0","P678Xpptkis_q1", "rain678Xpptkis_q0", "rain678Xpptkis_q1")
X_1  <- c("pptkis", R, C, C2, S, "PDXpptkis_q0","PDXpptkis_q1", "rainDXpptkis_q0","rainDXpptkis_q1")

# Define parameters to extract from regression
BETA_1 <- c("rainDXpptkis_q1" , "rainDXpptkis_q0" , "PDXpptkis_q1", "PDXpptkis_q0")


boot_1 <- BOOTtse(data, 
                  EST = DNV, Mt5 = "Mt5", Mt8 = "Mt8",
                  X8 = X8_1, Z8 = Z8, 
                  X5 = X5_1, Z5 = Z5, 
                  X = X_1, 
                  kpowD = 3, 
                  M = M, 
                  FE_vars = FE_vars, 
                  J_vars = J_vars, 
                  BETA = BETA_1, 
                  REPS = 500, 
                  output_file = "BOOT_DATA_col1.csv")


# Initialize results list
results <- list()





###################################################################

# Define X5, X8, and X, omitting gdpQt_q1 (baseline)
X5_2 <- c("gdpQt_q2", "gdpQt_q3", "gdpQt_q4", R, C, C2, S,
          "P345XgdpQt_q1", "P345XgdpQt_q2", "P345XgdpQt_q3", "P345XgdpQt_q4", 
          "rain345XgdpQt_q1","rain345XgdpQt_q2", "rain345XgdpQt_q3", "rain345XgdpQt_q4")

X8_2 <- c("gdpQt_q2", "gdpQt_q3", "gdpQt_q4", R, C, C2, S,
          "P678XgdpQt_q1","P678XgdpQt_q2", "P678XgdpQt_q3", "P678XgdpQt_q4", 
          "rain678XgdpQt_q1","rain678XgdpQt_q2", "rain678XgdpQt_q3", "rain678XgdpQt_q4")

X_2  <- c("gdpQt_q2", "gdpQt_q3", "gdpQt_q4", R, C, C2, S,
          "PDXgdpQt_q1","PDXgdpQt_q2", "PDXgdpQt_q3", "PDXgdpQt_q4", 
          "rainDXgdpQt_q1", "rainDXgdpQt_q2", "rainDXgdpQt_q3", "rainDXgdpQt_q4")

# Parameters to extract from regression
BETA_2 <- c("PDXgdpQt_q1","PDXgdpQt_q2", "PDXgdpQt_q3", "PDXgdpQt_q4", 
            "rainDXgdpQt_q1", "rainDXgdpQt_q2", "rainDXgdpQt_q3", "rainDXgdpQt_q4")



boot_2 <- BOOTtse(data = data,
                    EST = DNV, Mt5 = "Mt5", Mt8 = "Mt8",
                    X8 = X8_2, Z8 = Z8, 
                    X5 = X5_2, Z5 = Z5, 
                    X = X_2, 
                    kpowD = 3, 
                    M = M, 
                    FE_vars = FE_vars, 
                    J_vars = J_vars, 
                    BETA = BETA_2, 
                    REPS = 500, 
                    output_file = "BOOT_DATA_col2.csv")



##########################table formatting###################################
#----------------------
# Load Required Libraries
#----------------------
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(kableExtra)
#------------------------------
# extracting results 
#------------------------------

results[[1]] <- data.frame(
  variable = BETA_1,
  coefficient = round(coef(boot_1$original_model$model)[BETA_1], 3),  # Extract original coefficients
  standard_error = round(summary(boot_1$original_model$model)$coefficients[BETA_1, "Cluster s.e."], 3)  # Extract clustered SEs
)


results[[2]] <- data.frame(
  variable = BETA_2,
  coefficient = round(coef(boot_2$original_model$model)[BETA_2], 3),  # Extract original coefficients
  standard_error = round(summary(boot_2$original_model$model)$coefficients[BETA_2, "Cluster s.e."], 3)  # Extract clustered SEs
)


# Set output file paths
output_combined_xlsx <- file.path(output, "table6.xlsx")
output_combined_tex <- file.path(output, "table6.tex")

# Ensure variable names are consistently formatted in both results
results[[1]] <- results[[1]] %>%
  rename_with(~ str_trim(.))  # Remove extra spaces in column names

results[[2]] <- results[[2]] %>%
  rename_with(~ str_trim(.)) %>%
  mutate(variable = case_when(
    variable == "delta_RP_diff" ~ "delta_RP_diff_2",
    variable == "rain_cumdev_diff" ~ "rain_cumdev_diff_2",
    TRUE ~ variable
  ))

# Create a named vector for variable name mapping to LaTeX
var_label_map <- c(
  "rainDXpptkis_q1" = "$\\Delta$ rainfall shock $\\times$ recruiter presence",
  "rainDXpptkis_q0" = "$\\Delta$ rainfall shock $\\times$ no recruiter presence",
  "PDXpptkis_q1" = "$\\Delta$ price shock $\\times$ recruiter presence",
  "PDXpptkis_q0" = "$\\Delta$ price shock $\\times$ no recruiter presence",
  
  "PDXgdpQt_q1" = "$\\Delta$ price shock $\\times$ agricultural GDP, quartile = 1",
  "PDXgdpQt_q2" = "$\\Delta$ price shock $\\times$ agricultural GDP, quartile = 2",
  "PDXgdpQt_q3" = "$\\Delta$ price shock $\\times$ agricultural GDP, quartile = 3",
  "PDXgdpQt_q4" = "$\\Delta$ price shock $\\times$ agricultural GDP, quartile = 4",
  
  "rainDXgdpQt_q1" = "$\\Delta$ rainfall shock $\\times$ agricultural GDP, quartile = 1",
  "rainDXgdpQt_q2" = "$\\Delta$ rainfall shock $\\times$ agricultural GDP, quartile = 2",
  "rainDXgdpQt_q3" = "$\\Delta$ rainfall shock $\\times$ agricultural GDP, quartile = 3",
  "rainDXgdpQt_q4" = "$\\Delta$ rainfall shock $\\times$ agricultural GDP, quartile = 4"
)

#----------------------
# Convert coefficients and standard errors to formatted characters BEFORE pivoting
#----------------------
results[[1]] <- results[[1]] %>%
  mutate(
    coefficient = sprintf("%.3f", coefficient),
    standard_error = sprintf("(%.3f)", standard_error)  # Brackets for SE
  )

results[[2]] <- results[[2]] %>%
  mutate(
    coefficient = sprintf("%.3f", coefficient),
    standard_error = sprintf("(%.3f)", standard_error)  # Brackets for SE
  )

#----------------------
# Harmonize formats before merging
#----------------------
results_formatted <- bind_rows(
  results[[1]] %>% mutate(Column = "(1)"),
  results[[2]] %>% mutate(Column = "(2)")
) %>%
  mutate(variable = if_else(variable %in% names(var_label_map), var_label_map[variable], variable)) %>%
  pivot_wider(names_from = Column, values_from = c(coefficient, standard_error), 
              values_fill = list(coefficient = "", standard_error = "")) %>%  # Now all are characters
  arrange(ifelse(variable %in% results[[1]]$variable, 1, 2))  # Keep (1) results at the top


#----------------------
# Ensure SE goes below the coefficient while preserving variable order
#----------------------
results_formatted_final <- results_formatted %>%
  pivot_longer(cols = c(`coefficient_(1)`, `standard_error_(1)`, `coefficient_(2)`, `standard_error_(2)`),
               names_to = "Type", values_to = "Estimate") %>%
  mutate(
    Column = case_when(
      Type %in% c("coefficient_(1)", "standard_error_(1)") ~ "(1)",
      Type %in% c("coefficient_(2)", "standard_error_(2)") ~ "(2)"
    ),
    Type = case_when(
      str_detect(Type, "standard_error") ~ "SE",
      TRUE ~ "Coefficient"
    )
  ) %>%
  arrange(match(variable, results_formatted$variable), Type) %>%  # Preserve order, SE follows coefficient
  pivot_wider(names_from = Column, values_from = Estimate, values_fill = list(Estimate = ""))


#---------------
# village count
#---------------
# Function to count the number of rows used for estimation
count_valid_rows <- function(data, X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var) {
  # Identify all variables required for estimation
  required_vars <- unique(c(X8, Z8, X5, Z5, X, M, FE_vars, J_vars, cluster_var))
  
  # Keep only the variables present in the dataset
  required_vars <- required_vars[required_vars %in% names(data)]
  
  # Subset data and remove rows with NA
  valid_data <- na.omit(data[, required_vars])
  
  # Return the number of rows remaining after removing NAs
  return(nrow(valid_data))
}

# Count the number of rows used
num_col1 <- count_valid_rows(data, X8_1, Z8, X5_1, Z5, X_1, M, FE_vars, J_vars, cluster_var)
num_col2 <- count_valid_rows(data, X8_2, Z8, X5_2, Z5, X_2, M, FE_vars, J_vars, cluster_var)

#----------------------
# Drop "Type" column before appending the "Number of villages" row
#----------------------
final_table <- results_formatted_final %>%
  dplyr::select(-Type)  # Drop Type column first

#----------------------
# Ensure SE rows have an empty row name
#----------------------
final_table <- final_table %>%
  mutate(variable = ifelse(row_number() %% 2 == 0, "", variable))  # Empty every second row (SE rows)

#----------------------
# Ensure LaTeX math expressions remain intact
#----------------------
final_table <- final_table %>%
  mutate(variable = str_replace_all(variable, "\\\\\\\\", "\\\\"))  # Ensure proper LaTeX rendering

# Ensure empty row has the exact same structure as final_table
empty_row <- as.data.frame(matrix("", nrow = 1, ncol = ncol(final_table)))
colnames(empty_row) <- colnames(final_table)

# Create "Number of Villages" row with correct column names
num_villages_row <- data.frame(variable = "\\addlinespace[12pt] \\textbf{Number of villages}", 
                               `(1)` = as.character(num_col1), 
                               `(2)` = as.character(num_col2),
                               stringsAsFactors = FALSE)

# Ensure column names match before merging
colnames(num_villages_row) <- colnames(final_table)

# ✅ Append only once: Add empty row, then "Number of villages"
final_table <- rbind(final_table, empty_row, num_villages_row)

#----------------------
# Format and Export Table (Fix Alignment, Move Column (2) to the Right)
#----------------------
latex_table <- final_table %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "lcr",  # Left-align col (1), center col (1), right-align col (2)
        col.names = c("", "(1)", "(2)"), 
        caption = "Table 6: Evidence on the Opportunity Cost Mechanism (Reproduced)") %>%  # **Add Table Title**
  column_spec(1, width = "7cm") %>%  # Adjust first column width dynamically
  column_spec(2, width = "3cm") %>%  # Adjust second column width (centered)
  column_spec(3, width = "3cm") %>%  # Adjust third column width (right-aligned)
  kable_styling(latex_options = c("hold_position"))  # **Fix unwanted "c" and disable extra formatting**

# Save the properly formatted LaTeX table
writeLines(latex_table, output_combined_tex)

# Save as an Excel file for reference
write.xlsx(final_table, output_combined_xlsx)

# Display output in console (optional)
cat(latex_table)