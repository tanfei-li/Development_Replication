# Create an empty data frame for Panel A
panel_A_vars <- unique(results[[5]]$variable)
panel_A_table <- data.frame(variable = panel_A_vars)

# Merge results from Columns 1-5
for (col in 1:5) {
  if (!is.null(results[[col]])) {
    panel_A_table <- left_join(panel_A_table, 
                               results[[col]] %>% select(variable, boot_coefficient, standard_error) %>%
                                 rename(!!paste0("coef_", col) := boot_coefficient,
                                        !!paste0("se_", col) := standard_error), 
                               by = "variable")
  }
}


# Create an empty data frame for Panel B
panel_B_vars <- unique(results[[9]]$variable)
panel_B_table <- data.frame(variable = panel_B_vars)

# Merge results from Columns 6-9
for (col in 6:9) {
  if (!is.null(results[[col]])) {
    panel_B_table <- left_join(panel_B_table, 
                               results[[col]] %>% select(variable, boot_coefficient, standard_error) %>%
                                 rename(!!paste0("coef_", col) := boot_coefficient,
                                        !!paste0("se_", col) := standard_error), 
                               by = "variable")
  }
}



# Function to format estimates with standard errors below
format_estimates <- function(coef, se) {
  ifelse(!is.na(coef), paste0(formatC(coef, format="f", digits=3), "\n(", formatC(se, format="f", digits=3), ")"), "")
}

# Apply formatting to Panel A
panel_A_table <- panel_A_table %>%
  mutate(across(starts_with("coef_"), as.character)) %>%
  mutate(across(starts_with("se_"), as.character)) %>%
  rowwise() %>%
  mutate(across(starts_with("coef_"), ~ format_estimates(.x, get(paste0("se_", substr(cur_column(), 6, 6))))))

# Drop separate SE columns
panel_A_table <- panel_A_table %>% select(-starts_with("se_"))

# Apply formatting to Panel B
panel_B_table <- panel_B_table %>%
  mutate(across(starts_with("coef_"), as.character)) %>%
  mutate(across(starts_with("se_"), as.character)) %>%
  rowwise() %>%
  mutate(across(starts_with("coef_"), ~ format_estimates(.x, get(paste0("se_", substr(cur_column(), 6, 6))))))

# Drop separate SE columns
panel_B_table <- panel_B_table %>% select(-starts_with("se_"))


# Save Panel A
write.csv(panel_A_table, "Panel_A.csv", row.names = FALSE)
MKTAB("Panel_A.csv")

# Save Panel B
write.csv(panel_B_table, "Panel_B.csv", row.names = FALSE)
MKTAB("Panel_B.csv")


library(kableExtra)  # For LaTeX table formatting
library(readr)       # For reading CSV files
library(dplyr)       # For data manipulation


convert_to_latex <- function(csv_file, output_tex) {
  # Read the CSV file
  results_table <- read_csv(csv_file, show_col_types = FALSE)
  
  # Convert to LaTeX format
  latex_table <- results_table %>%
    kable(format = "latex", booktabs = TRUE, align = "c", escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
  
  # Save to .tex file
  writeLines(latex_table, output_tex)
  
  cat("LaTeX table saved to:", output_tex, "\n")
}


# Convert Panel A
convert_to_latex("Panel_A.csv", "Panel_A.tex")

# Convert Panel B
convert_to_latex("Panel_B.csv", "Panel_B.tex")

