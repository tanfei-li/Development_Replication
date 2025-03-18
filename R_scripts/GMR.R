# Load required packages
library(sampleSelection)   # For mvProbit
library(dplyr)             # Data Handling
library(sandwich)          # Clustered SEs
library(lmtest)            # Inference
library(GJRM)

# Load the dataset

data <- read_dta(file.path(rep_files, "mainvillageregs.dta"))

# Keep villages with area planted lambda
data <- data %>% filter(`_samplePlant` == 1)



# Define variable groups
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

ZX5 <- c(C, C2, S, p5r, rainS5, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z5)
ZX8 <- c(C, C2, S, p8r, rainS8, "Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5", Z8)

# Step 0: Data Cleaning
# Drop rows with missing values only in these variables

# Define formulas
formula_Mt8 <- as.formula(paste("Mt8 ~", paste(ZX8, collapse = " + "), "+ prop"))
formula_Mt5 <- as.formula(paste("Mt5 ~", paste(ZX5, collapse = " + "), "+ prop"))

bvp_model <- gjrm(
  formula = list(formula_Mt8, formula_Mt5),
  data = data,
  margins = c("probit", "probit"),
  model = "B"  # lowercase "model"
)

# Summary
summary(bvp_model)








# Number of coefficients per equation
k1 <- ncol(bvp_model$X1)
k2 <- ncol(bvp_model$X2)

# Mt8 Coefficients + SE
results_Mt8 <- results_df[1:k1, ]
rownames(results_Mt8) <- colnames(bvp_model$X1)
print("Equation 1 (Mt8):")
print(results_Mt8)

# Mt5 Coefficients + SE
results_Mt5 <- results_df[(k1 + 1):(k1 + k2), ]
rownames(results_Mt5) <- colnames(bvp_model$X2)
print("Equation 2 (Mt5):")
print(results_Mt5)



################################################
install.packages("cmdstanr")
