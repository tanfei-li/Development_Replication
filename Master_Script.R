rm(list = ls())
Sys.setenv(LANGUAGE="en")
Sys.setlocale("LC_ALL", "en_US.UTF-8")  # Adjust to your system's locale

# List of required packages
required_packages <- c("sampleSelection", "mvtnorm", "MASS", "systemfit", "sandwich", 
                       "lmtest", "boot", "dplyr", "tidyr", "stringr", "readr", 
                       "openxlsx", "maxLik", "miscTools", "tidyverse", "haven", "fixest", "brglm2", 
                       "flextable", "lfe", "clubSandwich","pbivnorm","glmnet")

# Identify missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

# Install missing packages
if (length(new_packages)) install.packages(new_packages)

# Load necessary libraries
library(sampleSelection)  # For bivariate probit
library(mvtnorm)          # For bivariate normal functions
library(MASS)             # For normal density function
library(systemfit)        # For Seemingly Unrelated Regression (SUR)
library(sandwich)         # For robust and clustered standard errors
library(lmtest)           # For statistical testing with robust SEs
library(boot)             # For bootstrapping
library(dplyr)            # For data manipulation
library(tidyr)            # For reshaping data
library(stringr)          # For string operations
library(readr)            # For reading/writing CSV files
library(openxlsx)         # For exporting to Excel
library(maxLik)           # Required for `sampleSelection`
library(miscTools)        # Required for `sampleSelection`
library(tidyverse)        # Collection of useful data science packages
library(haven)            # Read Stata, SPSS, and SAS files
library(fixest)           # Fixed effects regressions (similar to `feols` in Stata) - haven't explored it yet
library(brglm2)           # For the two-step appraoch
library(flextable)
library(lfe)
library(clubSandwich)
library(pbivnorm)
library(glmnet)
library(kableExtra)



# Set working directories //change according to your organisation structure
userdir <- "C:/Users/121685/Desktop/Development_Replication/AEJApplied_20150548_replication"
rep_files <- file.path(userdir, "dta")
rep_code <- file.path(userdir, "R_scripts")
output <- file.path(userdir, "rep_output")

# Load estimation functions
source(file.path(rep_code, "estimation-programs.R")) 


# Set output directory for logs
output_dir <- "output/logs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Global settings
set.seed(10101)  # Bootstrap seed
REPS <- 1000  # Number of bootstrap repetitions

# DNV specifications
kpowD <- 3  # Polynomial order on selection terms
EST1 <- "SULPM"  # First-stage estimator

# Two-step variables
R <- c("Rlambda", "shareHH_aboveRminRany", "paddy_farmland_shr_5")
C <- c("ln_dist_keccap", "ln_ddist_kabcap", "ln_ddist_emigctr", "nonlandmotor_travel", "urban")
desaC <- c("ln_dist_emigctr_desa", "nonlandmotor_travel", "urban")
S <- c("postprim")
C2 <- c("arabShr", "chineseShr", "muslim_shr")

# Rainfall shock variables
rainS5 <- c("rain_cumdev345")
rainS8 <- c("rain_cumdev678")
rainD <- c("rain_cumdev_diff")

rainS5L <- c("rain_cumdev345", "rain5lambda")
rainS8L <- c("rain_cumdev678", "rain8lambda")
rainDL <- c("rain_cumdev_diff", "rainDlambda")

rainS5F <- c("rain_cumdev345", "rain5lambda", "shareHHaboveXrain5")
rainS8F <- c("rain_cumdev678", "rain8lambda", "shareHHaboveXrain8")
rainDF <- c("rain_cumdev_diff", "rainDlambda", "shareHHaboveXrainD")

# Price shock variables
p5r <- c("P_rice_y2m1_y5m3")
p8r <- c("P_rice_y5m4_y8m3")
pDr <- c("delta_RP_diff")

p5rL <- c("P_rice_y2m1_y5m3", "P445lambda")
p8rL <- c("P_rice_y5m4_y8m3", "P448lambda")
pDrL <- c("delta_RP_diff", "P44Dlambda")

p5rF <- c("P_rice_y2m1_y5m3", "P445lambda", "shareHHaboveXpr5")
p8rF <- c("P_rice_y5m4_y8m3", "P448lambda", "shareHHaboveXpr8")
pDrF <- c("delta_RP_diff", "P44Dlambda", "shareHHaboveXprD")

# Dependent variable
M <- c("d_ln_emig_shr")

# Instruments
Z5 <- c("lnVinSD", "lnAinSD", "lnN5inSD", "lnpop5")
Z8 <- c("lnVinSD", "lnAinSD", "lnN8inSD", "lnpop8")

# Covariates for the selection equations
X5 <- c(R, p5r, rainS5, C, C2, S)
X8 <- c(R, p8r, rainS8, C, C2, S)
X <- c(R, C, C2, S, pDr, rainD)

# Clustering and Fixed Effects
J_vars <- "district"  # Fixed effects at the district level
FE_vars <- "prop"  # Fixed effects (converted from `i.prop`)
cluster_var <- "district"  # Clustering variable

##############################

source(file.path(rep_code, "table4.R")) 
source(file.path(rep_code, "table5.R")) 
source(file.path(rep_code, "table6.R")) 