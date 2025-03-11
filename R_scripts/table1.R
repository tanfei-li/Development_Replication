library(dplyr)
library(openxlsx)
library(haven)  

# Load data #still has problems 
data <- read_dta(file.path(rep_files, "table1dta.dta"))

for (v in c(5, 8)) {
  data <- data %>%
    mutate(
      !!paste0("total_tki_", v, "g0") := ifelse(get(paste0("total_tki_", v)) == 0, NA, get(paste0("total_tki_", v))),
      !!paste0("emig_shr_", v, "g0") := ifelse(get(paste0("emig_shr_", v)) == 0, NA, get(paste0("emig_shr_", v)))
    )
  
  selected_columns <- c(
    paste0("pop_", v), paste0("total_tki_", v), paste0("emig_shr_", v), paste0("any_tki_", v),
    paste0("total_tki_", v, "g0"), paste0("emig_shr_", v, "g0")
  )
  
  summary_stats <- data %>%
    select(all_of(selected_columns)) %>%
    summarise(across(everything(), list(mean = mean, median = median, sd = sd, max = max), na.rm = TRUE))
  
  write.xlsx(summary_stats, file.path(output_dir, paste0("table1_200", v, ".xlsx")))
}

# Create new variables
data <- data %>%
  mutate(
    d_emig_shrg0 = ifelse(emig_shr_8 == 0 | emig_shr_5 == 0, NA, d_emig_shr),
    d_tkig0 = ifelse(any_tki_5 == 0 | any_tki_8 == 0, NA, d_tki)
  )

summary_stats_delta <- data %>%
  select(starts_with("d_")) %>%
  summarise(across(everything(), list(mean = mean, median = median, sd = sd, max = max), na.rm = TRUE))

write.xlsx(summary_stats_delta, file.path(output_dir, "table1_Delta.xlsx"))

# Collapse data
data_collapsed <- data %>%
  group_by(urban) %>%
  summarise(across(starts_with("pop_"), sum, na.rm = TRUE),
            across(starts_with("total_tki_"), sum, na.rm = TRUE))

# Add extra rows
urban_total <- colSums(data_collapsed[1:2, -1])
ur_ratio <- urban_total / sum(urban_total)

data_extended <- data_collapsed %>%
  bind_rows(data.frame(urban = "Total", urban_total)) %>%
  bind_rows(data.frame(urban = "Ratio", ur_ratio))

# Create var5 and var8 columns
data_extended$var5 <- c(data_extended$pop_5[4], data_extended$total_tki_5[4], data_extended$total_tki_5[3])
data_extended$var8 <- c(data_extended$pop_8[4], data_extended$total_tki_8[4], data_extended$total_tki_8[3])

data_final <- data_extended %>%
  slice(1:3) %>%
  select(var5, var8)

write.xlsx(data_final, file.path(output_dir, "table1_national.xlsx"))


