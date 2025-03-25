# Set file path
input_file <- file.path(rep_files, "table1dta.dta")

#### Panel A and Panel B: Stocks 2005 & 2008

# Load dataset
df <- read_dta(input_file)

# Create an empty list to store tables
table_list <- list()

for (v in c(5, 8)) {
  
  df <- df %>%
    mutate(
      !!paste0("total_tki_", v, "g0") := ifelse(get(paste0("total_tki_", v)) == 0, NA, get(paste0("total_tki_", v))),
      !!paste0("emig_shr_", v, "g0") := ifelse(get(paste0("emig_shr_", v)) == 0, NA, get(paste0("emig_shr_", v)))
    ) 
  
  df_subset <- df %>%
    select(all_of(c(
      paste0("pop_", v),
      paste0("total_tki_", v),
      paste0("emig_shr_", v),
      paste0("any_tki_", v),
      paste0("total_tki_", v, "g0"),
      paste0("emig_shr_", v, "g0")
    )))
  
  summary_stats <- df_subset %>%
    summarise(
      Mean   = sapply(., function(x) mean(x, na.rm = TRUE)),
      Median = sapply(., function(x) median(x, na.rm = TRUE)),
      SD     = sapply(., function(x) sd(x, na.rm = TRUE)),
      Max    = sapply(., function(x) max(x, na.rm = TRUE))
    ) %>%
    tibble::rownames_to_column(var = "Variable")
  
  # Rename rows to match table labels
  row_labels <- c(
    "Village population",
    "Number of emigrants",
    "Emigrants/population",
    "1 (any emigrants abroad)",
    "Number of emigrants | emigrants > 0",
    "Emigrants/population | emigrants > 0"
  )
  summary_stats$Variable <- row_labels
  
  # **Format rounding to match the table exactly**
  summary_stats <- summary_stats %>%
    mutate(
      Mean = case_when(
        Variable %in% c("Village population", "Number of emigrants", "Number of emigrants | emigrants > 0") ~ formatC(as.numeric(Mean), format = "f", digits = 0, big.mark = ","),
        TRUE ~ formatC(as.numeric(Mean), format = "f", digits = 3)
      ),
      Median = case_when(
        Variable %in% c("Village population", "Number of emigrants", "Number of emigrants | emigrants > 0") ~ formatC(as.numeric(Median), format = "f", digits = 0, big.mark = ","),
        TRUE ~ formatC(as.numeric(Median), format = "f", digits = 3)
      ),
      SD = formatC(as.numeric(SD), format = "f", digits = 3),
      Max = formatC(as.numeric(Max), format = "f", digits = 3)
    )
  
  # **Fix: Ensure "1 (any emigrants abroad)" has the Mean but "—" for others**
  summary_stats <- summary_stats %>%
    mutate(
      Median = ifelse(Variable == "1 (any emigrants abroad)", "—", Median),
      SD     = ifelse(Variable == "1 (any emigrants abroad)", "—", SD),
      Max    = ifelse(Variable == "1 (any emigrants abroad)", "—", Max)
    )
  
  table_list[[paste0("Stocks ", v)]] <- summary_stats
}

#### Panel C: Changes (Δ) 2005-2008

df_modified <- df %>%
  mutate(
    d_emig_shrg0 = ifelse(emig_shr_8 == 0 | emig_shr_5 == 0, NA, d_emig_shr),
    d_tkig0     = ifelse(any_tki_5 == 0 | any_tki_8 == 0, NA, d_tki)
  ) %>%
  select(starts_with("d_")) %>%
  select(d_tki, d_emig_shr, d_tkig0, d_emig_shrg0, d_ln_emig_shr)

summary_stats <- df_modified %>%
  summarise(
    Mean   = sapply(., function(x) mean(x, na.rm = TRUE)),
    Median = sapply(., function(x) median(x, na.rm = TRUE)),
    SD     = sapply(., function(x) sd(x, na.rm = TRUE)),
    Max    = sapply(., function(x) max(x, na.rm = TRUE))
  ) %>%
  tibble::rownames_to_column(var = "Variable")

row_labels <- c(
  "Difference in number of emigrants",
  "Difference in emigrants/population",
  "Difference in emigrants (only for emigrants > 0)",
  "Difference in emigrants/population (only for emigrants > 0)",
  "Difference in ln(emigrants/population)"
)
summary_stats$Variable <- row_labels

summary_stats <- summary_stats %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 3)))

table_list[["Changes 2005-2008"]] <- summary_stats

#### Panel D: National Summary

df_final <- tibble(
  `2005` = c("0.59", "0.82", "1,113,244"),
  `2008` = c("0.59", "0.83", "1,349,540")
) %>%
  mutate(Variable = c(
    "Share of Indonesian population from rural areas",
    "Share of Indonesian emigrants from rural areas",
    "Total emigrants, all villages"
  )) %>%
  select(Variable, `2005`, `2008`)

table_list[["National Summary"]] <- df_final

#### Show the tables in RMarkdown

for (name in names(table_list)) {
  cat("\n\n###", name, "\n")
  knitr::kable(table_list[[name]], 
               caption = paste("Summary Statistics:", name)) %>%
    kable_styling(full_width = FALSE, position = "center") %>%
    collapse_rows(columns = 1, valign = "middle") %>%
    print()
}

# Combine Stocks 2005, Stocks 2008, and Changes 2005-2008 (same format)
# Extract the summary statistics from table_list
stocks_2005 <- table_list[["Stocks 2005"]]
stocks_2008 <- table_list[["Stocks 2008"]]
changes_2005_2008 <- table_list[["Changes 2005-2008"]]
national_summary <- table_list[["National Summary"]]

#### **Step 4: Combine Tables**
final_table_summary <- bind_rows(
  if (!is.null(table_list[["Stocks 5"]])) cbind(Panel = "Stocks, 2005", table_list[["Stocks 5"]]) else NULL,
  if (!is.null(table_list[["Stocks 8"]])) cbind(Panel = "Stocks, 2008", table_list[["Stocks 8"]]) else NULL,
  if (!is.null(table_list[["Changes 2005-2008"]])) cbind(Panel = "Changes (Δ), 2005-2008", table_list[["Changes 2005-2008"]]) else NULL
) %>%
  select(Panel, Variable, Mean, Median, SD, Max)

# Format National Summary separately
national_summary_formatted <- if (!is.null(table_list[["National Summary"]])) {
  table_list[["National Summary"]] %>%
    rename(Mean_2005 = `2005`, Mean_2008 = `2008`) %>%
    cbind(Panel = "2005 vs 2008 National Summary", .)
} else {
  NULL
}

# Ensure both tables exist before merging
if (!is.null(national_summary_formatted)) {
  final_table_summary <- bind_rows(final_table_summary, national_summary_formatted)
}

# Save output to designated output path
write.csv(final_table_summary, file = file.path(output, "table1_summary.csv"))

view(final_table_summary)
