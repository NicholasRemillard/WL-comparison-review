library(stringr)
library(tidyverse)

df_clean <- readRDS("data/metrics_to_tally.RDS")


# Define keywords that indicate a lower value is better
error_metrics <- c("RMSE", "MAE", "MAPE", "MSE", "Error")

df_summary <- df_clean %>%
  group_by(First.Author, Year) %>%
  filter({
    # 1. Check for error keywords, strictly ignoring NAs in the text
    # If the text is NA, we assume it is NOT an error metric
    is_error_metric <- any(
      str_detect(Best.Metric, regex(paste(error_metrics, collapse = "|"), ignore_case = TRUE)), 
      na.rm = TRUE
    )
    
    # 2. Filter logic
    if (is_error_metric) {
      # For errors, keep the row with the LOWEST value
      Best.Metric.Val == min(Best.Metric.Val, na.rm = TRUE)
    } else {
      # For accuracy/others (or if Metric name is NA), keep the row with the HIGHEST value
      Best.Metric.Val == max(Best.Metric.Val, na.rm = TRUE)
    }
  }) %>%
  # Optional: Handle ties if two locations have the exact same score
  # slice(1) %>% 
  ungroup()

saveRDS(df_summary, file = "data/best_loc_per_study.RDS")

# For 
write.csv(df_summary, file = "data/best_loc_per_study.csv")

