library(stringr)
library(tidyverse)

df_clean <- readRDS("data/data_to_tally_cleaned.RDS")

# 1. Helper: Determine if a metric name implies "Lower is Better"
is_error_metric <- function(name) {
  n <- tolower(name)
  any(sapply(c("rmse", "mae", "mse", "error", "misclassification"), function(x) grepl(x, n)))
}

# 2. Helper: Clean and Normalize Values (Handles %, strings, and 0-100 scaling)
clean_value <- function(val_str, is_error) {
  if (is.na(val_str) || val_str == "") return(NA)
  
  # Step A: Remove text in parentheses (e.g., "27.4 (14.0)" -> "27.4")
  val_clean <- gsub("\\s*\\(.*?\\)", "", val_str)
  
  # Step B: Remove % signs
  val_clean <- gsub("%", "", val_clean)
  
  # Step C: Convert to numeric
  clean_num <- as.numeric(val_clean)
  
  # Step D: Normalization (0-100 scale vs 0-1 scale)
  # If it's NOT an error metric and value > 1, divide by 100
  if (!is.na(clean_num) && !is_error && clean_num > 1) {
    clean_num <- clean_num / 100
  }
  
  return(clean_num)
}

# 3. Helper: Parse Metadata to get Map of Index -> Name
get_metric_names <- function(meta_string) {
  if (is.na(meta_string) || meta_string == "") return(data.frame())
  
  lines <- unlist(strsplit(meta_string, "\n"))
  metric_lines <- lines[grepl("^Metric", lines, ignore.case = TRUE)]
  
  if (length(metric_lines) == 0) return(data.frame())
  
  # Extract Index and Name
  indices <- as.integer(str_extract(metric_lines, "(?<=Metric )\\d+"))
  names <- str_trim(sub("^Metric \\d+:", "", metric_lines))
  
  return(data.frame(Index = indices, Name = names, stringsAsFactors = FALSE))
}

# --- MAIN LOOP ---
# Initialize new columns
df_clean$Best.Metric <- NA_character_
df_clean$Best.Metric.Val <- NA_real_

df_clean <- df_clean %>%
  relocate(Best.Metric, Best.Metric.Val, .after = Wear.Locations.Included)

for (i in 1:nrow(df_clean)) {
  
  # tryCatch allows us to handle errors without stopping the loop
  tryCatch({
    
    meta_string <- df_clean$Corresponding.Metrics.and.Sites.for.the.following.table[i]
    metric_map <- get_metric_names(meta_string)
    
    if (nrow(metric_map) > 0) {
      
      candidates <- data.frame(
        Metric_Name = character(),
        Value = numeric(),
        Is_Error = logical(),
        stringsAsFactors = FALSE
      )
      
      for (m in 1:8) {
        val_str <- df_clean[i, paste0("Metric.", m)]
        
        if (!is.na(val_str) && m %in% metric_map$Index) {
          m_name <- metric_map$Name[metric_map$Index == m]
          is_err <- is_error_metric(m_name)
          val_num <- clean_value(val_str, is_err)
          
          # Only add if we successfully got a number
          if (!is.na(val_num)) {
            candidates <- rbind(candidates, data.frame(
              Metric_Name = m_name,
              Value = val_num,
              Is_Error = is_err
            ))
          }
        }
      }
      
      # Select Winner Logic
      if (nrow(candidates) > 0) {
        winner <- NULL
        perf_metrics <- candidates[!candidates$Is_Error, ]
        error_metrics <- candidates[candidates$Is_Error, ]
        
        if (nrow(perf_metrics) > 0) {
          winner <- perf_metrics[which.max(perf_metrics$Value), ]
        } else if (nrow(error_metrics) > 0) {
          winner <- error_metrics[which.min(error_metrics$Value), ]
        }
        
        if (!is.null(winner) && nrow(winner) == 1) {
          df_clean$Best.Metric[i] <- winner$Metric_Name
          df_clean$Best.Metric.Val[i] <- winner$Value
        }
      }
    }
    
  }, error = function(e) {
    # IF AN ERROR OCCURS:
    # Print a small warning to the console so you know which row failed
    message(paste("Skipping Row", i, "due to error:", e$message))
    # No need to assign NA manually; the columns were initialized as NA
  })
}

# 1. Create a logical vector (TRUE/FALSE) for matches
has_keywords <- grepl("posture|activity|energy", 
                      df_clean$Corresponding.Metrics.and.Sites.for.the.following.table, 
                      ignore.case = TRUE)

# 2. Get the row numbers (Indices)
row_indices <- which(has_keywords)

# 3. Print the results
print(paste("Found", length(row_indices), "matching rows."))
print(row_indices)

# Optional: View the text of the first few matches to confirm
df_keywords <- df_clean[has_keywords,]

# Remove rows with keywords from df_clean
df_clean <- df_clean[-row_indices,]


saveRDS(df_clean, file = "data/metrics_to_tally.RDS")

saveRDS(df_keywords, file = "data/metrics_to_tally_manual.RDS")
write.csv(df_keywords, file = "data/metrics_to_tally_manual.csv")
