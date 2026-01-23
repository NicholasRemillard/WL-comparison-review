library(dplyr)
library(stringr)

df <- readRDS("data/extraction_data.RDS")

df_tally <- df %>% select(
  First.Author,
  Year,
  Physical.Activity.Outcomes.Evaluated,
  Wear.Locations.Included,
  Corresponding.Metrics.and.Sites.for.the.following.table,
  Site.1.Metric.1,
  Site.1.Metric.2,
  Site.1.Metric.3,
  Site.1.Metric.4,
  Site.1.Metric.5,
  Site.1.Metric.6,
  Site.2.Metric.1,
  Site.2.Metric.2,
  Site.2.Metric.3,
  Site.2.Metric.4,
  Site.2.Metric.5,
  Site.2.Metric.6,
  Site.3.Metric.1,
  Site.3.Metric.2,
  Site.3.Metric.3,
  Site.3.Metric.4,
  Site.3.Metric.5,
  Site.3.Metric.6,
  Site.4.Metric.1,
  Site.4.Metric.2,
  Site.4.Metric.3,
  Site.4.Metric.4,
  Site.4.Metric.5,
  Site.4.Metric.6,
  Site.5.Metric.1,
  Site.5.Metric.2,
  Site.5.Metric.3,
  Site.5.Metric.4,
  Site.5.Metric.5,
  Site.5.Metric.6,
  Site.6.Metric.1,
  Site.6.Metric.2,
  Site.6.Metric.3,
  Site.6.Metric.4,
  Site.6.Metric.5,
  Site.6.Metric.6,
  Site.7.Metric.1,
  Site.7.Metric.2,
  Site.7.Metric.3,
  Site.7.Metric.4,
  Site.7.Metric.5,
  Site.7.Metric.6,
  Site.8.Metric.1,
  Site.8.Metric.2,
  Site.8.Metric.3,
  Site.8.Metric.4,
  Site.8.Metric.5,
  Site.8.Metric.6,
  Additional.Notes
)

# Save for next script
saveRDS(df_tally, file = "data/data_to_tally.RDS")



# Get list of unique locations

# 1. Split by semicolon OR comma
all_parts <- unlist(str_split(df_tally$Wear.Locations.Included, "[;,]"))

# 2. Remove "Other:", trim whitespace, and convert to lowercase
clean_parts <- all_parts %>%
  str_remove_all(fixed("Other:", ignore_case = TRUE)) %>%
  str_trim() %>%
  str_to_lower()

# 3. Get unique values (removing empty strings)
unique_locations <- unique(clean_parts[clean_parts != ""])

print(unique_locations)

# 1. Convert the vector to a data frame
#    We name the column "location" for clarity in the CSV
locations_df <- data.frame(location = unique_locations)

# 2. Save the file
#    row.names = FALSE prevents R from adding a numbered index column (1, 2, 3...)
write.csv(locations_df, "unique_locations.csv", row.names = FALSE)

# Use the above to organize a look up table for the next part of the script. If the above is modified,
# the subsquent script mapping the terms must be updated as well.
