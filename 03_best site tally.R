library(dplyr)
library(stringr)

df <- readRDS("data/extraction_data.RDS")

df_tally <- df %>% select(
  First.Author,
  Year,
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
  Site.8.Metric.6
)


# Get list of unique locations
# 1. Split the strings by either semicolon OR comma
#    "[;,]" is a regex that means "semicolon or comma"
all_parts <- unlist(str_split(df_tally$Wear.Locations.Included, "[;,]"))

# 2. Remove the "Other:" text (case insensitive)
#    We replace it with an empty string
clean_parts <- str_remove_all(all_parts, fixed("Other:", ignore_case = TRUE))

# 3. Trim whitespace from the start and end of each word
clean_parts <- str_trim(clean_parts)

# 4. Get unique values and remove any empty strings created by the cleaning
unique_locations <- unique(clean_parts[clean_parts != ""])

# View the result
print(unique_locations)
