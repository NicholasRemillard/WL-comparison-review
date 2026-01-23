library(stringr)
library(tidyverse)

df_summary <- readRDS("data/best_loc_per_study.RDS")


# 1. Tally Wear Locations per Outcome
wear_counts <- df_summary %>%
  count(Physical.Activity.Outcomes.Evaluated, Wear.Locations.Included, sort = FALSE)

# 2. Tally Broad Categories per Outcome
broad_counts <- df_summary %>%
  count(Physical.Activity.Outcomes.Evaluated, Broad.Category, sort = FALSE)

summary_nested <- df_summary %>%
  group_by(Physical.Activity.Outcomes.Evaluated) %>%
  summarise(
    # Create a string summary for Broad Category
    Broad_Cat_Tally = paste(names(table(Broad.Category)), 
                            table(Broad.Category), 
                            sep = ": ", collapse = ", "),
    
    # Create a string summary for specific Wear Locations
    Wear_Loc_Tally = paste(names(table(Wear.Locations.Included)), 
                           table(Wear.Locations.Included), 
                           sep = ": ", collapse = ", "),
    
    .groups = "drop"
  )

# Filter for rows containing a semicolon in the Outcome column
studies_with_combined_outcomes <- df_summary %>%
  filter(str_detect(Physical.Activity.Outcomes.Evaluated, ";")) %>%
  select(First.Author, Year, Physical.Activity.Outcomes.Evaluated) %>%
  distinct() # Removes duplicates if a study has multiple rows with the same combined label

# View the result
print(studies_with_combined_outcomes)
