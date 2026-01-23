library(tidyverse)

df_tally <- readRDS("data/data_to_tally.RDS")

# 1. Define the mapping based on your "Vertical Position" sheet
# List structure: Name = Category, Values = Search Terms
location_map_list <- list(
  "head"          = c("head", "ear", "next to ears", "side of head", "left ear"),
  "chest"         = c("chest", "necklace", "shirt pocket", "jacket top pocket", "chest front pocket", "midback", "lower neck", "torso"),
  "ribs"          = c("ribs", "left ribs", "ribs front pocket", "left lower ribs"),
  "shoulder"      = c("shoulder", "right shoulder"),
  "upper_arm"     = c("upper arm", "right upper arm", "left upper arm", "right arm", "arm"),
  "lower_arm"     = c("lower arm", "right lower arm", "left lower arm", "dominant lower arm", "dominant upper arm", "forearm"),
  "wrist"         = c("wrist", "right wrist", "left wrist", "dominant wrist", "non-dominant wrist"),
  "hand"          = c("hand", "left hand", "right hand"),
  "waist"         = c("waist", "hip", "low back", "left hip", "right hip", "belt", "non-dominant hip", "center hip", "center waist", "back", "dominant hip", "side of waist", "front of waist", "cm (center of mass)", "cervix"),
  "pants_pocket"  = c("pants pocket", "right pocket", "left pocket", "right pants pocket", "pocket", "front pants pocket", "waist front pocket", "rear pants pocket", "left pants pocket"),
  "thigh"         = c("thigh", "right thigh", "left thigh", "dominant thigh"),
  "knee"          = c("knee", "right knee", "left knee"),
  "shank"         = c("shank", "lower leg", "calf", "shin", "right shank"),
  "ankle"         = c("ankle", "right ankle", "left ankle", "dominant ankle", "non-dominant ankle"),
  "foot"          = c("foot", "left foot", "right foot", "right shoe"),
  "other"         = c("other", "bag", "backpack", "jacket side pocket", "handbag", "hand while not using phone", "hand while using phone")
)

# 2. Convert this list into a clean Lookup Dataframe
# This creates a table with two columns: 'term' and 'category'
lookup_df <- stack(location_map_list) %>%
  rename(term = values, category = ind) %>%
  mutate(term = tolower(term)) # Ensure all terms are lowercase for matching

# Preview the lookup table
head(lookup_df)


# --- STEP 3: Process with Counts (The Modified Part) ---
df_counts <- df_tally %>%
  mutate(row_id = row_number()) %>% # Create a unique ID for each study
  
  # A. Split into individual rows
  separate_rows(Wear.Locations.Included, sep = "[;,]") %>%
  
  # B. Clean text
  mutate(
    clean_term = Wear.Locations.Included %>%
      str_remove_all("Other:") %>%
      str_trim() %>%
      str_to_lower()
  ) %>%
  
  # C. Join with Lookup Table
  left_join(lookup_df, by = c("clean_term" = "term")) %>%
  
  # D. Handle uncategorized items (optional)
  mutate(category = replace_na(as.character(category), "uncategorized")) %>%
  
  # E. Count UNIQUE terms per category for each study
  #    If a study has "left wrist" and "right wrist", this counts as 2.
  #    If a study has "wrist", this counts as 1.
  group_by(row_id, First.Author, Year, category) %>%
  summarize(subcat_count = n_distinct(clean_term), .groups = "drop") %>%
  
  # F. Pivot to Wide Format
  pivot_wider(
    names_from = category,
    values_from = subcat_count,
    values_fill = 0 # Fill missing categories with 0 count
  ) %>%
  
  # G. Remove row_id if you don't need it anymore
  select(-row_id)

# View the result
print(head(df_counts))

# ------------------------------------------------------------------------------
# Get simple count table
# ------------------------------------------------------------------------------

# 1. Clean up column names: Replace underscores with dots
#    df_counts is the dataframe you created in the previous step
df_counts_dotted <- df_counts %>%
  rename_with(~ str_replace_all(., "_", "."), .cols = -c(First.Author, Year))

# 2. Select location columns
location_cols <- df_counts_dotted %>%
  select(-First.Author, -Year)

# 3. Summarize (Standard Split)
#    Now we don't need the "___" trick. The default "_" is fine.
summary_table <- location_cols %>%
  summarise(across(everything(), list(
    included = ~ sum(. > 0, na.rm = TRUE),
    multisub = ~ sum(. > 1, na.rm = TRUE)  # <--- Changed from 'multi_sub' to 'multisub'
  ))) %>%
  
  # Now the split works perfectly because there is only one underscore in the name
  # e.g., "upper.arm_multisub" splits into "upper.arm" and "multisub"
  pivot_longer(everything(), names_to = c("category", "metric"), names_sep = "_") %>%
  
  pivot_wider(names_from = metric, values_from = value) %>%
  column_to_rownames("category") %>%
  t() %>%
  as.data.frame()

print(summary_table)



# Next, identify which model performed the best and only use that one to compare results across studies.
# To do this, need to average the results across sites for each metric in a given study, then choose
# only the metric with the highest average results.

