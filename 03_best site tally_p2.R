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

library(stringr)

# 1. Create a clean working copy
df_long <- df_tally

# 2. Clean the 'Wear.Locations.Included' column
# We remove "Other:" so it doesn't get treated as a site name
df_long$Wear.Locations.Included <- gsub("Other:", "", df_long$Wear.Locations.Included, ignore.case = TRUE)

# 3. Expand the dataframe (The "Long" transformation)
# We split by either a semicolon (;) OR a comma (,) to catch all sites
df_long <- df_long %>%
  separate_rows(Wear.Locations.Included, sep = "[;,]") %>%
  mutate(
    # Trim extra whitespace (e.g., " shank" becomes "shank")
    Wear.Locations.Included = str_trim(Wear.Locations.Included)
  ) %>%
  # Remove any empty rows that might have been created by trailing separators
  filter(Wear.Locations.Included != "")

# Add broad category
get_broad_category <- function(raw_site, map) {
  clean_site <- tolower(trimws(raw_site))
  
  for (cat in names(map)) {
    if (clean_site %in% map[[cat]]) {
      return(cat)
    }
  }
  return("unknown") # Returns 'unknown' if not found in your list
}

# 3. Create the column and move it
df_long <- df_long %>%
  rowwise() %>% # Apply the function row by row
  mutate(Broad.Category = get_broad_category(Wear.Locations.Included, location_map_list)) %>%
  ungroup() %>%
  relocate(Broad.Category, .before = Wear.Locations.Included) # Move it to the specific spot

# ------------------------------------------------------------------------------
# Shortening and cleaning data frame
# ------------------------------------------------------------------------------

df_clean <- as.data.frame(df_long %>% 
                            select(
                              First.Author, 
                              Year, 
                              Physical.Activity.Outcomes.Evaluated, 
                              Broad.Category, 
                              Wear.Locations.Included,
                              Corresponding.Metrics.and.Sites.for.the.following.table
                            ))

# 2. Add the 8 Metric columns, initialized with NA
# We do this using base R to ensure it's a simple structure
for(m in 1:8) {
  df_clean[[paste0("Metric.", m)]] <- NA_character_
}

# 3. Helper Function (Same as before)
find_site_index <- function(meta_string, current_site_name) {
  if (is.na(meta_string) || meta_string == "") return(NA)
  
  lines <- unlist(strsplit(meta_string, "\n"))
  site_lines <- lines[grepl("^Site", lines, ignore.case = TRUE)]
  target_name <- tolower(str_trim(current_site_name))
  
  for (line in site_lines) {
    s_idx <- as.integer(str_extract(line, "(?<=Site )\\d+"))
    s_name <- tolower(str_trim(sub("^Site \\d+:", "", line)))
    
    if (target_name == s_name) {
      return(s_idx)
    }
  }
  return(NA)
}

# 4. The Loop: Read from 'df_long', Write to 'df_clean'
for (i in 1:nrow(df_clean)) {
  
  # Get info
  meta_text <- df_clean$Corresponding.Metrics.and.Sites.for.the.following.table[i]
  current_location <- df_clean$Wear.Locations.Included[i]
  
  # Find the Index
  site_idx <- find_site_index(meta_text, current_location)
  
  if (!is.na(site_idx)) {
    for (m in 1:8) {
      # Construct the column name to look for in the ORIGINAL dataframe
      old_col_name <- paste0("Site.", site_idx, ".Metric.", m)
      
      # Check if that column exists in the source data
      if (old_col_name %in% names(df_long)) {
        # READ from df_long, WRITE to df_clean
        val <- as.character(df_long[i, old_col_name])
        df_clean[i, paste0("Metric.", m)] <- val
      }
    }
  }
}

# Export for manual inspection
write.csv(df_clean, "data/cleaned_site_metrics.csv")
