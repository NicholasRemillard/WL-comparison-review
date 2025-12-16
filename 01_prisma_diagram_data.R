library(dplyr)
library(stringr)

df <- readRDS("data/extraction_data.RDS")

df <- read.csv("data/covidence_excluded_studies.csv")

df_other <- df %>% dplyr::filter(stringr::str_detect(Notes, "Other"))

df_features <- df %>% dplyr::filter(stringr::str_detect(Notes, "features"))

df_loc <- df %>% dplyr::filter(stringr::str_detect(Notes, "per location"))

summary_df <- df %>% 
  summarise(n_did_not_compare = sum(str_detect(Notes, "Did not compare wear")),
            n_wrong_outcomes = sum(str_detect(Notes, "Wrong PA|wrong model performance")),
            n_wrong_study_design = sum(str_detect(Notes, "calibration|Only 1 wear")),
            n_gait_abnormalities = sum(str_detect(Notes, "gait")),
            n_non_wearable = sum(str_detect(Notes, "includes non-wearable sensor")),
            n_no_criterion = sum(str_detect(Notes, "No criterion")),
            n_no_english = sum(str_detect(Notes, "Not in English")),
            n_no_acc = sum(str_detect(Notes, "Device does not have accelerometer")),
            n_Other = sum(str_detect(Notes, "Other")),
            n_features = sum(str_detect(Notes, "features|per location"))
        )

# Check that all are accounted for
nrow(df) == rowSums(summary_df, na.rm = TRUE)

# Other had 9 studies - need to categorize into category that fits best
# Song 2009 - wrong study design
# vanHees 2013 - wrong study design
# Rokni 2019 - wrong outcomes
# Rokni 2017 - wrong outcomes
# Figueira 2016 - wrong study design
# Cvetković 2015 - wrong study design
# Yang 2010 - missing critical info
# Taylor 2012 - wrong study design
# Ehatisham-ul-Haq 2022 - wrong study design

# Wrong study design - add 6
# wrong outcomes - add 2
# Yang - in its own category
