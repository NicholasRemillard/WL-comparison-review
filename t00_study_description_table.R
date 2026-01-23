library(dplyr)

df <- readRDS("data/extraction_data.RDS")

df_description <- df %>% select(
  First.Author,
  Year,
  Journal,
  Journal_category,
  Total.number.of.male.participants,
  Total.number.of.female.participants,
  Age.Mean,
  Age.SD,
  Population.Age.Range,
  Physical.Activity.Outcomes.Evaluated,
  If.outcome.was.categorical..list.categories.here.,
  Protocol.Activities...Categories,
  Accelerometer.based.Device,
  Sensors.used,
  Wear.Locations.Included,
  Algorithm.s..Included.in.Analysis
)
