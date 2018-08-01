# Packages
library(dplyr)

# Load data
load("Data/1_reading_cleaning/ankervakken_9d_uniek_schip.Rda")
load("Data/1_reading_cleaning/ship_features.Rda")
ship_specs_mt <- read.csv2("Data/1_reading_cleaning/ShipSpecsMT.csv", sep = ",", stringsAsFactors=FALSE)

# Make files mergeable
ankervakken_9d_uniek_schip <-
  ankervakken_9d_uniek_schip %>%
  mutate(in_ankervakken_9d = 1)

ship_features <-
  ship_features %>%
  mutate(IMO = as.character(IMO),
         in_features = 1)

ship_specs_mt <-
  ship_specs_mt %>%
  select(-X) %>%
  rename(Name_MT = Name,
         Flag_MT = Flag,
         IMO_MT  = IMO) %>%
  mutate(MMSI  = as.character(MMSI),
         in_MT = 1)

# Merge Marine Traffic data on MMSI, add missing IMOs from MT, and then merge ship features on IMO
ship_specs_almost_complete <-
  ankervakken_9d_uniek_schip %>%
  left_join(ship_specs_mt, by=c("MMSI","MMSI")) %>%
  mutate(IMO = if_else(IMO == "", IMO_MT, IMO))

ship_specs_complete <-
  ship_specs_almost_complete %>%
  left_join(ship_features, by=c("IMO", "IMO"))

# Count merged records  
merge_table <-
  ship_specs_complete %>%
  group_by(in_ankervakken_9d, in_MT, in_features) %>%
  summarise(n())

# Fill missings as much as possible
# Leading is the 'ankervakken'-data, then Marine Traffic, then the feature set
# Exception is IMO, which was already filled during merging
ship_specs_complete <-
  ship_specs_complete %>%
  mutate(Name          = if_else(Name == "", Name_MT, Name),
         Flag          = if_else(Flag_MT == "webpage not found", Flag, Flag_MT),
         Gross.Tonnage = if_else(Gross.Tonnage %in% c("-", "webpage not found"), as.character(Ship_GT), Gross.Tonnage))
ship_specs_complete$Flag <-
  sapply(ship_specs_complete$Flag,
         function(y) substr(y, regexpr("[", y, fixed = TRUE)[1] + 1, regexpr("[", y, fixed = TRUE)[1] + 2))

# Delete redundant variables
ship_specs_complete <-
  ship_specs_complete %>%
  select(-in_ankervakken_9d,
         -in_MT,
         -in_features,
         -IMO_MT,
         -Name_MT,
         -Flag_MT,
         -YearMonth,
         -response)
ship_specs_complete <-
  ship_specs_complete %>%
  select(grep("^def",  names(ship_specs_complete), value = TRUE, invert = TRUE))
ship_specs_complete <-
  ship_specs_complete %>%
  select(grep("^cert",  names(ship_specs_complete), value = TRUE, invert = TRUE))  

# Save file
save(ship_specs_complete, file="Data/1_reading_cleaning/ship_specs_complete.Rda")


