# Island summary calculations


# Install and load packages
sapply(c("dplyr", "tidyr", "readr"), require, character.only=T)

# Load data
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Correct data structure: convert character columns to factors
str(fauna)
fauna[] <- lapply(fauna, function(x) if(is.character(x)) factor(x) else x)
str(fauna)

# Make subset of unique island data
islands <- fauna %>%
  distinct(Island.archipelago, Archipelago, Island, Latitude, Longitude, .keep_all = FALSE)

# Group data per island and summarize the number of extinct, native and introduced species
fauna_summary <- fauna %>%
  group_by(Island.archipelago, AlienNativeExtinct) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(AlienNativeExtinct), values_from = count, values_fill = 0)

# Merge the summarized fauna groups with the unique island name data
merged_data <- islands %>%
  left_join(fauna_summary, by = "Island.archipelago")

# Add columns summarizing the number of birds, mammals and reptiles.
fauna_summary2 <- fauna %>%
  group_by(Island.archipelago, Class) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(Class), values_from = count, values_fill = 0)
merged_data2 <- merged_data %>%
  left_join(fauna_summary2, by = "Island.archipelago")

# Calculate "before" and "after" species richness columns
merged_data3 <- merged_data2 %>%
  mutate(
    Before = Extinct + Native,
    After = Native + Alien )

# Calculate proportion extinction and proportion introduced species for each island community
merged_data4 <- merged_data3 %>%
  mutate(
    Proportion.extinct = ifelse(Before > 0, Extinct / Before, 0),
    Proportion.introduced = ifelse(After > 0, Alien / After, 0) )

# Change column name "Alien" to "Non-native"
colnames(merged_data4)[colnames(merged_data4) == "Alien"] <- "Non_native"

# Look at data (this is the information found in Table S1)
head(merged_data4)
summary(merged_data4)

# Save data as CSV file (UTF-8 encoded to keep special characters)
write_csv(merged_data4,"Island Summary data.csv") 

###


