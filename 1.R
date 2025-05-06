library(tidyverse)  # includes readr, dplyr, ggplot2
library(janitor)    # for cleaning column names
bird_data <- read_csv("https://raw.githubusercontent.com/jagatnshah/project5202/main/Birds%20of%20Bardia%20National%20Park.csv") %>%
  clean_names() %>%       # converts column names to snake_case
  fill(family)            # fills down missing family values
colnames(bird_data)
threatened_species <- bird_data %>%
  filter(globally_threatened_status %in% c("NT", "VU", "EN"))
threatened_by_family <- bird_data %>%
  filter(globally_threatened_status %in% c("NT", "VU", "EN")) %>%
  group_by(family) %>%
  summarise(threatened_species_count = n()) %>%
  arrange(desc(threatened_species_count))
print(threatened_by_family)
ggplot(threatened_by_family, aes(x = reorder(family, threatened_species_count), 
                                 y = threatened_species_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Globally Threatened Bird Species by Family",
    x = "Family",
    y = "Number of Threatened Species"
  ) +
  theme_minimal()
# Save it to working directory
ggsave("C:/Users/jagat/Desktop/R plot/threatened_species_by_family.png", 
       width = 8, height = 6, dpi = 300)
migration_by_family <- bird_data %>%
  group_by(family, migration_status) %>%
  summarise(species_count = n(), .groups = 'drop') %>%
  arrange(family, migration_status)
# Update "W" to "w" in migration_status
bird_data <- bird_data %>%
  mutate(migration_status = recode(migration_status, "W" = "w"))
# Now, summarize and plot the migration pattern by family
migration_by_family <- bird_data %>%
  group_by(family, migration_status) %>%
  summarise(species_count = n(), .groups = 'drop') %>%
  arrange(family, migration_status)
# Create the plot
ggplot(migration_by_family, aes(x = reorder(family, species_count), 
                                y = species_count, fill = migration_status)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Migration Pattern Per Family",
    x = "Family",
    y = "Number of Species",
    fill = "Migration Status"
  ) +
  theme_minimal()
species_richness <- bird_data %>%
  group_by(family) %>%
  summarise(species_count = n_distinct(scientific_name)) %>%
  arrange(desc(species_count))
ggplot(species_richness, aes(x = reorder(family, species_count), y = species_count)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Species Richness Per Family",
    x = "Family",
    y = "Number of Species"
  ) +
  theme_minimal()
ggsave("C:/Users/jagat/Desktop/R plot/Species Richness Per Family.png", 
       width = 8, height = 6, dpi = 300)
# Threatened status
bird_data <- bird_data %>%
  mutate(threat_level = if_else(globally_threatened_status %in% c("NT", "VU", "EN"), 
                                "Threatened", "Not Threatened"))
threat_counts <- bird_data %>%
  group_by(threat_level) %>%
  summarise(count = n_distinct(scientific_name))
ggplot(threat_counts, aes(x = "", y = count, fill = threat_level)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Threatened vs Non-Threatened Species", fill = "Status") +
  theme_void()
ggsave("C:/Users/jagat/Desktop/R plot/threat_pie_chart.png",
       width = 6, height = 6, dpi = 300)
# Count species per migration status
migration_counts <- bird_data %>%
  group_by(migration_status) %>%
  summarise(count = n_distinct(scientific_name))
# Basic pie chart
ggplot(migration_counts, aes(x = "", y = count, fill = migration_status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Species by Migration Pattern", fill = "Migration") +
  theme_void()
ggsave("C:/Users/jagat/Desktop/R plot/migration_pie_chart.png",
       width = 6, height = 6, dpi = 300)
# Count + percent
migration_counts <- bird_data %>%
  group_by(migration_status) %>%
  summarise(count = n_distinct(scientific_name)) %>%
  mutate(percent = round(100 * count / sum(count), 1),
         label = paste0(migration_status, ": ", percent, "%"))
# Pie chart
ggplot(migration_counts, aes(x = "", y = count, fill = migration_status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Species by Migration Pattern", fill = "Migration") +
  theme_void()
ggsave("C:/Users/jagat/Desktop/R plot/migration percent_pie_chart.png",
       width = 6, height = 6, dpi = 300)
