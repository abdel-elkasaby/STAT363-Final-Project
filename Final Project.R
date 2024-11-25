library(ggplot2)
library(dplyr)

power <- read.csv('C:/Users/abdel/OneDrive/Desktop/PSU/STAT363/Final Project/global_power_plant_database.csv', na.strings = c(NA, "", "NA"), header = TRUE)

# Filter out rows with NA values in 'latitude', 'longitude', and 'primary_fuel'
filtered_power <- power %>% filter(!is.na(latitude) & !is.na(longitude) & !is.na(primary_fuel))

str(filtered_power)
summary(filtered_power)

# 1. Map of Power Plant Locations by Fuel Type
ggplot() +
  borders("world", colour = "gray50", fill = "gray50") +
  geom_point(data = filtered_power, aes(x = longitude, y = latitude, color = primary_fuel), alpha = 0.6, size = 1) +
  labs(title = "Geographic Distribution of Power Plants by Fuel Type",
       x = "Longitude",
       y = "Latitude",
       color = "Primary Fuel") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Select top 10 countries by the number of power plants
top_countries <- filtered_power %>%
  group_by(country_long) %>%
  summarize(count = n()) %>%
  top_n(10, count) %>%
  pull(country_long)

# 2. Proportion of Each Fuel Type by Country using ggplot2
ggplot(filtered_power %>% filter(country_long %in% top_countries), aes(x = country_long, fill = primary_fuel)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Each Fuel Type by Country",
       x = "Country",
       y = "Proportion",
       fill = "Primary Fuel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare the data for the bar plot
country_fuel_counts <- table(filtered_power$country_long[filtered_power$country_long %in% top_countries],
                             filtered_power$primary_fuel[filtered_power$country_long %in% top_countries])

colors <- rainbow(nrow(country_fuel_counts))
# 3. Bar Plot of the Number of Power Plants by Fuel Type for the Top 10 Countries using base R
par(mar = c(5, 4, 4, 8), xpd = TRUE) 

barplot(country_fuel_counts, beside = TRUE, legend = FALSE, 
        col = colors,
        main = "Number of Power Plants by Fuel Type for the Top 10 Countries",
        xlab = "Fuel Type",
        ylab = "Number of Power Plants",
        las = 2, cex.names = 0.7)
legend("topright", inset = c(-0.15, 0), legend = rownames(country_fuel_counts), fill = colors, cex = 0.5)

# 4. Facet Wrap of Histograms of Fuel Types for the Top 10 Countries using ggplot2
ggplot(filtered_power %>% filter(country_long %in% top_countries), aes(x = primary_fuel, fill = primary_fuel)) +
  geom_bar() +
  facet_wrap(~ country_long) +
  labs(title = "Distribution of Fuel Types for the Top 10 Countries",
       x = NULL,
       y = "Number of Power Plants",
       fill = "Fuel Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 10))