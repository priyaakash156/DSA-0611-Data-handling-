# Load necessary libraries
library(ggplot2)
library(maps)
library(ggmap)
library(viridis)
library(dplyr)

# City data
city_data <- data.frame(
  City = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
  Latitude = c(40.7128, 34.0522, 41.8781, 29.7604, 33.4484),
  Longitude = c(-74.0060, -118.2437, -87.6298, -95.3698, -112.0740),
  Population = c(8398748, 3990456, 2705994, 2325502, 1660272)
)

# Scatter plot on a map based on Latitude and Longitude
world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = city_data, aes(x = Longitude, y = Latitude), color = "blue", size = 3) +
  labs(title = "Cities on the Map", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Bubble map where bubble size represents Population
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = city_data, aes(x = Longitude, y = Latitude, size = Population), color = "blue", alpha = 0.6) +
  scale_size_continuous(range = c(3, 12)) +
  labs(title = "Bubble Map of Cities by Population", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Choropleth map of Population by city (use different colors to represent population sizes)
us_map <- map_data("state")
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = city_data, aes(x = Longitude, y = Latitude, color = Population, size = Population), alpha = 0.6) +
  scale_color_viridis(option = "viridis") +
  scale_size_continuous(range = c(3, 12)) +
  labs(title = "Choropleth Map of Cities by Population", x = "Longitude", y = "Latitude", color = "Population") +
  theme_minimal()

# Bar plot showing the top 5 cities by Population
ggplot(data = city_data, aes(x = reorder(City, -Population), y = Population, fill = City)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Cities by Population", x = "City", y = "Population") +
  theme_minimal() +
  theme(legend.position = "none")

# Generate a heatmap showing the density of cities based on Population
ggplot(city_data, aes(x = Longitude, y = Latitude)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", color = "white") +
  scale_fill_viridis(option = "viridis") +
  geom_point(data = city_data, aes(size = Population), color = "red", alpha = 0.6) +
  scale_size_continuous(range = c(3, 12)) +
  labs(title = "Heatmap of City Density by Population", x = "Longitude", y = "Latitude", fill = "Density") +
  theme_minimal()

# Display the plots
