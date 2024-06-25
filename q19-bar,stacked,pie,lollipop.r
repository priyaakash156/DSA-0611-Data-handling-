# Load required libraries
library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Rating = c("Good", "Excellent", "Fair", "Poor", "Good")
)

# Count occurrences of each Rating category
rating_counts <- table(data$Rating)

# Create bar plot
ggplot(data, aes(x = Rating)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Count of Each Rating Category", x = "Rating", y = "Count") +
  theme_minimal()

# Create stacked bar plot
ggplot(data, aes(x = factor(ID), fill = Rating)) +
  geom_bar() +
  labs(title = "Stacked Bar Plot of Rating over IDs", x = "ID", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Calculate proportion of each Rating category
rating_prop <- prop.table(table(data$Rating))
rating_prop_df <- data.frame(rating_prop)
names(rating_prop_df) <- c("Rating", "Freq")

# Create pie chart with values inside
ggplot(rating_prop_df, aes(x = "", y = Freq, fill = factor(Rating))) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Freq * 100, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Pie Chart of Rating Distribution", fill = "Rating") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

# Calculate average rating
average_rating <- data %>%
  group_by(Rating) %>%
  summarise(avg_ID = mean(ID))

# Create lollipop plot
ggplot(average_rating, aes(x = Rating, y = avg_ID)) +
  geom_segment(aes(x = Rating, xend = Rating, y = 0, yend = avg_ID), color = "skyblue") +
  geom_point(size = 3, color = "blue") +
  labs(title = "Lollipop Plot of Average Rating", x = "Rating", y = "Average ID") +
  theme_minimal()

# Count frequencies of each Rating category
freq_ratings <- data %>%
  group_by(Rating) %>%
  summarise(count = n())

# Create bar chart
ggplot(freq_ratings, aes(x = Rating, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Each Rating Category", x = "Rating", y = "Frequency") +
  theme_minimal()
