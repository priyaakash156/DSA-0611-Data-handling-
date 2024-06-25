# Load required libraries
library(ggplot2)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Score = c(85, 92, 78, 88, 90)
)

# Create histogram of Score
ggplot(data, aes(x = Score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Scores", x = "Score", y = "Frequency")
# Box plot of Score
ggplot(data, aes(y = Score)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Box Plot of Scores", y = "Score")
# Create data for bar chart (count of scores in different ranges)
score_ranges <- cut(data$Score, breaks = c(0, 50, 100), labels = c("0-50", "51-100"), include.lowest = TRUE)

# Count scores in each range
score_counts <- table(score_ranges)

# Create bar chart
bar_data <- data.frame(score_range = names(score_counts), count = as.numeric(score_counts))

ggplot(bar_data, aes(x = score_range, y = count, fill = score_range)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Count of Scores in Different Ranges", x = "Score Range", y = "Count") +
  scale_fill_manual(values = c("0-50" = "skyblue", "51-100" = "lightgreen"))

# Density plot of Score
ggplot(data, aes(x = Score)) +
  geom_density(fill = "green", alpha = 0.5) +
  ggtitle("Density Plot of Scores") +
  xlab("Score") +
  ylab("Density") +
  theme_minimal()

# Violin plot of Score
ggplot(data, aes(x = "", y = Score, fill = ..density..)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), scale = "width", alpha = 0.5) +
  labs(title = "Violin Plot of Scores", y = "Score", x = "") +
  theme_minimal()
