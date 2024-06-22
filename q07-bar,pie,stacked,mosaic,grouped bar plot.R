# Load required libraries
library(ggplot2)
library(gridExtra)
library(vcd)

# Data
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Gender = c("Male", "Female", "Male", "Female", "Male"),
  Education = c("Graduate", "Undergrad", "High School", "Graduate", "Undergrad"),
  Occupation = c("Engineer", "Teacher", "Doctor", "Lawyer", "Artist")
)

# 1. Bar plot showing the count of each Gender
ggplot(data, aes(x = Gender)) +
  geom_bar() +
  labs(title = "Count of Each Gender", x = "Gender", y = "Count")

# 2. Pie chart representing the distribution of Education levels
ggplot(data, aes(x = "", fill = Education)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Education Levels")

# 3. Stacked bar plot showing the count of each Occupation by Gender
ggplot(data, aes(x = Gender, fill = Occupation)) +
  geom_bar(position = "stack") +
  labs(title = "Count of Each Occupation by Gender", x = "Gender", y = "Count")

# 4. Mosaic plot to visualize the association between Education and Occupation
mosaic(~ Education + Occupation, data = data, main = "Mosaic Plot of Education and Occupation")

# 5. Grouped bar plot showing counts of Gender across different Education levels
ggplot(data, aes(x = Education, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Counts of Gender Across Different Education Levels", x = "Education", y = "Count")
