# Load required libraries
library(ggplot2)
library(plotly)
library(tidyverse)

# Create the data frame
data <- data.frame(
  Student = c('A', 'B', 'C', 'D', 'E'),
  Math_Score = c(85, 72, 90, 78, 88),
  Reading_Score = c(78, 85, 80, 75, 82),
  Attendance = c(95, 92, 98, 85, 93)
)

# 1. How do reading scores vary with both math scores and attendance percentage among students?
summary(data)

# 2. Create a 3D scatter plot to visualize the relationship between math score, reading score, and attendance.
plot_ly(data, x = ~Math_Score, y = ~Reading_Score, z = ~Attendance, type = 'scatter3d', mode = 'markers') %>%
  layout(title = '3D Scatter Plot of Math Score, Reading Score, and Attendance',
         scene = list(
           xaxis = list(title = 'Math Score'),
           yaxis = list(title = 'Reading Score'),
           zaxis = list(title = 'Attendance (%)')
         ))

# 3. Is there a correlation between attendance, math scores, and reading scores based on the 3D plot?
# Calculate correlation matrix
cor_matrix <- cor(data[, c("Math_Score", "Reading_Score", "Attendance")])
print(cor_matrix)

# 4. Generate a 3D surface plot to show how reading scores change with variations in both math scores and attendance.
# Create a grid for surface plot
math_seq <- seq(min(data$Math_Score), max(data$Math_Score), length.out = 50)
attendance_seq <- seq(min(data$Attendance), max(data$Attendance), length.out = 50)
grid <- expand.grid(Math_Score = math_seq, Attendance = attendance_seq)

# Fit a model to predict Reading_Score based on Math_Score and Attendance
model <- lm(Reading_Score ~ Math_Score * Attendance, data = data)
grid$Reading_Score <- predict(model, newdata = grid)

plot_ly(grid, x = ~Math_Score, y = ~Reading_Score, z = ~Attendance, type = 'mesh3d') %>%
  layout(title = '3D Surface Plot of Reading Score',
         scene = list(
           xaxis = list(title = 'Math Score'),
           yaxis = list(title = 'Reading Score'),
           zaxis = list(title = 'Attendance (%)')
         ))

# 5. Compare the 3D plots of reading scores against both math scores and attendance separately. Are there any significant insights or outliers?
# Separate 3D scatter plots for comparison
scatter_math <- plot_ly(data, x = ~Math_Score, y = ~Reading_Score, z = ~Attendance, type = 'scatter3d', mode = 'markers') %>%
  layout(title = '3D Scatter Plot: Math Score vs Reading Score vs Attendance',
         scene = list(
           xaxis = list(title = 'Math Score'),
           yaxis = list(title = 'Reading Score'),
           zaxis = list(title = 'Attendance (%)')
         ))

scatter_attendance <- plot_ly(data, x = ~Attendance, y = ~Reading_Score, z = ~Math_Score, type = 'scatter3d', mode = 'markers') %>%
  layout(title = '3D Scatter Plot: Attendance vs Reading Score vs Math Score',
         scene = list(
           xaxis = list(title = 'Attendance (%)'),
           yaxis = list(title = 'Reading Score'),
           zaxis = list(title = 'Math Score')
         ))

# Display both plots for comparison
subplot(scatter_math, scatter_attendance)

# Insights and outliers
# From the correlation matrix and 3D plots, we can identify any significant correlations or outliers.
