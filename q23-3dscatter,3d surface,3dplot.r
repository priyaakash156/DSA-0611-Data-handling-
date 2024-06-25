# Load necessary libraries
library(plotly)
library(akima)

# Create the dataset
data <- data.frame(
  Patient = c('A', 'B', 'C', 'D', 'E'),
  Age = c(45, 55, 60, 50, 65),
  SystolicBP = c(120, 130, 140, 125, 150),
  DiastolicBP = c(80, 85, 90, 82, 95),
  Cholesterol = c(180, 200, 220, 190, 240)
)

# View the data
print(data)

# 3D scatter plot: Blood Pressure vs Age vs Cholesterol Level
plot_ly(data, x = ~SystolicBP, y = ~Age, z = ~Cholesterol, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Cholesterol, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Blood Pressure vs Age vs Cholesterol Level',
         scene = list(xaxis = list(title = 'Systolic Blood Pressure (mmHg)'),
                      yaxis = list(title = 'Age (years)'),
                      zaxis = list(title = 'Cholesterol Level (mg/dL)')))

# Calculate correlations
correlations <- cor(data[, c('Age', 'SystolicBP', 'DiastolicBP', 'Cholesterol')])
print(correlations)

# Interpolating the data for surface plot
interp_data <- interp(data$Age, data$SystolicBP, data$Cholesterol)

# 3D surface plot
plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(title = '3D Surface Plot: Cholesterol Levels with varying Age and Blood Pressure',
         scene = list(xaxis = list(title = 'Age (years)'),
                      yaxis = list(title = 'Systolic Blood Pressure (mmHg)'),
                      zaxis = list(title = 'Cholesterol Level (mg/dL)')))

# 3D scatter plot: Age vs Cholesterol vs Blood Pressure
plot_ly(data, x = ~Age, y = ~Cholesterol, z = ~SystolicBP, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Cholesterol, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Age vs Cholesterol vs Blood Pressure',
         scene = list(xaxis = list(title = 'Age (years)'),
                      yaxis = list(title = 'Cholesterol Level (mg/dL)'),
                      zaxis = list(title = 'Systolic Blood Pressure (mmHg)')))

# 3D scatter plot: Blood Pressure vs Cholesterol vs Age
plot_ly(data, x = ~SystolicBP, y = ~Cholesterol, z = ~Age, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Cholesterol, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Blood Pressure vs Cholesterol vs Age',
         scene = list(xaxis = list(title = 'Systolic Blood Pressure (mmHg)'),
                      yaxis = list(title = 'Cholesterol Level (mg/dL)'),
                      zaxis = list(title = 'Age (years)')))
