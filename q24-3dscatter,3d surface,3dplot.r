# Load necessary libraries
library(plotly)
library(akima)

# Create the dataset
data <- data.frame(
  Employee = c('A', 'B', 'C', 'D', 'E'),
  HoursWorked = c(40, 35, 45, 38, 42),
  TasksCompleted = c(10, 8, 12, 9, 11),
  Efficiency = c(80, 75, 85, 78, 82)
)

# View the data
print(data)

# 3D scatter plot: Tasks Completed vs Efficiency vs Hours Worked
plot_ly(data, x = ~TasksCompleted, y = ~Efficiency, z = ~HoursWorked, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Efficiency, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Tasks Completed vs Efficiency vs Hours Worked',
         scene = list(xaxis = list(title = 'Tasks Completed'),
                      yaxis = list(title = 'Efficiency (%)'),
                      zaxis = list(title = 'Hours Worked')))

# Calculate correlations
correlations <- cor(data[, c('HoursWorked', 'TasksCompleted', 'Efficiency')])
print(correlations)

# Interpolating the data for surface plot
interp_data <- interp(data$TasksCompleted, data$HoursWorked, data$Efficiency)

# 3D surface plot
plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(title = '3D Surface Plot: Efficiency with varying Tasks Completed and Hours Worked',
         scene = list(xaxis = list(title = 'Tasks Completed'),
                      yaxis = list(title = 'Hours Worked'),
                      zaxis = list(title = 'Efficiency (%)')))

# 3D scatter plot: Efficiency vs Tasks Completed vs Hours Worked
plot_ly(data, x = ~TasksCompleted, y = ~Efficiency, z = ~HoursWorked, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Efficiency, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Tasks Completed vs Efficiency vs Hours Worked',
         scene = list(xaxis = list(title = 'Tasks Completed'),
                      yaxis = list(title = 'Efficiency (%)'),
                      zaxis = list(title = 'Hours Worked')))

# 3D scatter plot: Efficiency vs Hours Worked vs Tasks Completed
plot_ly(data, x = ~HoursWorked, y = ~Efficiency, z = ~TasksCompleted, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Efficiency, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Hours Worked vs Efficiency vs Tasks Completed',
         scene = list(xaxis = list(title = 'Hours Worked'),
                      yaxis = list(title = 'Efficiency (%)'),
                      zaxis = list(title = 'Tasks Completed')))
