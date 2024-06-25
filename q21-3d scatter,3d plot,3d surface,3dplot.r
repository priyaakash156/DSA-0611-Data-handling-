# Load necessary libraries
library(plotly)
library(akima)

# Create the dataset
data <- data.frame(
  Region = c('North', 'South', 'East', 'West', 'Central'),
  Sales = c(250, 300, 280, 270, 260),
  AdvertisingCost = c(1000, 1200, 1100, 1050, 1150),
  CustomerSatisfaction = c(8, 7, 9, 7.5, 8.5)
)

# View the data
print(data)

# 3D scatter plot
plot_ly(data, x = ~AdvertisingCost, y = ~Sales, z = ~CustomerSatisfaction, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerSatisfaction, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Advertising Cost vs Sales vs Customer Satisfaction',
         scene = list(xaxis = list(title = 'Advertising Cost ($)'),
                      yaxis = list(title = 'Sales ($ thousands)'),
                      zaxis = list(title = 'Customer Satisfaction (out of 10)')))

# Calculate correlations
correlations <- cor(data[, c('Sales', 'AdvertisingCost', 'CustomerSatisfaction')])
print(correlations)

# Interpolating the data
interp_data <- interp(data$AdvertisingCost, data$Sales, data$CustomerSatisfaction)

# 3D surface plot
plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(title = '3D Surface Plot: Customer Satisfaction with varying Sales and Advertising Cost',
         scene = list(xaxis = list(title = 'Advertising Cost ($)'),
                      yaxis = list(title = 'Sales ($ thousands)'),
                      zaxis = list(title = 'Customer Satisfaction (out of 10)')))
# 3D scatter plot: Sales vs Customer Satisfaction
plot_ly(data, x = ~Sales, y = ~CustomerSatisfaction, z = ~AdvertisingCost, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerSatisfaction, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Sales vs Customer Satisfaction vs Advertising Cost',
         scene = list(xaxis = list(title = 'Sales ($ thousands)'),
                      yaxis = list(title = 'Customer Satisfaction (out of 10)'),
                      zaxis = list(title = 'Advertising Cost ($)')))

# 3D scatter plot: Advertising Cost vs Customer Satisfaction
plot_ly(data, x = ~AdvertisingCost, y = ~CustomerSatisfaction, z = ~Sales, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerSatisfaction, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Advertising Cost vs Customer Satisfaction vs Sales',
         scene = list(xaxis = list(title = 'Advertising Cost ($)'),
                      yaxis = list(title = 'Customer Satisfaction (out of 10)'),
                      zaxis = list(title = 'Sales ($ thousands)')))
