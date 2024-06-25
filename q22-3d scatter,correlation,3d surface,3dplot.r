# Load necessary libraries
library(plotly)
library(akima)

# Create the dataset
data <- data.frame(
  Product = c('A', 'B', 'C', 'D', 'E'),
  Price = c(50, 70, 60, 45, 55),
  UnitsSold = c(1000, 800, 1200, 1500, 900),
  CustomerRatings = c(4.2, 4.0, 4.5, 3.8, 4.3)
)

# View the data
print(data)

# 3D scatter plot: Units Sold vs Customer Ratings vs Price
plot_ly(data, x = ~UnitsSold, y = ~CustomerRatings, z = ~Price, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerRatings, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Units Sold vs Customer Ratings vs Product Price',
         scene = list(xaxis = list(title = 'Units Sold'),
                      yaxis = list(title = 'Customer Ratings (out of 5)'),
                      zaxis = list(title = 'Price ($)')))

# Calculate correlations
correlations <- cor(data[, c('Price', 'UnitsSold', 'CustomerRatings')])
print(correlations)

# Interpolating the data
interp_data <- interp(data$UnitsSold, data$Price, data$CustomerRatings)

# 3D surface plot
plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = 'surface') %>%
  layout(title = '3D Surface Plot: Customer Ratings with varying Units Sold and Product Price',
         scene = list(xaxis = list(title = 'Units Sold'),
                      yaxis = list(title = 'Price ($)'),
                      zaxis = list(title = 'Customer Ratings (out of 5)')))

# 3D scatter plot: Customer Ratings vs Units Sold vs Price
plot_ly(data, x = ~UnitsSold, y = ~CustomerRatings, z = ~Price, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerRatings, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Units Sold vs Customer Ratings vs Price',
         scene = list(xaxis = list(title = 'Units Sold'),
                      yaxis = list(title = 'Customer Ratings (out of 5)'),
                      zaxis = list(title = 'Price ($)')))

# 3D scatter plot: Customer Ratings vs Price vs Units Sold
plot_ly(data, x = ~Price, y = ~CustomerRatings, z = ~UnitsSold, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~CustomerRatings, colorscale = 'Viridis')) %>%
  layout(title = '3D Scatter Plot: Price vs Customer Ratings vs Units Sold',
         scene = list(xaxis = list(title = 'Price ($)'),
                      yaxis = list(title = 'Customer Ratings (out of 5)'),
                      zaxis = list(title = 'Units Sold')))
