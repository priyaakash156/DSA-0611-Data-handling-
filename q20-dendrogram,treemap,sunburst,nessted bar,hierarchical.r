# Load required libraries
library(ggplot2)
library(dendextend)
library(treemap)
library(plotly)
library(ggdendro)
library(tidyverse)
library(ggraph)
library(igraph)

# Create the data frame
data <- data.frame(
  Parent_Category = c('A', 'A', 'B', 'B', 'C'),
  Subcategory = c('A1', 'A2', 'B1', 'B2', 'C1'),
  Value = c(10, 15, 20, 25, 12)
)

# 1. Create a dendrogram to visualize the hierarchical structure
# Prepare the data for hierarchical clustering
dend_data <- data %>%
  select(Parent_Category, Subcategory, Value) %>%
  pivot_wider(names_from = Subcategory, values_from = Value, values_fill = list(Value = 0)) %>%
  column_to_rownames('Parent_Category')

# Calculate the distance matrix
dend_dist <- dist(dend_data)
# Perform hierarchical clustering
dend <- as.dendrogram(hclust(dend_dist))

# Plot the dendrogram
plot(dend, main = "Hierarchical Clustering Dendrogram")

# 2. Generate a treemap of Parent Category and Subcategory based on Value
treemap(data,
        index = c("Parent_Category", "Subcategory"),
        vSize = "Value",
        title = "Treemap of Categories",
        palette = "Set3")

# 3. Plot a sunburst chart to show the distribution of values across categories
sunburst_data <- data %>%
  mutate(path = paste(Parent_Category, Subcategory, sep = "-")) %>%
  select(path, Value)

sunburst_plot <- plot_ly(
  labels = ~c(data$Parent_Category, data$Subcategory),
  parents = ~c(rep("", length(data$Parent_Category)), data$Parent_Category),
  values = ~c(rep(0, length(data$Parent_Category)), data$Value),
  type = 'sunburst'
) %>% layout(title = 'Sunburst Chart of Categories')
sunburst_plot

# 4. Create a nested bar plot of Parent Category and Subcategory
ggplot(data, aes(x = Parent_Category, y = Value, fill = Subcategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nested Bar Plot", x = "Parent Category", y = "Value")

# 5. Plot a hierarchical edge bundling plot to visualize connections between Parent Category and Subcategory
# Preparing data for hierarchical edge bundling plot
edges <- data %>%
  rename(from = Parent_Category, to = Subcategory)

# Create a graph object
g <- graph_from_data_frame(edges)

# Plot the hierarchical edge bundling
ggraph(g, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(aes(colour = from)) +
  geom_node_text(aes(label = name, filter = leaf, angle = -((-node_angle(x, y) + 90) %% 180) + 90, hjust = ifelse(x > 0, 1, 0)), size = 3) +
  theme_void() +
  labs(title = "Hierarchical Edge Bundling")
