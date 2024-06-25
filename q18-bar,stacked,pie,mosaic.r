# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data frame
data <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Variable1 = c(0, 1, 0, 1, 0),
  Variable2 = c(1, 0, 1, 1, 0),
  Variable3 = c(0, 1, 1, 0, 0)
)

# Count occurrences of 0s and 1s in Variable1
counts_var1 <- data %>%
  group_by(Variable1) %>%
  summarise(count = n())

# Create bar plot for Variable1
ggplot(counts_var1, aes(x = factor(Variable1), y = count, fill = factor(Variable1))) +
  geom_bar(stat = "identity") +
  labs(title = "Count of 0s and 1s in Variable1", x = "Value", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "lightgreen")) +
  theme_minimal()

# Reshape data for stacked bar plot
data_long <- data %>%
  gather(key = "Variable", value = "Value", -ID)

# Count occurrences of each combination
counts_stacked <- data_long %>%
  group_by(Variable, Value) %>%
  summarise(count = n())

# Create stacked bar plot
ggplot(counts_stacked, aes(x = Variable, y = count, fill = factor(Value))) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Variables", x = "Variable", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "lightgreen")) +
  theme_minimal()

# Calculate proportion of 0s and 1s in Variable2
prop_var2 <- prop.table(table(data$Variable2))
prop_var2_df <- data.frame(Value = names(prop_var2), Freq = as.numeric(prop_var2))

# Create pie chart with numbers inside
ggplot(prop_var2_df, aes(x = "", y = Freq, fill = factor(Value))) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Freq * 100, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Pie Chart of Variable2", fill = "Value") +
  theme_void()

# Load required library
library(vcd)

# Create mosaic plot
mosaicplot(table(data$Variable1, data$Variable2), main = "Mosaic Plot: Variable1 vs Variable2")

# Count frequencies of each combination of Variable1 and Variable2
freq_combinations <- data %>%
  group_by(Variable1, Variable2) %>%
  summarise(count = n())

# Create bar chart
ggplot(freq_combinations, aes(x = paste0("Var1=", Variable1, ", Var2=", Variable2), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Combinations in Variable1 and Variable2", x = "Combinations", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
