# Load required libraries
library(wordcloud)

# Create the data frame
data <- data.frame(
  Word = c("Apple", "Orange", "Banana", "Grape", "Cherry"),
  Frequency = c(15, 10, 8, 12, 5)
)

# Convert data to a format suitable for wordcloud function
wordcloud_data <- rep(data$Word, times = data$Frequency)

# Create word cloud
wordcloud(words = wordcloud_data, min.freq = 5, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Sort data by Frequency and select top 5 words
top_words <- head(data[order(-data$Frequency), ], 5)

# Create bar plot
ggplot(top_words, aes(x = reorder(Word, Frequency), y = Frequency, fill = Word)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Most Frequent Words", x = "Word", y = "Frequency") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data$percent <- data$Frequency / sum(data$Frequency) * 100

# Create pie chart with labels inside
ggplot(data, aes(x = "", y = Frequency, fill = Word, label = paste0(round(percent, 1), "%"))) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  labs(title = "Pie Chart of Word Frequencies", fill = "Word") +
  theme_void()

# Create histogram of Frequency
ggplot(data, aes(x = Frequency, fill = Word)) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = "identity") +
  labs(title = "Histogram of Frequency", x = "Frequency", y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
