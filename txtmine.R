library(tidyverse)
library(tidytext)

# Load the source data
wine.df <- read_csv("winemag-data-130k-v2.csv")

# Select relevant columns and remove rows with missing values
wine_subset <- wine.df %>%
  select(description, points)
wine_clean <- na.omit(wine_subset)

# TF-IDF ANALYSIS
# Goal: Use TF-IDF to find words that are uniquely important to each quality tier

# Create quality tiers based on the point distribution quartiles (1st Qu: 86, 3rd Qu: 91)
wine_tiered <- wine_clean %>%
  mutate(quality_tier = case_when(
    points <= 86          ~ "Standard",
    points >= 91          ~ "Elite",
    TRUE                  ~ "Premium"
  ))

# Tokenize descriptions into words and remove common stop words
wine_words <- wine_tiered %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

# Count word occurrences within each quality tier
word_counts <- wine_words %>%
  count(quality_tier, word, sort = TRUE)

# Calculate TF-IDF scores from the word counts
tier_tf_idf <- word_counts %>%
  bind_tf_idf(term = word, document = quality_tier, n = n)

# Prepare data for plotting by selecting the top 15 TF-IDF words per tier
top_tf_idf_words <- tier_tf_idf %>%
  group_by(quality_tier) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(quality_tier = factor(quality_tier, levels = c("Standard", "Premium", "Elite")))

# Generate Plot For TF-IDF Results
ggplot(top_tf_idf_words, aes(tf_idf, fct_reorder(word, tf_idf), fill = quality_tier)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~quality_tier, scales = "free") +
  labs(
    title = "The Distinct Vocabulary of Wine Quality (TF-IDF)",
    x = "TF-IDF Score",
    y = "Characteristic Words"
  )


# WORD FREQUENCY ANALYSIS
# Goal: Find the most divisive words by comparing their frequency ratio between Elite and Standard tiers

# Reuse the tokenized `wine_words` object
# First, get total word counts per tier for normalization
total_words_per_tier <- wine_words %>%
  count(quality_tier, name = "total_words")

# Calculate word frequencies per 10,000 words
word_frequencies <- word_counts %>%
  left_join(total_words_per_tier, by = "quality_tier") %>%
  mutate(frequency = (n / total_words) * 10000)

# Get total counts to filter out rare words that could skew ratios
word_total_counts <- word_frequencies %>%
  group_by(word) %>%
  summarise(total_n = sum(n))

# Join total counts and filter for words appearing at least 50 times
temp_ratios <- word_frequencies %>%
  left_join(word_total_counts, by = "word") %>%
  filter(total_n >= 50)

# Pivot the data to have a frequency column for each tier
pivoted_ratios <- temp_ratios %>%
  select(word, quality_tier, frequency) %>%
  pivot_wider(names_from = quality_tier, values_from = frequency)

# Replace NAs with a small value to prevent division-by-zero errors
pivoted_ratios[is.na(pivoted_ratios)] <- 1e-7

# Calculate the final Elite-to-Standard frequency ratio
word_ratios <- pivoted_ratios %>%
  mutate(ratio = Elite / Standard)

# Prepare data for the plot by selecting the top 10 most divisive words
top_elite_words <- word_ratios %>% arrange(desc(ratio)) %>% head(10)
top_standard_words <- word_ratios %>% arrange(ratio) %>% head(10)
key_words_to_plot <- c(top_elite_words$word, top_standard_words$word)

# Create a custom factor order for the plot based on the ratio
word_order <- word_ratios %>%
  filter(word %in% key_words_to_plot) %>%
  arrange(desc(ratio)) %>%
  pull(word)

# Filter the main frequency data and apply the custom word and tier ordering
plot_data <- word_frequencies %>%
  filter(word %in% key_words_to_plot) %>%
  mutate(word = factor(word, levels = word_order),
         quality_tier = factor(quality_tier, levels = c("Standard", "Premium", "Elite")))

# Generate Plot For Word Frequency (Including Years)
ggplot(plot_data, aes(x = quality_tier, y = frequency, fill = quality_tier)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word, scales = "free_y", ncol = 5) +
  labs(
    title = "The Vocabulary of Quality (Including Years)",
    x = "Wine Quality Tier",
    y = "Frequency (Occurrences per 10,000 Words)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"))


# WORD FREQUENCY ANALYSIS (Excluding Years)
# Because the previous Elite wines were dominated by years. This section refines the analysis by excluding numbers

# Filter the previously calculated ratios to remove numeric-only words
word_ratios_no_numbers <- word_ratios %>%
  filter(!str_detect(word, "^[0-9]+$"))

# Repeat the process of selecting top words and preparing plot data from the filtered set
top_elite_desc_words <- word_ratios_no_numbers %>% arrange(desc(ratio)) %>% head(10)
top_standard_desc_words <- word_ratios_no_numbers %>% arrange(ratio) %>% head(10)
key_desc_words_to_plot <- c(top_elite_desc_words$word, top_standard_desc_words$word)

word_order_desc <- word_ratios_no_numbers %>%
  filter(word %in% key_desc_words_to_plot) %>%
  arrange(desc(ratio)) %>%
  pull(word)

plot_data_desc <- word_frequencies %>%
  filter(word %in% key_desc_words_to_plot) %>%
  mutate(word = factor(word, levels = word_order_desc),
         quality_tier = factor(quality_tier, levels = c("Standard", "Premium", "Elite")))

# Generate Plot For Word Frequency (Excluding Years)
ggplot(plot_data_desc, aes(x = quality_tier, y = frequency, fill = quality_tier)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word, scales = "free_y", ncol = 5) +
  labs(
    title = "The Vocabulary of Quality (Excluding Years)",
    x = "Wine Quality Tier",
    y = "Frequency (Occurrences per 10,000 Words)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"))
