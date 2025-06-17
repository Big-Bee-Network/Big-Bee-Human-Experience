# Install necessary packages if you haven't
# install.packages("readr")
# install.packages("dplyr")
# install.packages("syuzhet")
# install.packages("ggplot2")
#install.packages("tidytext")


library(readr)
library(dplyr)
library(syuzhet)
library(ggplot2)
library(tidytext)


# Load the TSV
df <- read_tsv("survey.tsv", col_names = TRUE)
colnames(df)

####################View sentiment counts of combined text###################
#combine
df_clean <- df %>%
  mutate(
    combined_text = paste(Q3, Q4, Q5, Q7, sep = " "),
    combined_text = gsub("NA", "", combined_text) # remove literal "NA"
  )

# Apply sentiment analysis
df_clean <- df_clean %>%
  mutate(
    sentiment_score = get_sentiment(combined_text, method = "afinn"),
    sentiment_label = case_when(
      sentiment_score > 0 ~ "positive",
      sentiment_score < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )

sentiment_counts <- df_clean %>%
  count(sentiment_label)

print(sentiment_counts)

# Plot sentiment distribution
ggplot(sentiment_counts, aes(x = sentiment_label, y = n, fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Distribution of Survey Responses",
       x = "Sentiment",
       y = "Number of Responses")

# Example quotes for each sentiment
example_quotes <- df_clean %>%
  group_by(sentiment_label) %>%
  summarize(example = first(combined_text)) %>%
  ungroup()

print(example_quotes)

#################Count occurrences of each role##################
role_counts <- df_clean %>%
  count(Role)

print(role_counts)

# Plot with larger font sizes
ggplot(role_counts, aes(x = reorder(Role, n), y = n, fill = Role)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Distribution of Survey Responses by Role",
    x = "Role",
    y = "Number of Responses"
  ) +
  theme_minimal(base_size = 16)  # Set base font size for all elements



##########Q3###################
# Tokenize into bigrams
bigrams_q3 <- df_clean %>%
  select(Q3) %>%
  unnest_tokens(bigram, Q3, token = "ngrams", n = 5)

# Group relevant bigrams
bigram_counts_q3 <- bigram_counts_q3 %>%
  mutate(
    bigram = case_when(
      str_detect(bigram, "\\bcollections\\b|\\bmuseum\\b|\\bmuseums\\b") ~ "phrases including 'collections/museums'",
      str_detect(bigram, "\\bdigitization\\b|\\bdigitizing\\b") ~ "phrases including 'digitization'",
      str_detect(bigram, "\\bresearch\\b") ~ "phrases including 'research'",
      str_detect(bigram, "\\bbee\\b.*\\bresearch\\b|\\bresearch\\b.*\\bbee\\b") ~ "phrases including 'bee research'",
      str_detect(bigram, "\\bstudent\\b|\\bstudents\\b") ~ "phrases including 'student(s)'",
      str_detect(bigram, "\\bphoto\\b|\\bphotos\\b|\\bimaging\\b|\\b3d\\b|\\bphotogrammetry\\b") ~ "phrases including 'imaging/3D'",
      str_detect(bigram, "\\bpersonal\\b|\\bexperience\\b|\\bfelt\\b|\\bexcited\\b|\\benjoyed\\b|\\bpart\\b|\\bopportunity\\b|\\blearned\\b|\\blearning\\b") ~ "phrases including 'personal experience'",
      TRUE ~ bigram
    )
  ) %>%
  group_by(bigram) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

# View the full table
print(bigram_counts_q3, n = Inf)

# View the table
print(bigram_table, n = Inf)

bigram_counts_q3 %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%  # ensure exactly 4 rows
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 4 Phrases in Q3 (Grouped)",
    x = "Phrase",
    y = "Count"
  ) +
  theme_minimal(base_size = 16)



##########Q4###################

bigrams_q4 <- df_clean %>%
  select(Q4) %>%
  unnest_tokens(bigram, Q4, token = "ngrams", n = 2)

# Count bigram frequency
bigram_counts_q4 <- bigrams_q4 %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

# Apply grouping
bigram_counts_q4 <- bigram_counts_q4 %>%
  mutate(
    bigram = case_when(
      str_detect(bigram, "\\bcollections\\b|\\bmuseum\\b|\\bmuseums\\b|\\bspecimen\\b|\\bspecimens\\b") ~ "phrases including 'collections/museums/specimens'",
      str_detect(bigram, "\\bresearch\\b|\\bpublications\\b|\\bpublication\\b|\\bdata\\b|\\banalysis\\b") ~ "phrases including 'research/publications/data'",
      str_detect(bigram, "\\bimaging\\b|\\b3d\\b|\\bphoto\\b|\\bphotos\\b|\\bphotogrammetry\\b|\\bcamera\\b|\\bstacked\\b|\\bmacropod\\b") ~ "phrases including 'imaging/3D'",
      str_detect(bigram, "\\bstudent\\b|\\bstudents\\b|\\bundergaduate\\b|\\bundergraduates\\b|\\bmentoring\\b|\\btraining\\b|\\binterns\\b") ~ "phrases including 'students/mentoring/training'",
      str_detect(bigram, "\\boutreach\\b|\\bpromote\\b|\\bpublic\\b|\\bcommunity\\b") ~ "phrases including 'outreach/public engagement'",
      str_detect(bigram, "\\bcollaborate\\b|\\bcollaboration\\b|\\binternational\\b") ~ "phrases including 'collaboration/international'",
      str_detect(bigram, "\\bpersonal\\b|\\bexperience\\b|\\bopportunity\\b|\\bexcited\\b|\\blearned\\b|\\blearning\\b|\\bproud\\b") ~ "phrases including 'personal growth/experience'",
      TRUE ~ bigram
    )
  ) %>%
  group_by(bigram) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

# View full grouped table
print(bigram_counts_q4, n = Inf)

bigram_counts_q4 %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%  # ensure exactly 4 rows
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 4 Phrases in Q3 (Grouped)",
    x = "Phrase",
    y = "Count"
  ) +
  theme_minimal(base_size = 16)


##########Q5###################

# Tokenize Q5 into bigrams
bigrams_q5 <- df_clean %>%
  select(Q5) %>%
  unnest_tokens(bigram, Q5, token = "ngrams", n = 2)

# Count bigram frequency
bigram_counts_q5 <- bigrams_q5 %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

# Apply grouping logic
bigram_counts_q5 <- bigram_counts_q5 %>%
  mutate(
    bigram = case_when(
      str_detect(bigram, "\\bcollections\\b|\\bmuseum\\b|\\bspecimen\\b|\\bspecimens\\b") ~ "phrases including 'collections/museums/specimens'",
      str_detect(bigram, "\\bresearch\\b|\\bdata\\b|\\bdatabase\\b|\\bbiodiversity\\b|\\bpublications\\b|\\bpublication\\b") ~ "phrases including 'research/data/publications'",
      str_detect(bigram, "\\bimaging\\b|\\b3d\\b|\\bphoto\\b|\\bphotos\\b|\\bcamera\\b|\\bphotogrammetry\\b|\\bmacropod\\b") ~ "phrases including 'imaging/3D'",
      str_detect(bigram, "\\bstudent\\b|\\bstudents\\b|\\bundergrad\\b|\\bundergraduate\\b|\\binterns\\b|\\bmentoring\\b|\\btraining\\b") ~ "phrases including 'students/mentoring/training'",
      str_detect(bigram, "\\bcollaborate\\b|\\bcollaboration\\b|\\bcommunication\\b|\\bcoordination\\b|\\bteamwork\\b|\\bteam\\b|\\bteams\\b|\\bteaming\\b|\\bmeetings\\b|\\bslack\\b") ~ "phrases including 'collaboration/communication'",
      str_detect(bigram, "\\boutreach\\b|\\bpublic\\b|\\bcommunity\\b") ~ "phrases including 'outreach/public engagement'",
      str_detect(bigram, "\\bpersonal\\b|\\bexperience\\b|\\bopportunity\\b|\\bexcited\\b|\\blearned\\b|\\blearning\\b|\\bproud\\b") ~ "phrases including 'personal growth/experience'",
      TRUE ~ bigram
    )
  ) %>%
  group_by(bigram) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

# View full grouped table
print(bigram_counts_q5, n = Inf)

bigram_counts_q5 %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%  # ensure exactly 4 rows
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 4 Phrases in Q3 (Grouped)",
    x = "Phrase",
    y = "Count"
  ) +
  theme_minimal(base_size = 16)

##########Q6###################
bigrams_q6 <- df_clean %>%
  select(Q6) %>%
  unnest_tokens(bigram, Q6, token = "ngrams", n = 2)

# Count bigram frequency
bigram_counts_q6 <- bigrams_q6 %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

# Apply grouping logic
bigram_counts_q6 <- bigram_counts_q6 %>%
  mutate(
    bigram = case_when(
      str_detect(bigram, "\\bcollections\\b|\\bmuseum\\b|\\bspecimen\\b|\\bspecimens\\b") ~ "phrases including 'collections/museums/specimens'",
      str_detect(bigram, "\\bresearch\\b|\\bdata\\b|\\bdatabase\\b|\\banalysis\\b|\\bpublications\\b|\\bpublication\\b") ~ "phrases including 'research/data/publications'",
      str_detect(bigram, "\\bimaging\\b|\\b3d\\b|\\bphoto\\b|\\bphotos\\b|\\bcamera\\b|\\bphotogrammetry\\b|\\bmacropod\\b|\\bmetashape\\b") ~ "phrases including 'imaging/3D'",
      str_detect(bigram, "\\bstudent\\b|\\bstudents\\b|\\bundergrad\\b|\\bundergraduate\\b|\\bmentoring\\b|\\btraining\\b|\\btechnician\\b|\\btechnicians\\b") ~ "phrases including 'students/mentoring/training'",
      str_detect(bigram, "\\bcollaborate\\b|\\bcollaboration\\b|\\bcommunication\\b|\\bcoordination\\b|\\bteamwork\\b|\\bmeetings\\b|\\bslack\\b|\\bzoom\\b") ~ "phrases including 'collaboration/communication'",
      str_detect(bigram, "\\boutreach\\b|\\bpublic\\b|\\bcommunity\\b|\\bevent\\b|\\bevents\\b") ~ "phrases including 'outreach/public engagement'",
      str_detect(bigram, "\\bpersonal\\b|\\bexperience\\b|\\bopportunity\\b|\\bexcited\\b|\\blearned\\b|\\blearning\\b|\\bproud\\b") ~ "phrases including 'personal growth/experience'",
      TRUE ~ bigram
    )
  ) %>%
  group_by(bigram) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

# View full grouped table
print(bigram_counts_q6, n = Inf)

bigram_counts_q6 %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%  # ensure exactly 4 rows
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 4 Phrases in Q3 (Grouped)",
    x = "Phrase",
    y = "Count"
  ) +
  theme_minimal(base_size = 16)

# Prepare combined Q6 text for each response
df_clean_q6 <- df_clean %>%
  mutate(
    Q6_text = ifelse(is.na(Q6), "", Q6)
  )

# Compute sentiment
df_clean_q6 <- df_clean %>%
  mutate(
    Q6_text = ifelse(is.na(Q6), "", Q6)
  )

# Get NRC sentiment
nrc_sentiments <- get_nrc_sentiment(df_clean_q6$Q6_text)

# Sum up positive and negative for each row
df_clean_q6 <- df_clean_q6 %>%
  bind_cols(
    nrc_sentiments %>% select(positive, negative)
  ) %>%
  mutate(
    sentiment_label = case_when(
      positive > negative ~ "positive",
      negative > positive ~ "negative",
      TRUE ~ "neutral"
    )
  )

# Plot
df_clean_q6 %>%
  count(sentiment_label) %>%
  ggplot(aes(x = sentiment_label, y = n, fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Sentiment Distribution of Q6 (NRC Lexicon)",
    x = "Sentiment",
    y = "Number of Responses"
  ) +
  theme_minimal(base_size = 16)

##########Q7###################

# Tokenize Q5 into bigrams
bigrams_q7 <- df_clean %>%
  select(Q5) %>%
  unnest_tokens(bigram, Q5, token = "ngrams", n = 2)

# Count bigram frequency
bigram_counts_q7 <- bigrams_q7 %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

bigram_counts_q7 <- bigram_counts_q7 %>%
  mutate(
    bigram = case_when(
      str_detect(bigram, "\\bdata\\b") ~ "phrases including 'data'",
      str_detect(bigram, "\\bcollection\\b|\\bcollections\\b") ~ "phrases including 'collection'",
      str_detect(bigram, "\\bresearch\\b") ~ "phrases including 'research'",
      TRUE ~ bigram
    )
  ) %>%
  group_by(bigram) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

bigram_counts_q7 %>%
  arrange(desc(n)) %>%
  slice_head(n = 4) %>%  # ensure exactly 4 rows
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 4 Phrases in Q3 (Grouped)",
    x = "Phrase",
    y = "Count"
  ) +
  theme_minimal(base_size = 16)
