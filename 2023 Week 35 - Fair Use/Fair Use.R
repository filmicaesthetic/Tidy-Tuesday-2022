## fair use

# some code/ method borrowed from Simon Gorin: https://github.com/gorinsimon/PS4_reviews_prediction

pacman::p_load(tidyverse, tidytext, tidymodels)

tuesdata <- tidytuesdayR::tt_load(2023, week = 35)

fair_use_cases <- tuesdata$fair_use_cases |>
  mutate(case_id = row_number())

fair_use_findings <- tuesdata$fair_use_findings |>
  mutate(case_id = row_number())

findings_df <- fair_use_findings |>
  select(case_id, key_facts, issue, holding, outcome, tags)

findings_lkp <- findings_df |>
  left_join(fair_use_cases, by = "case_id") |>
  select(case_id, categories, fair_use_found)

## extract bigrams

issue_bigram <- findings_df |>
  select(case_id, issue) |>
  unnest_ngrams(bigram, issue, n = 2) |>
  separate(bigram, into = c("word_1", "word_2"), sep = " ") |>
  mutate(text_type = "issue")

key_facts_bigram <- findings_df |>
  select(case_id, key_facts) |>
  unnest_ngrams(bigram, key_facts, n = 2) |>
  separate(bigram, into = c("word_1", "word_2"), sep = " ") |>
  mutate(text_type = "key_facts")

holding_bigram <- findings_df |>
  select(case_id, holding) |>
  unnest_ngrams(bigram, holding, n = 2) |>
  separate(bigram, into = c("word_1", "word_2"), sep = " ") |>
  mutate(text_type = "holding")

# combine the bigram dfs, add outcome info and remove preliminary results
all_bigram <- rbind(issue_bigram, key_facts_bigram, holding_bigram) |>
  left_join(findings_lkp, by = "case_id")


# List of 'negation' words
negative_words <- c("no", "not", "none", "nobody", "nothing", "neither",
                    "nowhere", "never", "hardly", "scarcely", "barely",
                    "doesn't", "isn't", "wasn't", "shouldn't", "wouldn't",
                    "couldn't", "won't", "can't", "don't", "without")

all_df <- all_bigram |>
  mutate(is_preceding_negative = word_1 %in% negative_words) |>
  mutate(word = ifelse(is_preceding_negative == TRUE, paste("neg", word_2, sep = "_"), word_2)) |>
  select(-word_1, -word_2, -is_preceding_negative) |>
  filter(!word %in% stopwords::data_stopwords_nltk$en)

top_words <- all_df |>
  group_by(word) |>
  summarise(n = n()) |>
  arrange(-n) |>
  head(100)

kf_summary <- all_df |>
  group_by(fair_use_found) |>
  slice_sample(n = 20000, replace = TRUE) |>
  filter(word %in% top_words$word) |>
  group_by(word, text_type, fair_use_found) |>
  summarise(n = n()) |>
  arrange(-n) |> 
  pivot_wider(id_cols = c(word, text_type), names_from = fair_use_found, values_from = n) |>
  mutate(perc = ifelse(is.na(`TRUE`), 0,
                       ifelse(is.na(`FALSE`), 1,
                                    `TRUE` / (`TRUE` + `FALSE`))),
         word = as.factor(word)) |>
  filter(text_type == "key_facts") |>
  ungroup() |>
  select(word, perc)

kf_summary |>
  mutate(word = fct_reorder(word, -perc)) |>
  ggplot(aes(x = perc, y = word)) +
  geom_point()
