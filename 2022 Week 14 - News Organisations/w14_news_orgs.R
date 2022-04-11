##
## Tidy Tuesday: Week 14 2022
## News Organisations
##

library(dplyr)
library(ggplot2)
library(tidyr)
library(stopwords)
library(formattable)
library(forcats)
library(showtext)

'%!in%' <- function(x,y)!('%in%'(x,y))

# import google font
font_add_google("Rubik", "Rubik")
showtext_auto()

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 14)

news_orgs <- tuesdata$news_orgs

# explore the data

# look at unique revenue streams over time?

# split revenue streams column into multiple rows
revenue_split <- news_orgs %>%
  filter(revenue_streams != "Haven’t generated revenue yet, but intend to do so") %>%
  select(publication_name, revenue_streams) %>%
  mutate(revenue_streams = strsplit(revenue_streams, ", "),
  ) %>%
  unnest(revenue_streams)

# remove punctuation
news_orgs_nopunc <- news_orgs %>% mutate(real_world_impacts = gsub('[^A-Za-z0-9 ]', "", trimws(real_world_impacts)),
                                         summary = gsub('[^A-Za-z0-9 ]', "", trimws(summary)),
                                         real_world_impacts_count = as.numeric(is.na(real_world_impacts) == FALSE),
                                         summary_count = as.numeric(is.na(news_orgs$summary) == FALSE))

# count of occurrences in impact statement
impact_words <- news_orgs_nopunc %>%
  select(publication_name, real_world_impacts, real_world_impacts_count) %>%
  mutate(impact_count = sum(news_orgs_nopunc$real_world_impacts_count, na.rm = TRUE)) %>%
  mutate(words = strsplit(real_world_impacts, " ")) %>%
  unnest(words)

impact_words <- impact_words %>% 
  mutate(words = tolower(words)) %>%
  group_by(words) %>%
  summarise(impact_n = n(),
            impact_count = mean(impact_count, na.rm = TRUE)) %>%
  filter(words %!in% data_stopwords_nltk$en) %>%
  arrange(-impact_n)

# count of occurrences in summary statement
summary_words <- news_orgs_nopunc %>%
  select(publication_name, summary, summary_count) %>%
  mutate(summary_count = sum(news_orgs_nopunc$summary_count, na.rm = TRUE)) %>%
  mutate(words = strsplit(summary, " ")) %>%
  unnest(words)

summary_words <- summary_words %>% 
  mutate(words = tolower(words)) %>%
  group_by(words) %>%
  summarise(summary_n = n(),
            summary_count = mean(summary_count, na.rm = TRUE)) %>%
  filter(words %!in% data_stopwords_nltk$en) %>%
  arrange(-summary_n)

# totals by word
words <- merge(impact_words, summary_words, by = "words") %>% 
  mutate(#total = impact_n + summary_n,
         summary_perc = summary_n / summary_count, #summary_n / total,
         impact_perc = impact_n / impact_count,  #impact_n / total,
         max_perc = pmax(summary_perc, impact_perc, na.rm = TRUE)
         )

word_table <- words %>% 
  filter(summary_n > 30, impact_n > 15, length(words) > 1) %>%
  arrange(-max_perc)

word_table <- word_table %>% rowwise() %>% mutate(mean = mean(c(summary_perc, impact_perc)))

# set colours for plt
bg_color <- "#f0ece6"
dk_color <- "#575149"

# plot chart
as.data.frame(word_table) %>%
  filter(is.na(words) == FALSE, words != "") %>%
  mutate(words = fct_reorder(as.factor(words), impact_perc),
         max_type = summary_perc > impact_perc) %>%
  ggplot(aes(x = words)) +
  geom_segment(aes(y=summary_perc, yend=impact_perc, x=words, xend=words, color = max_type)) +
  annotate(geom = "rect", xmin = 2, xmax = 20, ymin = 0.5, ymax = 0.85, fill = bg_color, color = "#d9d2c7") +
  annotate(geom = "text", label = "Business vs. Impact", hjust = 0.5, x = 11, y = 0.8, family = "Rubik", size = 8, color = dk_color, fontface = 3) +
  annotate(geom = "text", 
           label = "Analysing the words used within a news organisations'\nbusiness summary vs. statement about their real-world impact.\n\nThe chart highlights words more frequently used within\nreal-world impact statements as a way to observe the\ndifferences in the choice of language.", hjust = 0.5, x = 11, y = 0.75, family = "Rubik", size = 4, color = dk_color, fontface = 1, vjust = 1) +
  annotate(geom = "point", shape = 1, x = 5, y = 0.8, stroke = 2, size = 3, color = "#bfb6aa") +
  annotate(geom = "point", shape = 18, x = 17, y = 0.8, size = 4, color = "#4f8226") +
  geom_point(aes(y = summary_perc), colour = "#bfb6aa", shape = 1, stroke = 2, size = 3) +
  geom_point(aes(y = impact_perc), colour = "#4f8226", shape = 18, size = 4, alpha = 0.8) +
  scale_color_manual(values = c("#4f8226", "#bfb6aa"), labels = c("Impact", "Business")) +
  scale_y_continuous(labels = scales::percent, n.breaks = 8) +
  labs(colour = "TYPE", y = "Percent of entries including word", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_fixed(ratio = 20 / 1) +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        text = element_text(family = "Rubik", color = dk_color),
        panel.background = element_rect(color = bg_color, fill = bg_color),
        plot.background = element_rect(color = bg_color, fill = bg_color),
        panel.grid = element_line(color = "#d9d2c7"))

ggsave("2022 Week 14 - News Organisations/BusinessImpact.png", width = 2.5, height = 2, dpi = 300)
