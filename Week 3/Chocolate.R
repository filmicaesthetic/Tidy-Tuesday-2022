##########
# Analysing chocolate ratings by characteristics
##########

library(tidyverse)
library(ggridges)
library(extrafont)
library(geomtextpath)
library(gghalves)

# load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# select characteristics and ratings
characteristics <- chocolate %>% select(most_memorable_characteristics, rating)

# split into single characteristics
characteristics_single <- characteristics %>% 
  mutate(most_memorable_characteristics = strsplit(as.character(most_memorable_characteristics), ", ")) %>% 
  unnest(most_memorable_characteristics)

# prepare the data for visualisation
characteristics_single <- characteristics_single %>% 
  group_by(most_memorable_characteristics) %>% 
  mutate(n = n(), 
         avg = mean(rating), 
         rating_diff = rating - mean(characteristics_single$rating)) %>% 
  arrange(avg) %>% 
  filter(n > 60)

# create list of characteristics for ordering factor
uniq <- unique(characteristics_single$most_memorable_characteristics)

# convert to factor
characteristics_single$most_memorable_characteristics <- as.factor(characteristics_single$most_memorable_characteristics)

# create palette
myPalette <- colorRampPalette(c("#d16f24", "#c4c91c", "#58a123"))

# ridge plot
characteristics_single %>% 
  arrange(-avg) %>%
  mutate(most = fct_relevel(most_memorable_characteristics, uniq)) %>%
  ggplot(aes(x = rating, y = most, fill = avg)) +
  geom_density_ridges(alpha = 0.6, color = "#362a21") +
  scale_fill_gradientn(colours = myPalette(100)) +
  ggtitle("What makes the perfect chocolate?", subtitle = "Rank of most memorable characteristics\nof chocolate by rating out of 5") +
  theme_ridges() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "#8c7b5f"),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(color = "#362a21", hjust = 0, family = "Octin Vintage Free"),
        axis.text.y = element_text(hjust = -0, color = "#362a21", vjust = -0.45, margin = margin(l = 10, r = -50, unit = "pt")),
        plot.background = element_rect(fill = "#ede2d1", color = "#8c7b5f"),
        panel.background = element_rect(fill = "#ede2d1"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        text = element_text(family = "Touka Maru Gothic-L", color = "#362a21")
        )

# polar half-boxplot, half-violin plot
characteristics_single %>% 
  arrange(-avg) %>%
  mutate(most = fct_relevel(most_memorable_characteristics, uniq)) %>%
  ggplot(aes(x = most, y = rating, fill = avg)) +
  geom_half_boxplot(alpha = 0.4, color = "#544336", nudge = 0.06, outlier.shape = NA, width = 0.7) +
  geom_half_violin(alpha = 0.9, color = "#544336", side = "r", nudge = 0.02, width = 0.7) +
  scale_fill_gradientn(colours = myPalette(100)) +
  labs(caption = "The chart visualises characteristics which occured more than 60 times.\nsource: Flavors of Cacao  visualisation: @filmicaesthetic") +
  ylim(1, 4) +
  ggtitle("What makes\nthe perfect chocolate?", subtitle = "Distribution of chocolate ratings (1-5)\nby most memorable characteristics.") +
  coord_curvedpolar() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "#cfbfa5"),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(margin = margin(t=6, b=10, unit = "pt"),size = 8, hjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t=12, b=5, unit = "pt"), color = "#362a21", hjust = 0.5, face = "bold", family = "Octin Vintage Free", size = 30),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12, hjust = 0.5, color = "#362a21", vjust = 0, margin = margin(l = 10, r = -50, unit = "pt")),
        axis.text.y = element_text(size = 10),
        plot.background = element_rect(fill = "#ede2d1", color = "#8c7b5f"),
        panel.background = element_rect(fill = "#ede2d1"),
        axis.title = element_blank(),
        text = element_text(family = "Touka Maru Gothic-L", color = "#362a21")
  )

ggsave("chocolate_svb.png", width = 20, height = 25, units = "cm")
