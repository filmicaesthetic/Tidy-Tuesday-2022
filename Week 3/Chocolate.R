##########
# 
##########

library(tidyverse)
library(ggridges)
library(extrafont)
library(geomtextpath)
library(gghalves)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

chocolate %>% ggplot(aes(x = review_date, y = rating)) +
  geom_point()

characteristics <- chocolate %>% select(most_memorable_characteristics, rating)

characteristics_single <- characteristics %>% 
  mutate(most_memorable_characteristics = strsplit(as.character(most_memorable_characteristics), ", ")) %>% 
  unnest(most_memorable_characteristics)

characteristics_single <- characteristics_single %>% group_by(most_memorable_characteristics) %>% mutate(n = n(), avg = mean(rating), rating_diff = rating - mean(characteristics_single$rating)) %>% arrange(avg) %>% filter(n > 60)
uniq <- unique(characteristics_single$most_memorable_characteristics)
characteristics_single$most_memorable_characteristics <- as.factor(characteristics_single$most_memorable_characteristics)
characteristics_single$most_memorable_characteristics

myPalette <- colorRampPalette(c("#d16f24", "#c4c91c", "#58a123"))

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


cor_chocolate <- chocolate %>% select(specific_bean_origin_or_bar_name, most_memorable_characteristics) %>%
  mutate(most_memorable_characteristics = strsplit(as.character(most_memorable_characteristics), ", ")) %>% 
  unnest(most_memorable_characteristics) %>%
  mutate(dummy = 1)

library(data.table)
cor_x <- dcast(cor_chocolate, specific_bean_origin_or_bar_name ~ most_memorable_characteristics, value.var = "dummy")
cor_x_sel <- cor_x %>% select(-specific_bean_origin_or_bar_name)
res <- cor(cor_x_sel, use = "everything")
res_bitter <- res[,'bitter']

#library
library(igraph)

# build the graph object
network <- graph_from_adjacency_matrix(res_bitter)

# plot it
plot(network)


characteristics_single %>% 
  arrange(-avg) %>%
  mutate(labs = paste0(most_memorable_characteristics," (",n,")"), most = fct_relevel(most_memorable_characteristics, uniq)) %>%
  ggplot(aes(x = most, y = rating, fill = avg)) +
  geom_half_boxplot(alpha = 0.6, color = "#544336", nudge = 0.06, outlier.shape = NA, width = 0.7) +
  geom_half_violin(alpha = 0.6, color = "#544336", side = "r", nudge = 0.02, width = 0.7) +
  scale_fill_gradientn(colours = myPalette(100)) +
  labs(caption = "This chart only visualises most memorable characteristics\nwhich occured more than 60 times in the original dataset.\n\ndata: ") +
  ylim(1, 4) +
  ggtitle("What makes the perfect chocolate?", subtitle = "Rank of most memorable characteristics\nof chocolate by rating out of 5") +
  coord_curvedpolar() +
  theme(legend.position = "none",
        panel.grid.major = element_line(color = "#cfbfa5"),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(color = "#362a21", hjust = 0, family = "Octin Vintage Free"),
        axis.text.x = element_text(hjust = 0.5, color = "#362a21", vjust = 0, margin = margin(l = 10, r = -50, unit = "pt")),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#ede2d1", color = "#8c7b5f"),
        panel.background = element_rect(fill = "#ede2d1"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        text = element_text(family = "Touka Maru Gothic-L", color = "#362a21")
  )

