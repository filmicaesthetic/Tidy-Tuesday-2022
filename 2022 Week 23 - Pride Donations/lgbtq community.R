## LGBTQ Communities

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggstream)
library(showtext)

# import two datasets using 'gay community' as key with the highest point
lgbt_1 <- read.csv2("C:/Users/Daniel/Downloads/multiTimeline (3).csv", sep=",", skip = 2, comment.char="#", colClasses = "character")
lgbt_2 <- read.csv2("C:/Users/Daniel/Downloads/multiTimeline (4).csv", sep=",", skip = 2, comment.char="#", colClasses = "character")

# join two datasets
lgbt <- lgbt_1 %>%
  left_join(lgbt_2, by = c("Month", "gay.community...Worldwide.")) %>%
  mutate(Month = as.Date(Month, "%Y-%d"))

# adjust column names
colnames(lgbt) <- c("Month", "Gay", "LGBT", "LGBTQ", "LGBT+", "LGBTQ+", "Lesbian", "Bi", "Bisexual", "Trans")

# combine similar searches and reshape dataframe
lgbt_long <- lgbt %>%
  mutate('Bi & Bisexual' = as.character(as.numeric(gsub("<1", "0.5", Bi)) + as.numeric(gsub("<1", "0.5", Bisexual))),
         'LGBT & LGBT+' = as.character(as.numeric(gsub("<1", "0.5", LGBT)) + as.numeric(gsub("<1", "0.5", `LGBT+`))),
         'LGBTQ & LGBTQ+' = as.character(as.numeric(gsub("<1", "0.5", LGBTQ)) + as.numeric(gsub("<1", "0.5", `LGBTQ+`)))) %>%
  select(-Bi, -Bisexual, -LGBT, -`LGBT+`, -LGBTQ, -`LGBTQ+`) %>%
  pivot_longer(cols = -Month, names_to = "search_term", values_to = "search_vol") %>%
  mutate(search_vol = as.numeric(gsub("<1", "0.5", search_vol)),
         search_term = as.factor(search_term)) %>%
  group_by(search_term) %>%
  mutate(id = 1:n())

# create custom palette
pal <- c("#c94d4d", "#d8832b", "#dab71f", "#62913d", "#242551", "#66456e")

# import Google fonts
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Roboto", "Roboto")
showtext_auto()

# hexagon legend key glyph
draw_key_hex <- function (data, params, size) {
  # hexagon vertex coordinates 
  v <- list(x = c(0.95, 0.725, 0.275, 0.05, 0.275, 0.725), 
            y = c(0.5, 0.110288568297003, 0.110288568297003, 0.5, 0.889711431702997, 0.889711431702997))
  # hexagon grob
  polygonGrob(v$x, v$y, 
              gp = gpar(col = data$colour,
                        fill = alpha(data$fill, data$alpha)))
}

# create streamgraph plot
lgbt_long %>%
  ggplot(aes(x = id, y = search_vol)) +
  geom_stream(aes(fill = search_term), sorting = "onset", key_glyph = draw_key_hex) +
  geom_segment(x = 1, xend = max(lgbt_long$id), y = 45, yend = 45) +
  annotate(geom = "text", x = max(lgbt_long$id), y = -45, size = 2.5, label = "data: Google Trends | visualisation: @filmicaesthetic", family = "Roboto", hjust = 1, color = "dark grey") +
  geom_label(aes(x = 1, y = 45), label = "2004", hjust = 0, family = "Bebas Neue") +
  geom_label(aes(x = max(lgbt_long$id), y = 45), label = "2022", hjust = 1, family = "Bebas Neue") +
  scale_fill_manual(values = pal) +
  labs(title = "Evolution of the",
       subtitle = "LGBTQ+ Community",
       caption = str_wrap("Exploring the prominence of various LGBTQ+ communities from 2004 - 2022, by looking at Google's relative search volume for each search term + 'community' eg. 'Gay Community', 'LGBTQ+ Community'."), 900) +
  theme_minimal() +
  labs(fill = "") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Roboto", size = 11),
        plot.caption = element_text(family = "Roboto", hjust = 0.5),
        plot.subtitle = element_text(family = "Bebas Neue", size = 30),
        legend.position = "bottom",
        text = element_text(family = "Roboto"))
