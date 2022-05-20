## 
## Tidy Tuesday Week 20: Eurovision
## Data: Eurovision & Data World
## 

## load packages
library(ggnetwork)
library(showtext)
library(raster)
library(grid)
library(gridExtra)
library(tidyverse)

# import Rubik font from Google
font_add_google("Rubik", "Rubik")
showtext_auto()

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 20)
eurovision <- tuesdata$eurovision
votes <- tuesdata$`eurovision-votes`

# tidy country names, remove those not to be used, and those with fewer than 10 occurrences
vote_tidy <- votes %>%
  mutate(to_country = gsub("The Netherlands", "Netherlands", to_country),
         from_country = gsub("The Netherlands", "Netherlands", from_country)) %>%
  filter(semi_final == "f", jury_or_televoting == "J", is.na(duplicate) == TRUE, !(to_country %in% c()), !(from_country %in% c())) %>%
  mutate(both_country = paste0(pmin(to_country, from_country), pmax(to_country, from_country))) %>%
  group_by(both_country) %>%
  mutate(n = n(),
            points = mean(points)) %>%
  mutate(points = ifelse(n >= 10, points, 0)) %>%
  dplyr::select(to_country, from_country, both_country, points) %>%
  distinct()

vote_tidy <- vote_tidy %>%
  filter(from_country %in% vote_tidy$to_country)

# import country long/lat data
countries <- read_csv("2022 Week 20 - Eurovision/data/countries.csv") %>%
  mutate(name = gsub("Bosnia and Herzegovina", "Bosnia & Herzegovina", # edit to match eurovision data
                     gsub("Macedonia [FYROM]", "F.Y.R. Macedonia", name)),
         longitude = ifelse(name == "Russia", 47.576927, # move russia into frame
                            ifelse(name == "Australia", 60, longitude)), # move autralia closer
         latitude = ifelse(name == "Russia", 58.595272, 
                           ifelse(name == "Azerbaijan", 40.91232, # move azerbaijan to not cover Armenia
                                  ifelse(name == "Australia", 15, latitude))))

# filter out any countries with no long/lat data
eurovis_countries <- data.frame(name = c(unique(vote_tidy$to_country))) %>%
  inner_join(countries, by = "name")

# filter out countries that only appear in one column
vote_filt <- vote_tidy %>%
  filter(from_country %in% eurovis_countries$name & to_country %in% eurovis_countries$name) 

# pivot data into matrix format
vote_mat <- vote_filt %>%
  select(-both_country) %>%
  pivot_wider(id_cols = to_country, names_from = from_country, values_from = points) %>%
  column_to_rownames(var="to_country")

# replace NA with 0
vote_mat[is.na(vote_mat)] <- 0

# create matrix
vote_mat <- as.matrix(vote_mat)

# find mean points given both ways between countries 
vote_summ <- vote_filt %>%
  mutate(both_country = paste0(pmin(to_country, from_country), pmax(to_country, from_country))) %>%
  group_by(both_country) %>%
  summarise(points = mean(points, na.rm = TRUE),
            from_country = first(from_country),
            to_country = first(to_country)) %>%
  dplyr::select(-both_country)

vote_summ$from_country[5] <- "Albania"
vote_summ$to_country[5] <- "Belarus"

# long/lat join df for from_country
country_join_a <- countries %>%
  dplyr::select("from_country" = name,
                "x" = longitude,
                "y" = latitude)

# long/lat join df for to_country
country_join_b <- countries %>%
  dplyr::select("to_country" = name,
                "xend" = longitude,
                "yend" = latitude)

# create df for plotting with both sets of long/lat values
plot_df <- data.frame(from_country = rownames(vote_mat)) %>%
  inner_join(vote_summ, by = "from_country") %>%
  inner_join(country_join_a, by = "from_country") %>%
  inner_join(country_join_b, by = "to_country") %>%
  dplyr::select(-to_country, country = from_country) %>%
  arrange(points)

# create top 5 section:

top_table <- vote_summ %>%
  arrange(-points) %>%
  head(10) %>%
  mutate(rank = 1:10)

# function to get Eurovision logo file
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

# save logo file
logo <- get_png("2022 Week 20 - Eurovision/img/Eurovision_generic_white.png")

# create simple world map
map <- map_data("world") %>% 
  dplyr::select(x = long, y = lat, group, id = subregion) %>%
  mutate(xend = 0,
         yend = 0)

# build plot
g <- ggplot(plot_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_polygon(data = map, aes(group = group), fill = "#231f36", colour = "#2d2942") +
  geom_edges(aes(alpha = (points ^ 2), size = (points ^ 3), color = points), curvature = -0.1) +
  geom_nodes(size = 10, color = "#1c192c", alpha = 0.8) +
  geom_nodetext(aes(label = str_wrap(toupper(country), 8)), family = "Rubik", size = 2.5, color = "white",
                fontface = "bold") +
  geom_text(x = -20, y = 70, hjust = 0, color = "white", size = 7, family = "Rubik", label = "Eurovision's Strongest International Bonds") +
  geom_text(x = -20, y = 68.7, hjust = 0, color = "white", size = 3.2, family = "Rubik", label = "Average points awarded between nations in Eurovision Grand Finals from 1975-2022") +
  geom_text(x = 50, y = 30, hjust = 0.8, color = "white", angle = 25, size = 2.5, family = "Rubik", label = "AUSTRALIA") +
  geom_text(x = 42, y = 71, hjust = 0.5, color = "white", size = 3, family = "Rubik", fontface = "bold",  label = "Top 10 country pairs by avg. points awarded") +
  geom_point(data = top_table, aes(x = 42, y = 71 - (rank / 1), xend = NULL, yend = NULL, color = points, size = (points ^ 3) + 20)) +
  geom_text(data = top_table, aes(x = 42, y = 71 - (rank / 1), xend = NULL, yend = NULL, label = round(points, 1)), size = 2, family = "Rubik", fontface = "bold", color = "#1c192c") + # points number text
  geom_text(data = top_table, aes(x = 41, y = 71 - (rank / 1), xend = NULL, yend = NULL, label = toupper(to_country)), hjust = 1, color = "white", size = 2.5) + # left country
  geom_text(data = top_table, aes(x = 43, y = 71 - (rank / 1), xend = NULL, yend = NULL, label = toupper(from_country)), hjust = 0, color = "white", size = 2.5) + # right country
  scale_color_viridis_b() +
  guides(size = "none", alpha = "none") +
  labs(color = "", caption = "Country pairings with fewer than 10 points exchanges are not shown.\ndata: Eurovision / Data.World", x = "average points awarded") +
  # ggtitle("THE POLITICS OF EUROVISION") +
  theme_blank() +
  coord_cartesian(xlim = c(-20, 50), ylim = c(30,70)) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.1, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_rect(fill = "#1c192c", color = "#1c192c"),
    panel.background = element_rect(fill = "#1c192c", color = "#1c192c"),
    text = element_text(family = "Rubik", color = "white"),
    legend.background = element_rect(fill = "#1c192c"),
    legend.margin = margin(t = -0.3, unit = "cm")
  )

# plot eurovision logo
t_1 <- ggplot(mapping = aes(x = 0:1, y = 1)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1c192c", color = "#1c192c"),
    panel.background = element_rect(fill = "#1c192c", color = "#1c192c"),
  ) +
  annotation_custom(logo, xmin = 0.02, xmax = 0.98)

# plot top description text
t_2 <- ggplot(mapping = aes(x = 0:1, y = 1)) +
  xlim(c(0, 1)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1c192c", color = "#1c192c"),
    panel.background = element_rect(fill = "#1c192c", color = "#1c192c"),
  ) +
  annotate(geom = "text", x = 0, y = 1, 
           size = 4, 
           color = "white",
           hjust = 0,
           label = str_wrap("The jury vote in the Eurovision Song Contest allows participating nations to award more points to their neighbours or political allies. It has become an accepted part of the competition, even part of its charm, but which countries have the strongest Eurovision bonds?\n\nThe visual below explores the strength of political relationships between participating nations in the Eurovision Song Contest by looking at the average points values awarded between each pair of countries since 1975.", 90), 
           family = "Rubik")

# join logo and description to create header section
title <- grid.arrange(t_1, t_2, nrow = 1, widths = c(0.35, 0.65))
# join with main plot
euro_plot <- arrangeGrob(title, g, nrow = 2, heights = c(0.15, 0.85))

ggsave("2022 Week 20 - Eurovision/outputs/eurovision_b.png", euro_plot, width = 10, height = 13)

