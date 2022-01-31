###############
# Checkers Chart - Board Games
# Tidy Tuesday 2022 - Week 4
# Data Source: Kaggle / Board Games Geek
###############

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(extrafont)
library(ggtext)

# import data
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

# combine ratings & details
board_games <- details %>%
  left_join(y = ratings, by = "id")

# split designer column into multiple rows
bg_split <- board_games %>%
  mutate(boardgamedesigner = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgamedesigner)), ", "),
  ) %>%
  unnest(boardgamedesigner)

# rank designers by number of games
designer_ratings <- bg_split %>% 
  group_by(boardgamedesigner) %>% 
  summarise(avg_rating = mean(average),
            count = n()) %>%
  arrange(-count) %>%
  filter(boardgamedesigner != "(Uncredited)")

# filter top 6 designers for chart
top_6_designers <- designer_ratings[1:6,]
top_6_designers <- top_6_designers %>% arrange(-count)
top_6_designers$boardgamedesigner <- as.character(top_6_designers$boardgamedesigner)

# get all data for each designer
bg_top6 <- bg_split %>% filter(boardgamedesigner %in% top_6_designers$boardgamedesigner)

# split into deciles
bg_top6_dec <- bg_top6 %>% group_by(boardgamedesigner) %>% mutate(decile_rank = ntile(average, 10)) %>% group_by(boardgamedesigner, decile_rank) %>% summarise(dec_avg = median(average))

# compare deciles to overall median
bg_top6_perc <- bg_top6_dec %>% mutate(perc = (dec_avg - overall_median) / overall_median)
bg_top6_perc$grouped <- cut(bg_top6_perc$perc, 
                            breaks = c(-Inf, -0.18, -0.12, -0.06, 0, 0.06, 0.12, 0.18, 0.24, 0.3, Inf),
                            labels = c("<-18%", "-18%:-12%", "-12%:-6%", "-6%:0%", "0%:+6%", "+6%:+12%", "+12%:+18%", "+18%:+24%", "+24%:+30%", "+30%<"))

# factor levels for reference
factors <-bg_top6_perc %>% select(grouped, perc)
factors <- factors %>% group_by(grouped) %>% summarise(level = 1)
factors$level <- seq(1:8)

# details
details_designer <- details %>%
  mutate(boardgamedesigner = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgamedesigner)), ", ")) %>% 
  unnest(boardgamedesigner) %>%
  group_by(boardgamedesigner) %>%
  summarise(count = n()) %>%
  arrange(-count)

# create cartesian checkers board
checkers <- data.frame(id = seq(1:121),
                       section = c(7, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4),
                                   rep(1,16), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16)),
                       player = c(0, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4),
                                  rep(1,10), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16), rep(1, 6)),
                       y = c(0, rep(seq(1:4), 6), rep(c(0.5, 1.5, 2.5, 3.5, 1, 2, 3, 1.5, 2.5, 2, 1, 1.5, 2, 1.5, 2, 2), 6)),
                       x = c(rep(0, 25), rep(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1.5, 1.5, 2, 2, 2.5, 3, 3.5, 4, 5), 6)),
                       level = c(9, rep(c(7, 5, 3, 1), 6), rep(c(8, 6, 4, 2, 7, 5, 3, 6, 4, 4, 5, 6, 5, 4, 3, 2), 6))
)

# combine with factors
checkers_fact <- merge(checkers, factors, by = "level", all.x = TRUE)

# calculate coordinates on polar axis
checkers_all <- checkers_fact %>% mutate(x_act = 0.5773502692 * x,
                                         angle = atan(y / x_act),
                                         angle_degree = 90 - (angle * 180 / pi),
                                         angle_degree_adj = angle_degree / 3 * 2,
                                         r = sqrt((y^2)+(x_act^2)),
                                         x_amount = section + (angle_degree_adj/60)/2*3) %>%
  arrange(player)

# adjustments
checkers_all$x_amount[1] <- 7
checkers_all <- checkers_all %>% group_by(player, level) %>% arrange(x_amount) %>% mutate(count = row_number(), player_level_count = paste(player, level, count, sep="-"))

# add player number
bg_top6_perc$player <- rep(1:6, each = 10)

# find top games
top_games <- bg_top6 %>% group_by(boardgamedesigner) %>% top_n(1, average) %>% select(boardgamedesigner, primary, yearpublished, average, thumbnail)

# basic designer stats
player_stats <- bg_top6 %>% group_by(boardgamedesigner) %>%
  summarise(min_year = min(yearpublished),
            max_year = max(yearpublished),
            min_to_max = paste(min_year,max_year, sep = "-"))

# values
player_list <- bg_top6_perc %>% group_by(player, boardgamedesigner) %>% summarise(count = n())

# combine dataframes
player_list <- merge(player_list, player_stats, by = "boardgamedesigner")
player_list <- merge(player_list, top_games, by = "boardgamedesigner")
player_list$colour <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33")
player_list <- player_list %>% 
  mutate(label = paste(paste0("<b><span style ='color: ",colour,"; font-size: 30pt;'>",toupper(boardgamedesigner),"</span></b>"),
                       paste0("<span style = 'color: black;font-size: 17pt;'>YEARS ACTIVE:",min_to_max),
                       paste0("<br>TOP RATED GAME<br><span style ='font-size: 14pt;'>",primary, "</span><br>",yearpublished," | <b>", average, "</b> / 10\n</span>"),
                       sep = "<br>"))

players <- merge(bg_top6_perc, factors, by = "grouped", all.x = TRUE)
players <- players %>% group_by(player, level) %>% mutate(count = row_number(), player_level_count = paste(player, level, count, sep = "-"), player_check = player) %>% arrange(player, level)


check_players <- merge(checkers_all, players, by=c("player_level_count", "grouped"), all.x = TRUE)

bg <- data.frame(x_amount = seq(1:6),
                 r = rep(4.5, 6))

# CHARTS

# produce main checkers chart
main <- check_players %>% ggplot() +
  geom_col(data = bg, aes(x = x_amount, y = r), width = 1, fill = "#dbd8d5", 
           position = position_nudge(x = 0.5), alpha = 0.6) +
  geom_point(aes(x = x_amount, y = r, color = as.factor(player_check)), size = 15) +
  #stat_function(fun = fun.1) +
  #scale_y_continuous(name = "diff from avg", labels = factor_labels$grouped, breaks = factor_labels$chart_value, limits = c(0, 4.5)) +
  scale_color_brewer(name = "Designer", palette="Set1", na.value="#a8a5a2", labels = player_list$label) +
  coord_polar(clip = "off")  + 
  annotate(geom = "richtext", x = 1, y = 4.8, label = player_list$label[1], family = "Bahnschrift", vjust = 0, color = "white") +
  annotate(geom = "richtext", x = 2, y = 4.8, label = player_list$label[2], family = "Bahnschrift", hjust = 0, color = "white") +
  annotate(geom = "richtext", x = 3, y = 4.8, label = player_list$label[3], family = "Bahnschrift", hjust = 0, color = "white") +
  annotate(geom = "richtext", x = 4, y = 4.8, label = player_list$label[4], family = "Bahnschrift", vjust = 1, color = "white") +
  annotate(geom = "richtext", x = 5, y = 4.8, label = player_list$label[5], family = "Bahnschrift", hjust = 1, color = "white") +
  annotate(geom = "richtext", x = 6, y = 4.8, label = player_list$label[6], family = "Bahnschrift", hjust = 1, color = "white") +
  ggtitle("<span style = 'font-size: 45pt;'>MOST PRODUCTIVE BOARD GAME DESIGNERS</span><br>Exploration of the six board game designers with the most design credits.<br>", subtitle = "<b>Richard H. Berg</b>, <b>Joseph Miranda</b> and <b>Bruno Cathala</b> each achieved higher-than-average ratings on over half of their extensive catalogues,<br>with <b>Berg</b> earning the highest individual rating: 8.6/10 for GMT Games's 2019 American Civil War title, Death Valley: Battles for the Shenandoah.<br><b>Reiner Knizia</b> is far and away the most productive designer with an impressive 329 titles, but less than 30% achieved an average rating<br>higher than the median. <b>Jim Dunnigan</b> was the first of the 6 to produce a game, releasing Jutland for Avalon Hill in 1967 (6.7/10)<br><br><span style = 'font-size: 25pt;'>MEDIAN DECILE RATINGS vs.\nOVERALL MEDIAN RATING</span>") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title.position = "plot",
        plot.margin = margin(t = 0, r = 130, b = 0, l = 130, unit = "pt"),
        plot.title = element_markdown(hjust = 0.5, vjust = 0),
        plot.subtitle = element_markdown(hjust = 0.5, vjust = 0, margin = margin(b = 20, unit = "pt")),
        legend.position = "none",
        panel.grid = element_line(color = "#edebe8"),
        legend.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 20, family = "Bahnschrift"))

# produce "How to Read Checkers Chart" plot

checkers_cart <- data.frame(x = c(10, 
                                  9, 11,
                                  8, 10, 12, 
                                  7, 9, 11, 13, 
                                  6, 8, 10, 12, 14, 
                                  7, 9, 11, 13, 
                                  8, 10, 12, 
                                  9, 11, 
                                  10),
                            y = c(-1,
                                  0, 0, 
                                  1, 1, 1, 
                                  2, 2, 2, 2, 
                                  3, 3, 3, 3, 3, 
                                  4, 4, 4, 4,
                                  5, 5, 5,
                                  6, 6, 
                                  7),
                            dummy = c(0,
                                       0, 1,
                                       0, 1, 1,
                                       0, 0, 1, 1,
                                       0, 0, 0, 0, 1,
                                       0, 0, 0, 1,
                                       0, 1, 1,
                                       0, 1,
                                       0)
                            
)

read_plot <- checkers_cart %>% ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(dummy)), size = 12) +
  scale_color_manual(values = c("#a8a5a2", "black")) +
  scale_y_continuous(name = "Variance %", labels = factor_labels$grouped, breaks = c(6, 5, 4, 3, 2, 1, 0, -1)) +
  ggtitle("HOW TO READ THE\nCHECKERS CHART", subtitle = "Every game a designer has produced\nis ranked and split evenly into 10 groups.\n\nEach dot represents one of these groups,\nwith the median rating of the group (decile)\nbeing grouped and plotted vs. the median rating\nacross all games in the dataset.\n\nThe closer to the centre, the higher the rating.") +
  geom_abline(intercept = 2.5, slope = 0) +     
  geom_text(aes(x = 16, y = 3.5, label = "MEDIAN\nRATING:\n6.45"), size = 3, family = "Bahnschrift") + 
  expand_limits(y = c(-2, 8), x = c(5, 17)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0, size = 15, margin = margin(b = 0, unit = "pt")),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 20, family = "Bahnschrift"))

# produce "Total Games By Designer" plot
count_plot <- top_6_designers %>% 
  mutate(Designer = fct_reorder(boardgamedesigner, desc(-count))) %>% 
  ggplot(aes(x = Designer, y = count)) +
  geom_col(aes(fill = Designer)) +
  labs(caption = "Source: Kaggle / Board Games Geek | Visualisation: @filmicaesthetic") +
  ggtitle("TOTAL GAMES BY DESIGNER", subtitle = "Total number of games where designer was credited, whether individually or as part of a team.") +
  scale_fill_manual(name = "Designer", values = c("#377eb8", "#e41a1c", "#ff7f00", "#ffff33", "#4daf4a", "#984ea3")) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 20, unit = "pt")),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 20, family = "Bahnschrift"))


# SAVE THE PLOT

library(gridExtra)

# arrange subplots
side <- grid.arrange(read_plot, count_plot, nrow = 1, widths = c(1,2.8))

# arrange with main plot
g <- arrangeGrob(main, side, nrow = 2, heights = c(2.5, 1))

# save file
ggsave(file="new_code_test.png", g, width = unit(18, "cm"), height = unit(25, "cm"))
