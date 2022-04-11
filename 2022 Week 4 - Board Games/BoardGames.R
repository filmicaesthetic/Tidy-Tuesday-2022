########
# Exploration of board game data
########

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(extrafont)
library(ggtext)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

board_games <- details %>%
  left_join(y = ratings, by = "id")

overall_median <- median(board_games$average)

bg_split <- board_games %>%
  mutate(boardgameartist = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgameartist)), ", "),
         ) %>%
  unnest(boardgameartist)

artist_ratings <- bg_split %>% 
  group_by(boardgameartist) %>% 
  summarise(avg_rating = mean(average),
            count = n()) %>%
  arrange(-count) %>%
  filter(boardgameartist != "(Uncredited)")

top_6_artists <- artist_ratings[1:6,]

bg_top6 <- bg_split %>% filter(boardgameartist %in% top_6_artists$boardgameartist)

bg_top6_dec <- bg_top6 %>% group_by(boardgameartist) %>% mutate(decile_rank = ntile(average, 10)) %>% group_by(boardgameartist, decile_rank) %>% summarise(dec_avg = median(average))
bg_top6_perc <- bg_top6_dec %>% mutate(perc = (dec_avg - overall_median) / overall_median)
bg_top6_perc$grouped <- cut(bg_top6_perc$perc, 
                            breaks = c(-Inf, -0.15, -0.09, -0.03, 0.03, 0.09, 0.15, 0.21, 0.27, Inf),
                            labels = c("<-15%", "-15%:-9%", "-9%:-3%", "-3%:3%", "3%:9%", "9%:15%", "15%:21%", "21%:27%", "28%<"))

factors <-bg_top6_perc %>% select(grouped, perc)
factors <- factors %>% group_by(grouped) %>% summarise(level = 1)
factors$level <- seq(1:9)


##DESIGNER

bg_split <- board_games %>%
  mutate(boardgamedesigner = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgamedesigner)), ", "),
  ) %>%
  unnest(boardgamedesigner)

designer_ratings <- bg_split %>% 
  group_by(boardgamedesigner) %>% 
  summarise(avg_rating = mean(average),
            count = n()) %>%
  arrange(-count) %>%
  filter(boardgamedesigner != "(Uncredited)")

top_6_designers <- designer_ratings[1:6,]

bg_top6 <- bg_split %>% filter(boardgamedesigner %in% top_6_designers$boardgamedesigner)

bg_top6_dec <- bg_top6 %>% group_by(boardgamedesigner) %>% mutate(decile_rank = ntile(average, 10)) %>% group_by(boardgamedesigner, decile_rank) %>% summarise(dec_avg = median(average))
bg_top6_perc <- bg_top6_dec %>% mutate(perc = (dec_avg - overall_median) / overall_median)
bg_top6_perc$grouped <- cut(bg_top6_perc$perc, 
                            breaks = c(-Inf, -0.18, -0.12, -0.06, 0, 0.06, 0.12, 0.18, 0.24, 0.3, Inf),
                            labels = c("<-18%", "-18%:-12%", "-12%:-6%", "-6%:0%", "0%:+6%", "+6%:+12%", "+12%:+18%", "+18%:+24%", "+24%:+30%", "+30%<"))

factors <-bg_top6_perc %>% select(grouped, perc)
factors <- factors %>% group_by(grouped) %>% summarise(level = 1)
factors$level <- seq(1:8)

min(bg_top6_dec$dec_avg)


details_category <- details %>%
  mutate(boardgamecategory = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgamecategory)), ", ")) %>% 
  unnest(boardgamecategory) %>%
  group_by(boardgamecategory) %>%
  summarise(count = n()) %>%
  arrange(-count)

details_artist <- details %>%
  mutate(boardgameartist = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgameartist)), ", ")) %>% 
  unnest(boardgameartist) %>%
  group_by(boardgameartist) %>%
  summarise(count = n()) %>%
  arrange(-count)

details_designer <- details %>%
  mutate(boardgamedesigner = strsplit(gsub("\\[|\\'|\\]", "", gsub(", Jr.", " Jr.", boardgamedesigner)), ", ")) %>% 
  unnest(boardgamedesigner) %>%
  group_by(boardgamedesigner) %>%
  summarise(count = n()) %>%
  arrange(-count)

list <- c('')

x <- gsub("\\[|\\'|\\]", "", details$boardgamecategory)

polar <- data.frame(x = c(10, 
                          9, 11,
                          8, 10, 12, 
                          7, 9, 11, 13, 
                          -2,0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 
                          -1, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 
                          0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 
                          1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 
                          2, 4, 6, 8, 10, 12, 14, 16, 18, 
                          1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 
                          0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 
                          -1, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 
                          -2, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 
                          7, 9, 11, 13, 
                          8, 10, 12, 
                          9, 11, 
                          10))

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
                             7)
                       
                       )



checkers <- data.frame(id = seq(1:125),
                       section = c(0, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4), rep(7, 4),
                                   rep(1,16), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16)),
                       player = c(0, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4), rep(7, 4),
                                   rep(1,10), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16), rep(1, 6)),
                       y = c(0, rep(seq(1:4), 7), rep(c(0.5, 1.5, 2.5, 3.5, 1, 2, 3, 1.5, 2.5, 2, 1, 1.5, 2, 1.5, 2, 2), 6)),
                       x = c(rep(0, 29), rep(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1.5, 1.5, 2, 2, 2.5, 3, 3.5, 4, 5), 6)),
                       level = c(9, rep(c(7, 5, 3, 1), 7), rep(c(8, 6, 4, 2, 7, 5, 3, 6, 4, 4, 5, 6, 5, 4, 3, 2), 6))
)

checkers <- data.frame(id = seq(1:121),
                       section = c(7, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4),
                                   rep(1,16), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16)),
                       player = c(0, rep(1,4), rep(2, 4), rep(3,4), rep(4, 4), rep(5, 4), rep(6, 4),
                                  rep(1,10), rep(2, 16), rep(3,16), rep(4, 16), rep(5, 16), rep(6, 16), rep(1, 6)),
                       y = c(0, rep(seq(1:4), 6), rep(c(0.5, 1.5, 2.5, 3.5, 1, 2, 3, 1.5, 2.5, 2, 1, 1.5, 2, 1.5, 2, 2), 6)),
                       x = c(rep(0, 25), rep(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1.5, 1.5, 2, 2, 2.5, 3, 3.5, 4, 5), 6)),
                       level = c(9, rep(c(7, 5, 3, 1), 6), rep(c(8, 6, 4, 2, 7, 5, 3, 6, 4, 4, 5, 6, 5, 4, 3, 2), 6))
)

checkers_fact <- merge(checkers, factors, by = "level", all.x = TRUE)
checkers_all <- checkers_fact %>% mutate(x_act = 0.5773502692 * x,
                                         angle = atan(y / x_act),
                                         angle_degree = 90 - (angle * 180 / pi),
                                         angle_degree_adj = angle_degree / 3 * 2,
                                         r = sqrt((y^2)+(x_act^2)),
                                         x_amount = section + (angle_degree_adj/60)/2*3) %>%
  arrange(player)

checkers_all$x_amount[1] <- 7
checkers_all <- checkers_all %>% group_by(player, level) %>% arrange(x_amount) %>% mutate(count = row_number(), player_level_count = paste(player, level, count, sep="-"))

bg_top6_perc$player <- rep(1:6, each = 10)

top_games <- bg_top6 %>% group_by(boardgamedesigner) %>% top_n(1, average) %>% select(boardgamedesigner, primary, yearpublished, average, thumbnail)

player_stats <- bg_top6 %>% group_by(boardgamedesigner) %>%
  summarise(min_year = min(yearpublished),
            max_year = max(yearpublished),
            min_to_max = paste(min_year,max_year, sep = "-"))

player_list <- bg_top6_perc %>% group_by(player, boardgamedesigner) %>% summarise(count = n())

player_list <- merge(player_list, player_stats, by = "boardgamedesigner")
player_list <- merge(player_list, top_games, by = "boardgamedesigner")
player_list$colour <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#b3b30e")
player_list <- player_list %>% 
  mutate(label = paste(paste0("<b><span style ='color: ",colour,"; font-size: 30pt;'>",toupper(boardgamedesigner),"</span></b>"),
                      paste0("<span style = 'color: black;font-size: 17pt;'>YEARS ACTIVE:",min_to_max),
                      paste0("<br>TOP RATED GAME<br><span style ='font-size: 14pt;'>",primary, "</span><br>",yearpublished," | <b>", average, "</b> / 10\n</span>"),
                      sep = "<br>"))

players <- merge(bg_top6_perc, factors, by = "grouped", all.x = TRUE)
players <- players %>% group_by(player, level) %>% mutate(count = row_number(), player_level_count = paste(player, level, count, sep = "-"), player_check = player) %>% arrange(player, level)

players$player_level_count[players$player_level_count == "3-9-1"] <- "0-9-1"
players$player_level_count[players$player_level_count == "6-8-2"] <- "5-8-1"

'%!in%' <- function(x,y)!('%in%'(x,y))

check_players <- merge(checkers_all, players, by=c("player_level_count", "grouped"), all.x = TRUE)

players$player_level_count[players$player_level_count %!in% check_players$player_level_count]

bg <- data.frame(x_amount = seq(1:6),
                 r = rep(4.5, 6))

factors$chart_value <- seq(4,0.5, by = -0.5)

factor_labels <- factors %>% arrange(chart_value)

loadfonts()

fun.1 <- function(x) 2.25 + (0.5 * (acos(abs((x %% 1) - 0.5)))) * (0.5 * (sin(abs((x %% 1) - 0.5))))

check_players %>% ggplot() +
  geom_point(aes(x = x_amount, y = r, color = as.factor(player_check)), size = 15) +
  scale_x_continuous(labels = player_list$label, 
                     breaks = player_list$player, 
                     limits = c(1, 7)) +
  theme(axis.text.x = element_markdown(),
        axis.title = element_markdown(),
        axis.ticks = element_blank(),
        text = element_text(size = 16, family = "Bahnschrift"))

lab1

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
  ggtitle("<span style = 'font-size: 45pt;'><b>MOST PRODUCTIVE BOARD GAME DESIGNERS</b></span><br>Exploration of the six board game designers with the most design credits.<br>", subtitle = "<br><span style = 'font-size: 25pt;'>MEDIAN DECILE RATINGS vs.\nOVERALL MEDIAN RATING</span>") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title.position = "plot",
        plot.margin = margin(t = 80, r = 130, b = 30, l = 130, unit = "pt"),
        plot.title = element_markdown(hjust = 0.5, vjust = 0),
        plot.subtitle = element_markdown(hjust = 0.5, vjust = 0, margin = margin(b = 40, unit = "pt")),
        legend.position = "none",
        panel.grid = element_line(color = "#edebe8"),
        legend.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 20, family = "Bahnschrift"))
main

checkers_cart$dummy <- c(0,
                         0, 1,
                         0, 1, 1,
                         0, 0, 1, 1,
                         0, 0, 0, 0, 1,
                         0, 0, 0, 1,
                         0, 1, 1,
                         0, 1,
                         0
                         )



read_plot <- checkers_cart %>% ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(dummy)), size = 10) +
  scale_color_manual(values = c("#a8a5a2", "black")) +
  scale_y_continuous(name = "Variance %", labels = factor_labels$grouped, breaks = c(6, 5, 4, 3, 2, 1, 0, -1)) +
  ggtitle("HOW TO READ THE\nCHECKERS CHART", subtitle = "Every game a designer has produced\nis ranked and split evenly into 10 groups.\n\nEach dot represents one of these groups,\nwith the median rating of the group (decile)\nbeing grouped and plotted vs. the median rating\nacross all games in the dataset.\n\nThe closer to the centre, the higher the rating.") +
  geom_abline(intercept = 2.5, slope = 0) +     
  geom_text(aes(x = 16, y = 3.5, label = "MEDIAN\nRATING:\n6.45"), size = 3, family = "Bahnschrift") + 
  expand_limits(y = c(-2, 8), x = c(3, 17)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0, face = "bold"),
        plot.margin = margin(l = 80, b = 50, unit = "pt"),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0, size = 15, margin = margin(b = 0, unit = "pt")),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 17, family = "Bahnschrift"))
read_plot
top_6_designers <- top_6_designers %>% arrange(-count)
top_6_designers$boardgamedesigner <- as.character(top_6_designers$boardgamedesigner)

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
        plot.margin = margin(r = 80, l = 50, b = 40, unit = "pt"),
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 20, unit = "pt")),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 20, family = "Bahnschrift"))


library(gridExtra)

side <- grid.arrange(read_plot, count_plot, nrow = 1, widths = c(1,2.3))

grid.arrange(main, side, nrow = 2, heights = c(2.6, 1))

g <- arrangeGrob(main, side, nrow = 2, heights = c(2.8, 1))

ggsave(file="test_long.png", g, width = unit(18, "cm"), height = unit(25, "cm"))

count_plot
  
checkers %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 8)

checkers_polar <- data.frame(x = c(rep(seq(1:7), 5), 
                                   rep(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), 5),
                                   1.181556577,
                                   1.181556577
                                   ),
                       y = c(rep(4, 7), 
                             rep(3, 7), 
                             rep(2, 7), 
                             rep(1, 7), 
                             rep(0, 7), 
                             rep(0, 6),
                             rep(0.5773502692, 6), 
                             rep(1.154700538, 6), 
                             rep(1.732050808, 6), 
                             rep(2.309401077, 6),
                             1.527525232,
                             3.055050463
                             ))


checkers_polar <- data.frame(x = c(rep(seq(1:7), 5),
                                   1.50000000000743,
                                   1.1815565774887,
                                   1.10977959256245,
                                   1.07858339923388,
                                   1.50000000000743,
                                   1.26836856253767,
                                   1.1815565774887,
                                   1.50000000000743,
                                   1.31844342251979,
                                   1.81844342252298,
                                   1.50000000000743,
                                   1.73163143747547,
                                   1.68155657749401,
                                   1.89022040744772,
                                   1.81844342252298,
                                   1.92141660077556,
                                   2.50000000000743,
                                   2.1815565774887,
                                   2.10977959256245,
                                   2.07858339923388,
                                   2.50000000000743,
                                   2.26836856253767,
                                   2.1815565774887,
                                   2.50000000000743,
                                   2.31844342251979,
                                   2.81844342252298,
                                   2.50000000000743,
                                   2.73163143747547,
                                   2.68155657749401,
                                   2.89022040744772,
                                   2.81844342252298,
                                   2.92141660077556,
                                   3.50000000000743,
                                   3.1815565774887,
                                   3.10977959256245,
                                   3.07858339923388,
                                   3.50000000000743,
                                   3.26836856253767,
                                   3.1815565774887,
                                   3.50000000000743,
                                   3.31844342251979,
                                   3.81844342252298,
                                   3.50000000000743,
                                   3.73163143747547,
                                   3.68155657749401,
                                   3.89022040744772,
                                   3.81844342252298,
                                   3.92141660077556,
                                   4.50000000000743,
                                   4.1815565774887,
                                   4.10977959256245,
                                   4.07858339923388,
                                   4.50000000000743,
                                   4.26836856253767,
                                   4.1815565774887,
                                   4.50000000000743,
                                   4.31844342251979,
                                   4.81844342252298,
                                   4.50000000000743,
                                   4.73163143747547,
                                   4.68155657749401,
                                   4.89022040744772,
                                   4.81844342252298,
                                   4.92141660077556,
                                   5.50000000000743,
                                   5.1815565774887,
                                   5.10977959256245,
                                   5.07858339923388,
                                   5.50000000000743,
                                   5.26836856253767,
                                   5.1815565774887,
                                   5.50000000000743,
                                   5.31844342251979,
                                   5.81844342252298,
                                   5.50000000000743,
                                   5.73163143747547,
                                   5.68155657749401,
                                   5.89022040744772,
                                   5.81844342252298,
                                   5.92141660077556,
                                   6.50000000000743,
                                   6.1815565774887,
                                   6.10977959256245,
                                   6.07858339923388,
                                   6.50000000000743,
                                   6.26836856253767,
                                   6.1815565774887,
                                   6.50000000000743,
                                   6.31844342251979,
                                   6.81844342252298,
                                   6.50000000000743,
                                   6.73163143747547,
                                   6.68155657749401,
                                   6.89022040744772,
                                   6.81844342252298,
                                   6.92141660077556
                                   
),
y = c(rep(4, 7), 
      rep(3, 7), 
      rep(2, 7), 
      rep(1, 7), 
      rep(0, 7),
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688,
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688,
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688,
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688,
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688,
      0.577350269192219,
      1.52752523165293,
      2.51661147842418,
      3.51188458428467,
      1.15470053838444,
      2.08166599946901,
      3.05505046330585,
      1.73205080757666,
      2.64575131106968,
      1.52752523166763,
      2.30940107676888,
      2.08166599948412,
      2.64575131108497,
      2.51661147845274,
      3.05505046333526,
      3.51188458432688
))

checkers_polar %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 8) +
  coord_polar()


