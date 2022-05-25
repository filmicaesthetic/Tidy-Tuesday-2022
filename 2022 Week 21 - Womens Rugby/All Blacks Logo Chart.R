##
##
## TidyTuesday Week 21: Women's Rugby
## data: ScrumQueens
##

library(showtext)
library(tidyverse)
library(lubridate)
library(ggtext)

#import google fonts
font_add_google("Titillium Web", "Titillium Web", bold.wt = 900)
font_add_google("Rubik", "Rubik")
showtext_auto()

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 21)

fifteens <- tuesdata$fifteens
sevens <- tuesdata$sevens

# list of new zealand teams
nz_teams <- c("New Zealand", "New Zealand Wild Ducks", "Aotearoa Maori New Zealand")

# fern calculations used later

# # fern design
# fern <- data.frame(x = c(seq(1:100))) %>%
#   mutate(y = x + (sin(x / 7) * 4) - (x / 5))
# 
# fern_sel <- fern %>%
#   filter(x >= 50 & x <= 80)
# 
ang <- -0.28
# 
# fern_top <- fern_sel %>%
#   mutate(x_2 = ((x - 80) * cos(ang)) - ((y - 60.36915) * sin(ang)) + 80 - ((x - 80)/5),
#          y_2 = ((x - 80) * sin(ang)) + ((y - 60.36915) * cos(ang)) + 60.36915) %>%
#   filter(x_2 > 54)
# 
ang_b <- 0.16
# 
# fern_bottom <- fern_sel %>%
#   mutate(x_2 = ((x - 80) * cos(ang_b)) - ((y - 60.36915) * sin(ang_b)) + 80 - ((x - 80)/5),
#          y_2 = ((x - 80) * sin(ang_b)) + ((y - 60.36915) * cos(ang_b)) + 60.36915) %>%
#   filter(x_2 > 54)

# select tournaments to include
tournament_sel <- c("Hong Kong Sevens", "World Cup", "World Series", "Commonwealth Games", "Olympic Games")

# nz_final_wins
nz_final_wins <- sevens %>%
  filter(stage == "Final", 
         winner %in% nz_teams,
         tournament %in% tournament_sel) %>%
  select(date, loser, tournament, score_1, score_2) %>%
  mutate(date_num = year(date) + (month(date) / 12) + day(date) / 365,
         labs = paste0("   ", tournament),
         year = year(date))

# reformat world series final victories
nz_world_series <- nz_final_wins %>%
  filter(tournament == "World Series") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(date = max(as.Date(date)),
            loser = NA,
            tournament = "World Series",
            score_1 = NA,
            score_2 = NA,
            date_num = max(date_num),
            labs = paste0("   ",n(), " WORLD SERIES FINAL",ifelse(n() > 1, "S", "")))

# move a couple of world series finals to avoid overlapping
new_date_1 <- as.Date("2019-08-01")
# manually adjust to fit better on the chart
nz_world_series$date[8] <- as.character(new_date_1)
nz_world_series$date_num[8] <- year(new_date_1) + (month(new_date_1) / 12) + day(new_date_1) / 365

new_date_2 <- as.Date("2013-03-24")
# manually adjust to fit better on the chart
nz_world_series$date[2] <- as.character(new_date_2)
nz_world_series$date_num[2] <- year(new_date_2) + (month(new_date_2) / 12) + day(new_date_2) / 365

# join together
nz_final_wins <- nz_final_wins %>% filter(tournament != "World Series")
nz_final_wins <- rbind(nz_final_wins, nz_world_series)


year_diff <- max(nz_final_wins$date_num) - min(nz_final_wins$date_num)

# create data for stem
stem <- fern_sel %>%
  filter(x < 54 | x > 78) %>%
  mutate(xend = NA,
         yend = NA,
         x_b = NA,
         y_b = NA,
         date = NA,
         date_num = NA)

# creating lines
plot_lines <- data.frame(
  date = as.character(nz_final_wins$date),
  date_num = nz_final_wins$date_num) %>%
  mutate(
    x = 54 + sqrt(((2022.7 - date_num) / year_diff)) * 24,
    y = x + (sin(x / 7) * 4) - (x / 5),
    xend = ((x - 80) * cos(ang)) - ((y - 60.36915) * sin(ang)) + 80 - ((x - 80)/5),
    yend = ((x - 80) * sin(ang)) + ((y - 60.36915) * cos(ang)) + 60.36915,
    x_b = ((x - 80) * cos(ang_b)) - ((y - 60.36915) * sin(ang_b)) + 80 - ((x - 80)/5),
    y_b = ((x - 80) * sin(ang_b)) + ((y - 60.36915) * cos(ang_b)) + 60.36915,
  )

# additional dates to create a smoother curve
add_dates <- data.frame(
  date = NA,
  date_num = seq(1997, 2024, by = 0.1)
) %>%
  mutate(
    x = 54 + sqrt(((2022.7 - date_num) / year_diff)) * 24,
    y = x + (sin(x / 7) * 4) - (x / 5),
    xend = ((x - 80) * cos(ang)) - ((y - 60.36915) * sin(ang)) + 80 - ((x - 80)/5),
    yend = ((x - 80) * sin(ang)) + ((y - 60.36915) * cos(ang)) + 60.36915,
    x_b = ((x - 80) * cos(ang_b)) - ((y - 60.36915) * sin(ang_b)) + 80 - ((x - 80)/5),
    y_b = ((x - 80) * sin(ang_b)) + ((y - 60.36915) * cos(ang_b)) + 60.36915,
  )
# join with stem
add_dates <- rbind(stem, add_dates)

# create axis labels and positions
axis_labels <- data.frame(year = c(1995, 2000, 2005, 2010, 2015, 2020, 2022)) %>%
  mutate(
    x = 54 + sqrt(((2022.4 - year) / year_diff)) * 24,
    y = x + (sin(x / 7) * 4) - (x / 5),
    xend = ((x - 80) * cos(ang)) - ((y - 60.36915) * sin(ang)) + 80 - ((x - 80)/5),
    yend = ((x - 80) * sin(ang)) + ((y - 60.36915) * cos(ang)) + 60.36915,
    x_b = ((x - 80) * cos(ang_b)) - ((y - 60.36915) * sin(ang_b)) + 80 - ((x - 80)/5),
    y_b = ((x - 80) * sin(ang_b)) + ((y - 60.36915) * cos(ang_b)) + 60.36915,
    ang = c(45, 25, 16, 20, 14, 20, 34)
  ) %>%
  mutate(year = round(year))

# join stem and main data
comb <- rbind(stem, plot_lines) %>%
  left_join(nz_final_wins %>% select(date, labs, tournament) %>% mutate(date = as.character(date)), by = "date")

# create scatter plot
# filter just games with new zealand teams
nz_games <- sevens %>%
  filter(winner %in% nz_teams | loser %in% nz_teams) %>%
  mutate(nz_win = ifelse(winner %in% nz_teams, 1, 0),
         nz_score = ifelse(team_1 %in% nz_teams, as.numeric(score_1), as.numeric(score_2)),
         nz_team = ifelse(nz_win == 1, winner, loser),
         final = ifelse(stage == "Final", 1, 0),
         year = ifelse(year(date) >= 2022, year(date) + (month(date) / 12), year(date) + ceiling(month(date) / 4) / 3)) %>%
  group_by(year) %>%
  mutate(nz_score = 1:n()) %>%
  select(year, date, nz_team, nz_score, nz_win)

# angle to rotate main line
ang_c <- 0.28
# scatter data
games_plot_calc <- nz_games %>%
  mutate(score_prop = (nz_score + 2) / max(nz_games$nz_score),
         score_ang = score_prop * ang_c,
         date_num = year,
         x = 54 + sqrt(((2022.7 - date_num) / year_diff)) * 24,
         y = x + (sin(x / 7) * 4) - (x / 5),
         xend = ((x - 80) * cos(score_ang)) - ((y - 60.36915) * sin(score_ang)) + 80,
         yend = ((x - 80) * sin(score_ang)) + ((y - 60.36915) * cos(score_ang)) + 60.36915)

# plot colours
bg_color <- "#070707"
fg_color <- "#fafafa"

# subtitle text
subt <- "**New Zealand Rugby Women's Sevens** team games and tournament final victories from 1997 - 2022.\n
Each iteration of the New Zealand national team has dominated in Women's Sevens tournaments,\n
with the most victories in **World Cup (2)** and **World Series Finals (27)**, and victories in both the\n
**Commonwealth Games (2018)** and **Olympic Games (2021)**.\n\n
All three New Zealand national teams earned victories in the **Hong Kong Sevens** tournament between\n
1997 and 2007: **New Zealand Wild Ducks (2)**, **New Zealand (3)**, and **Aotearoa Maori New Zealand** had\n
the highest number of tournament victories with **5**."

# caption text
capt <- "data: @ScrumQueens | visualisation: @filmicaesthetic"

# build plot
comb %>%
  ggplot() +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend, size = ((80 - x) ^ 2) * 1), color = fg_color) +
  #geom_segment(aes(x = x + 0.15, xend = x + 0.5, y = y, yend = yend + 1, size = ((80 - x) / 20) * (yend - y)), color = bg_color) +
  #geom_text(aes(x = x , y = y , size = (80 - x) ^ 2, label = ifelse(is.na(labs), "", labs)), fontface= "bold", color = fg_color, angle = -45, hjust = 0) +
  geom_text(aes(x = x, y = yend - 0.2, size = 1.4 * ((80 - x) ^ 2), label = paste0(toupper(labs),"   .")), fontface = "bold", color = bg_color, angle = 90, hjust = 1) +
  geom_line(data = add_dates, aes(x = x, y = y, size = 2 * (80 - x) ^ 2 ), color = fg_color) +
  geom_text(data = axis_labels, aes(x = x, y = y, size = 1.2 *((80 - x) ^ 2), family = "Rubik", fontface = "bold", angle = ang, label = year)) +
  geom_line(data = add_dates, aes(x = x, y = yend), size = 4.5, color = bg_color) +
  #geom_line(aes(x = x_b, y = y_b), size = 1, color = fg_color) +
  geom_point(data = games_plot_calc, aes(x = xend, y = yend, alpha = nz_win, size = 0.1 * (80 - x) ^ 2), color = fg_color) +
  annotate(geom = "text", x = 65, y = 40, family = "Titillium Web", size=50, fontface = "bold", label = "DOMINANT", color = fg_color) +
  geom_richtext(x = 65, y = 38, family = "Rubik", fill = NA, size=11, label.color = NA, lineheight = 0.15, label = subt, hjust = 0.5, vjust = 1, color = fg_color) +
  annotate(geom = "text", x = 65, y = 32, family = "Rubik", size=8, lineheight = 0.1, label = ".", vjust = 1, color = bg_color) +
  annotate(geom = "text", x = 80, y = 45, angle = 90, family = "Rubik", size=6, label = capt, hjust=0.5, color = "grey") +
  geom_text(x = 65, y = 44, family = "Rubik", label = "All games played", size = 9, hjust=0.5, color = "grey") +
  geom_text(x = 65, y = 60, family = "Rubik", label = "All tournament final victories", size = 9, hjust=0.5, color = "grey") +
  scale_size(range = c(0.1, 12)) +
  scale_alpha(range = c(0.1, 0.8), breaks = c(0, 1), labels = c("loss", "win")) +
  coord_fixed() +
  theme_minimal() +
  guides(size = "none", alpha = guide_legend(label.position = "bottom")) +
  theme(plot.background = element_rect(color = bg_color, fill = bg_color),
        panel.background = element_rect(color = bg_color, fill = bg_color),
        legend.position = "bottom",
        legend.text = element_text(color = fg_color, size = 26),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Rubik"))

# save plot
ggsave("all_blacks_chart.png", width = 8, height = 10)

