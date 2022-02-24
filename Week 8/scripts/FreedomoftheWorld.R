#############
#
# Freedom in the World
# #TidyTuesday - Week 8
# data: Freedom House / United Nations
# by way of Arthur Cheib
#
#############

library(dplyr)
library(ggplot2)
library(showtext)
library(sysfonts)
library(gganimate)

font_add_google("ZCOOL QingKe HuangYou", "ZCOOL QingKe HuangYou")
showtext.auto()


freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
PR_1995 <- freedom %>% filter(year == 1995) %>% group_by(country) %>% summarise("PR_1995" = PR + CL)
PR_2020 <- freedom %>% filter(year == 2020) %>% group_by(country) %>% summarise("PR_2020" = PR + CL) 

change <- merge(PR_1995, PR_2020, by = "country")
change_df <- change %>% mutate(diff = PR_2020 - PR_1995)
change_df <- change_df %>% mutate(scaled_diff = abs(diff / max(change_df$diff)))
change_df$diff <- cut(change_df$diff, breaks = c(-5, -0.1, 0.1, 10), labels = c("NET POSITIVE CHANGE", "NO NET CHANGE", "NET NEGATIVE CHANGE"))

freedom <- merge(freedom, change_df, by = "country", all.x = TRUE)
freedom <- freedom %>% mutate("Index" = PR + CL)
freedom <- freedom[complete.cases(freedom),]

pal <- c("#319422", "grey", "#c4601a")

bg_col <- "#f5f2f0"

p <- freedom %>% 
  ggplot(aes(x = Region_Name, y = Index)) +
  geom_boxplot(alpha = 0.6, fill = bg_col, color = "#947966", outlier.alpha = 0, width = 0.4) +
  geom_point(aes(color = diff, alpha = scaled_diff), position = position_jitter(h = 0.05, w = 0.2)) +
  scale_alpha(range = c(0.2, 0.9), guide = "none") +
  scale_y_reverse(breaks = seq(2:15)) +
  scale_color_manual(values = pal) +
  transition_states(year, transition_length = 3) +
  ggtitle("Political Rights and Civil Liberties Index") +
  labs(subtitle = "Net change by country, grouped by continent, from 1995 - 2020\n\nYear: {closest_state}", caption = "data: freedom house / united nations") +
  theme(plot.background = element_rect(fill = bg_col),
        legend.background = element_rect(fill = "#f7f6f5"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(fill = bg_col),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#f0ede6"),
        panel.grid.minor.y = element_line(color = "#f0ede6"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        text = element_text(family = "ZCOOL QingKe HuangYou", color = "#574233" )
        )

a <- animate(p, renderer = ffmpeg_renderer())

anim_save("animation.mp4", a)
