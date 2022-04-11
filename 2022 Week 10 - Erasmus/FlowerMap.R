#############
#
# Erasmus Student Mobility
# #TidyTuesday - Week 9
# data: Data.Europa
# hattip to Data is Plural
# flower icons: flaticon.com
#
#############

# load packages
library(tidyverse)
library(ggimage)
library(maps)
library(mapproj)
library(gganimate)
library(lubridate)
library(showtext)

# import google font
font_add_google("Fredoka", "Fredoka")
showtext_auto()

# load data
erasmus <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

# combine receiving and sending city data
erasmus_sel <- erasmus %>% 
  filter(sending_country_code != receiving_country_code) %>%
  select(mobility_start_month, 
         sending_country_code,
         sending_city,
         receiving_country_code,
         receiving_city,
         participants,
         mobility_start_month)

send <- erasmus_sel$sending_country_code
rec <- erasmus_sel$receiving_country_code
send_cit <- erasmus_sel$sending_city
rec_cit <- erasmus_sel$receiving_city
erasmus_switch <- erasmus_sel %>% mutate(sending_country_code = rec,
                         receiving_country_code = send,
                         sending_city = rec_cit,
                         receiving_city = send_cit) 

both_ways <- rbind(erasmus_sel, erasmus_switch)

both_ways <- both_ways %>% 
  rename(country = sending_country_code,
         country_marker = receiving_country_code)

# get city long/lat data
cities <- world.cities %>% 
  rename(sending_city = name) %>%
  distinct(sending_city, country.etc, .keep_all = TRUE)

# merge with data
both_ll <- merge(both_ways, cities, by="sending_city")

# CHOOSE COUNTRY
country_chosen <- c("Germany", "DE")

# create map data
map <- map_data("world", region = country_chosen[1])
cities <- map.cities(x = world.cities, country = country_chosen[1])

# tidy data
both_sel <- both_ll %>% 
  filter(country.etc == country_chosen[1]) %>% 
  mutate(value = participants / 5000) %>%
  select(-receiving_city)

# convert mobility_start_month to date
both_sel <- both_sel %>% 
  group_by(sending_city, long, lat, country_marker, mobility_start_month) %>% 
  summarise(participants = sum(participants)) %>% 
  group_by(sending_city) %>%
  mutate(mobility_start_date = as.Date(paste(mobility_start_month, "01", sep = "-"))) %>%
  filter(country_marker != country_chosen[2])

# create data frame for flower icons
flower_df <- data.frame(country_marker = unique(both_sel$country_marker))

flower_images <- c()

for (i in 1:length(flower_df$country_marker)) {
  flower_images <- c(flower_images, paste0("~/R/Tidy Tuesday 2022/Week 10/flowers/f",i,".png"))
}

flower_df$flower <- flower_images

# create full dataframe for animation
ctry_marker <- unique(both_sel %>%
  select(sending_city, country_marker)
)

all_ctries <- country_pos %>% filter(country %in% both_ll$country_marker) %>% left_join(national_flowers, by = "name")


rep_months <- unique(both_sel$mobility_start_date)
min_months <- min(rep_months)
max_months <- max(rep_months)
month_diff <- interval(min_months, max_months) %/% months(1) + 1
rep_cities <- unique(both_sel$sending_city)

unique_cities <- data.frame(sending_city = c(rep(rep_cities, each = month_diff)),
                            mobility_start_date = c(rep(seq.Date(min(rep_months),max(rep_months), by = "month"), length(rep_cities)))
)

mrg_both <- both_sel %>% select(-long, -lat)
mrg_city <- both_sel %>% select(sending_city, long, lat)
mrg_city <- unique(mrg_city)

long_df <- merge(unique_cities, mrg_city, by = "sending_city", all.x=TRUE)
long_df <- merge(long_df, ctry_marker, by = "sending_city", all.x = TRUE)
long_df <- merge(long_df, flower_df, by = "country_marker", all.x = TRUE)
long_df <- merge(long_df, mrg_both, by = c("sending_city", "mobility_start_date", "country_marker"), all.x=TRUE)

long_df$participants[is.na(long_df$participants)] <- 0

# manually dodge points around city
long_df <- long_df %>%
  arrange(mobility_start_date) %>% 
  group_by(sending_city, country_marker) %>%
  mutate(cumulative = cumsum(participants),
         adj_cumulative = cumulative ^ (1/3) / 50)

long_adj <- long_df %>% group_by(sending_city, country_marker) %>% 
  summarise(max_dist = max(long_df$adj_cumulative[long_df$country_marker == country_marker & long_df$sending_city == sending_city]) / 2) %>%
  group_by(sending_city) %>%
  mutate(rank = rank(max_dist, ties.method = "first"))

long_rank <- merge(long_df, long_adj, by = c("sending_city", "country_marker"), all.x = TRUE)


lvl_df <- data.frame(rank = c(seq(1:57)),
                     level = c(1, rep(2, 8), rep(3, 16), rep(4, 32)),
                     lat_mult = c(1, rep(rep(c(1, -1), each = 4),7)),
                     long_mult = c(1, 1, 1, rep(rep(c(-1, 1), each = 4),6), -1, -1, -1, -1, 1, 1))

long_rank <- merge(long_rank, lvl_df, by = "rank", all.x = TRUE)

long_rank <- long_rank %>% group_by(sending_city) %>% mutate(group_max = max(max_dist))

long_rank <- long_rank %>% 
  mutate(new_lat = lat + (lat_mult * 10 * group_max * sin(plyr::round_any(90 * ((sin((rank - 1) * pi * ((level / 2) * 1.5)) / 2) + 0.5), (45/level)))),
         new_long = long + (lat_mult * 10 * group_max * cos(plyr::round_any(90 * ((cos((rank - 1) * pi * ((level / 2) * 1.5)) / 2) + 0.5), (45/level)))))

# create plot
p <- ggplot(map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill="#6d8a5b", color = "#786e65") +
  coord_map(projection = "mercator") +
  geom_image(data = long_rank, aes(x = new_long, y=new_lat, image=flower, size = I(adj_cumulative))) +
  transition_states(mobility_start_date) +
  enter_grow(size = 0) +
  ggtitle("Cultural Impact of the\nErasmus Exchange Program", subtitle="A visual interpretation of the cumulative cultural impact of the Erasmus exchange program\nin Germany, with each flower representing a visit to or from another country.") +
  labs(caption = "Data: Data.Europa | Visualisation: @filmicaesthetic") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5, margin = margin(t = 20, unit = "pt")),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#e8ddd3", color = "#e8ddd3"),
        plot.background = element_rect(fill = "#e8ddd3", color = "#e8ddd3"),
        text = element_text(family = "Fredoka"))

# animate
a <- animate(p, end_pause = 30, width = 900, height = 1200)

# save animation
anim_save("flower_map.gif", a)
