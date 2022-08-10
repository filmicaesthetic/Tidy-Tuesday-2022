# load required packages
pacman::p_load(tidyverse, showtext, lubridate, ggforce, gganimate)

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 32)

wheels <- tuesdata$wheels

wheels_sel <- wheels %>%
  arrange(-diameter) %>%
  select(name, height, diameter, country, location, number_of_cabins, passengers_per_cabin) %>%
  head(20)

wheels_fil <- wheels_sel[complete.cases(wheels_sel),]

rm(wheels_join)

# create iterations with different starting angle to create movement in animation
for (i in 1:12) {
  wheels_it <- wheels_fil %>%
    head(5) %>%
    arrange(diameter)
  
  wheels_it$order <- 1:5
  
  wheels_it <- wheels_it %>%
    rowwise() %>%
    mutate(radius = diameter / 2,
           cabin_count = list(rep(1, times = number_of_cabins))) %>%
    unnest(cabin_count) %>%
    group_by(name) %>%
    mutate(cabin_id = 1:n(),
           cabin_angle = ((6.28319 / number_of_cabins) * cabin_id) - (((6.28319 / number_of_cabins / 3)) + (((i - 1) * 3) + (i / 3))),
           stand_height = height - radius,
           cabin_x = radius * sin(cabin_angle),
           cabin_y = radius * cos(cabin_angle) + stand_height,
           iter = i
    )
  
  if (exists("wheels_join")) {
    wheels_join <- rbind(wheels_join, wheels_it)
  } else {
    wheels_join <- wheels_it
  }
}

for (i in 1:4) {
  wheels_loopit <- wheels_join
  wheels_loopit$loop <- i
  
  if (exists("wheels_loop")) {
    wheels_loop <- rbind(wheels_loop, wheels_loopit)
  } else {
    wheels_loop <- wheels_loopit
  }
  
}

# add frame numbers
wheels_frame <- wheels_join %>%
  arrange(diameter, iter, cabin_id) %>%
  group_by(name, iter) %>%
  mutate(frame = (order * 12) + iter,
         pass_label = ifelse(cabin_id == 1, paste0(passengers_per_cabin, " passengers\nper cabin"), NA)) 

g <- wheels_frame %>%
  #filter(iter < 2) %>%
  ggplot() +
  geom_circle(aes(x0 = 0, y0 = stand_height, r = radius)) +
  geom_segment(aes(x = 0, xend = cabin_x, y = stand_height, yend = cabin_y)) +
  geom_point(aes(x = cabin_x, y = cabin_y, size = passengers_per_cabin)) +
  geom_segment(aes(x = 0, xend = -100, y = stand_height, yend = 0)) +
  geom_segment(aes(x = 0, xend = 100, y = stand_height, yend = 0)) +
  geom_segment(aes(x = -100, xend = 100, y = 0, yend = 0)) +
  geom_label(aes(x = cabin_x, y = cabin_y + 100, label = pass_label)) +
  geom_label(aes(x = 0, y = 0, label = name), size = 18, hjust=0.5, vjust = 0) +
  scale_size(range(10, 50)) +
  transition_states(frame) +
  transition_reveal(order) +
  # view_follow(fixed_x = TRUE,
  #             fixed_y = c(-100, NA)) +
  coord_fixed() +
  theme_minimal()

animate(g, fps = 6, height = 1000, width= 1000)
