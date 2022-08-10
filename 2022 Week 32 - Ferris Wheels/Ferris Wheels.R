# load required packages
pacman::p_load(tidyverse, showtext, lubridate, ggforce, gganimate, stringr)

# download google fonts for viz
font_add_google("Bungee", "Bungee")
font_add_google("Roboto", "Roboto")
showtext_auto()

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 32)

# get ferris wheels dataset
wheels <- tuesdata$wheels

# create basic model to predict missing diameters
diam_est <- lm(diameter ~ height, wheels)

# get predictions
pred_diam <- predict.lm(diam_est, wheels %>% select(height))

# add predictions to dataset
wheels$diam_pred <- pred_diam

# create new column with existing diameters and predictions filling in gaps
wheels_imp <- wheels %>%
  mutate(new_diameter = ifelse(is.na(diameter), diam_pred, diameter))

# select relevant features for viz - only include operating wheels
wheels_sel <- wheels_imp %>%
  filter(status == "Operating") %>%
  arrange(-diameter) %>%
  select(name, height, diameter, country, location, number_of_cabins, passengers_per_cabin) %>%
  head(20)

# filter out missing data
wheels_fil <- wheels_sel[complete.cases(wheels_sel),]

# create iterations with different starting angle to create movement in animation
for (i in 1:9) {
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
           cabin_angle = ((6.28319 / number_of_cabins) * cabin_id) - (((6.28319 / number_of_cabins / 3) * (i - 1))) - (((6.28319 / number_of_cabins / 3) * 12 * order)),
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

# add frame numbers
wheels_frame <- wheels_join %>%
  arrange(diameter, iter, cabin_id) %>%
  group_by(name, iter) %>%
  mutate(frame = (order * 12) + iter,
         pass_label = ifelse(cabin_id == 1, paste0(passengers_per_cabin, " passengers\nper cabin"), NA),
         height_label = ifelse(cabin_id == 1, paste0("Height: ", height, "m"), NA),
         diam_label = ifelse(cabin_id == 1, ifelse(is.na(diameter), "diameter data missing", paste0("Diameter: ", diameter, "m")), NA)) 

# create ferris wheel plot
g <- wheels_frame %>%
  #filter(order == 3, iter < 3) %>%
  mutate(name = gsub(" Ferris Wheel", "", name)) %>%
  ggplot() +
  geom_circle(aes(x0 = 0, y0 = stand_height, r = radius), color = "#1d1d1d") +
  geom_circle(aes(x0 = 0, y0 = stand_height, r = radius/  2), color = "#1d1d1d") +
  geom_segment(aes(x = 0, xend = cabin_x, y = stand_height, yend = cabin_y)) +
  geom_point(aes(x = cabin_x, y = cabin_y, size = passengers_per_cabin), shape = 21, color = "#1d1d1d", fill = "white") +
  geom_segment(aes(x = 0, xend = -100, y = stand_height, yend = 0), color = "#1d1d1d") +
  geom_segment(aes(x = 0, xend = 100, y = stand_height, yend = 0), color = "#1d1d1d") +
  geom_segment(aes(x = -100, xend = 100, y = 0, yend = 0), color = "#1d1d1d") +
  geom_segment(aes(x = -radius, xend = radius, y = height + 40, yend = height + 40), color = "#1d1d1d") + # diameter label
  geom_text(aes(x = 0, y = height+50, label = diam_label), family = "Roboto", size = 5) + # diameter label
  geom_segment(aes(x = radius+40, xend = radius+40, y = 0, yend = stand_height + radius), color = "#1d1d1d") + # height label
  geom_text(aes(x = radius+50, y = (stand_height + radius)/2, label = height_label), size = 5, family = "Roboto", angle = 270) + # height label
  geom_text(aes(x = -radius-50, y = (stand_height + radius)/2, label = "."), color = "#478db5", size = 5, family = "Roboto", angle = 270) + # dummy label
  geom_label(aes(x = 0, y = height - (radius / 4), label = pass_label), family = "Roboto") +
  geom_label(aes(x = 0, y = -50, label = name), size = 14, hjust=0.5, vjust = 0.5, label.padding = unit(0.5, "lines"), family = "Bungee") +
  geom_circle(aes(x0 = 0, y0 = stand_height, r = radius / 8), color = "#1d1d1d", fill = "white") +
  labs(title = "World's Largest\nOperating\nFerris Wheels", caption = "data: ferriswheels / Emil Hvitfeldt | visualisation: @filmicaesthetic") +
  #ggtitle("World's Largest Operating\nFerris Wheels") +
  scale_size(range = c(5, 20)) +
  transition_states(frame) +
  #transition_reveal(order) +
  # view_follow(fixed_x = TRUE,
  #             fixed_y = c(-100, NA)) +
  coord_fixed() +
  theme_minimal() +
  theme(text = element_text(family = "Roboto", size = 20,),
        plot.title = element_text(family = "Bungee", size = 35, hjust = 0.5, margin = margin(t = 30, l = 300, r = 300, unit = "pt")),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 8),
        plot.background = element_rect(color = "#478db5", fill = "#478db5"),
        panel.background = element_rect(color = "#478db5", fill = "#478db5"))

# animation
animate(g, fps = 10, height = 1000, width= 800)

# save gif
anim_save("ferriswheels.gif")
