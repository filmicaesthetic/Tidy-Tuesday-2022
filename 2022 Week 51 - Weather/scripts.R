# weather accuracy

pacman::p_load(dplyr, ggplot2, tidyr, camcorder, lubridate, ggalluvial, ggimage)

camcorder::gg_record(dir = "camcorder")

tuesdata <- tidytuesdayR::tt_load(2022, week = 51)

weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

top_outlooks <- weather_forecasts |>
  group_by(forecast_outlook) |>
  summarise(n = n()) |>
  arrange(-n) |>
  filter(!(is.na(forecast_outlook))) |>
  head(10)


outlook_by_date <- weather_forecasts |>
  filter(date < "2022-01-30") |>
  #filter(forecast_outlook %in% top_outlooks$forecast_outlook) |>
  mutate(month = month(date)) |>
  group_by(month, forecast_outlook) |>
  summarise(n = n()) |>
  group_by(month) |>
  mutate(rank = rank(-n))


outlook_img <- outlook_meanings |>
  mutate(img = paste0("2022 Week 51 - Weather/img/",forecast_outlook,".png")) |>
  filter(forecast_outlook %in% top_outlooks$forecast_outlook)

pal <- c("TSTRMS" = "#c00770")

outlook_by_date <- outlook_by_date |>
  left_join(outlook_img, by = "forecast_outlook")

ggplot(data = outlook_by_date,
       aes(x = month, y = n, alluvium = forecast_outlook)) +
  geom_alluvium(aes(fill = forecast_outlook, colour = forecast_outlook),
                alpha = .75, decreasing = FALSE) +
  geom_image(aes(image = img)) +
  scale_x_continuous() +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -30, hjust = 0),
        legend.position = "none")
