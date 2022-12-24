
pacman::p_load(tidyverse)

data <- tidytuesdayR::tt_load("2022-11-08")

state_stations <- data$state_stations
station_info <- data$station_info

radio <- state_stations |> 
  right_join(station_info, by = c("call_sign"))

radio |>
  group_by(format) |>
  summarise(count = n()) |>
  arrange(-count) |>
  head(100)

classic_rock <- radio |>
  filter(format == "Classic rock")

