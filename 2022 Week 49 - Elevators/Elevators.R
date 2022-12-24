# elevators

pacman::p_load(dplyr, ggplot2, tidyr)

tuesdata <- tidytuesdayR::tt_load(2022, week = 49)

elevators <- tuesdata$elevators

floors <- elevators |>
  group_by(DV_FLOOR_FROM) |>
  summarise(n = n()) |>
  arrange(-n)

too_many_stairs <- elevators |>
  filter(as.numeric(DV_FLOOR_FROM) > 10) |>
  mutate(id = paste(HOUSE_NUMBER, STREET_NAME, ZIP_CODE, sep = "_"))

tms_all <- elevators |>
  mutate(id = paste(HOUSE_NUMBER, STREET_NAME, ZIP_CODE, sep = "_")) |>
  filter(id %in% too_many_stairs$id) |>
  mutate(DV_FLOOR_FROM = as.numeric(DV_FLOOR_FROM)) |>
  group_by(id) |>
  mutate(min_floor = min(DV_FLOOR_FROM)) |>
  filter(!is.na(min_floor) & min_floor > 10 & !is.na(DV_FLOOR_TO))

