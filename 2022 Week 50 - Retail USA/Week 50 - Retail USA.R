
# shopping

# load packages
pacman::p_load(dplyr, ggplot2, tidyr, stringr)

# import data
tuesdata <- tidytuesdayR::tt_load(2022, week = 50)

state_retail <- tuesdata$state_retail
coverage_codes <- tuesdata$coverage_codes

state_retail_conv <- state_retail |>
  mutate(change_yoy = as.numeric(change_yoy))

# fill missing values with average of surrounding values by state & sector
state_retail_fill <- state_retail |>
  mutate(change_yoy = as.numeric(change_yoy)) |>
  group_by(state_abbr, subsector) |>
  mutate(
    change_yoy = 
      if_else(
        is.na(change_yoy),
        slider::slide_dbl(change_yoy, mean, .before = 6, .after = -6),
        change_yoy
      )
  ) |> 
  ungroup()

summary(state_retail_fill)

state_retail_fill |>
  filter(state_abbr != "USA",
         subsector != "total") |>
  #filter(subsector == "Food and Beverage") |>
  mutate(date = as.Date(paste0(year,"-",str_pad(month, 2, pad = "0"), "-01")),
         change_yoy = as.numeric(change_yoy)) |>
  ggplot(aes(x = date, y = change_yoy)) +
  geom_line(aes(group = state_abbr, color = subsector)) +
  facet_wrap(~subsector)





state_retail |>
  mutate(date = as.Date(paste0(year,"-",str_pad(month, 2, pad = "0"), "-01")),
         change_yoy = as.numeric(change_yoy)) |>
  select(date, subsector, state_abbr, change_yoy) |>
  pivot_wider(id_cols = c("date", "subsector"), names_from = "state_abbr", values_from = "change_yoy")



state_retail |>
  group_by(state_abbr, coverage_code) |>
  summarise(n = n()) |>
  ggplot(aes(x = n, y = state_abbr)) +
  geom_col(aes(fill = coverage_code), position = position_stack())
