# week 40

# load packages
pacman::p_load(tidyverse, tidytuesdayR, lubridate)

# get data
tuesdata <- tidytuesdayR::tt_load(2022, week = 40)

product_hunt <- tuesdata$product_hunt

product_hunt <- product_hunt  |>
  mutate(release_year = year(as.Date(release_date)))

product_hunt |> head()

product_hunt_long <- product_hunt |>
  mutate(makers = gsub("\\[|\\]|\\'", "", makers)) |>
  mutate(makers = str_split(makers, ", ")) |>
  unnest(makers)

top_10_makers <- product_hunt_long |>
  filter(nchar(makers) > 0) |>
  group_by(makers) |>
  summarise(n = n(),
            avg_upvotes = median(upvotes),
            total_upvotes = sum(upvotes)) |>
  arrange(-total_upvotes) |>
  head(10)

top_10_hunters <- product_hunt_long |>
  filter(nchar(hunter) > 0) |>
  group_by(hunter) |>
  summarise(n = n(),
            avg_upvotes = median(upvotes),
            total_upvotes = sum(upvotes)) |>
  arrange(-total_upvotes) |>
  head(10) |>
  arrange(total_upvotes) |>
  mutate(total_upvotes_r = sqrt(total_upvotes / pi) * 2,
         cumsum_upvotes = cumsum(total_upvotes_r))

top_10_hunters <- top_10_hunters |>
  mutate(plot_position_x = cumsum_upvotes / sum(top_10_hunters$total_upvotes_r) - ifelse(is.na(lag(cumsum_upvotes)) == TRUE, 0, lag(cumsum_upvotes) / sum(top_10_hunters$total_upvotes_r) / 2),
         plot_position_y = 1)

top_10_hunters <- product_hunt_long |>
  filter(nchar(hunter) > 0) |>
  group_by(hunter) |>
  summarise(n = n(),
            avg_upvotes = median(upvotes),
            total_upvotes = sum(upvotes)) |>
  arrange(-total_upvotes) |>
  head(10) |>
  arrange(total_upvotes) |>
  mutate(total_upvotes_r = sqrt(total_upvotes / pi) * 2,
         cumsum_upvotes = cumsum(total_upvotes_r))

top_10_hunters <- top_10_hunters |>
  mutate(plot_position_x = ifelse(is.na(lag(cumsum_upvotes)) == TRUE, 0, lag(cumsum_upvotes)) + (total_upvotes_r / 2),
         plot_position_y = 1)

upvotes_by_year <- product_hunt_long |>
  filter(nchar(makers) > 0) |>
  group_by(release_year) |>
  summarise(n = n(),
            avg_upvotes = median(upvotes),
            total_upvotes = sum(upvotes))

hunter_makers <- product_hunt_long |>
  filter(makers %in% top_10_makers$makers) |>
  mutate(hunter = ifelse(hunter %in% top_10_hunters$hunter, hunter, "other"))

product_hunt_long |>
  filter(makers %in% top_10_makers$makers) |>
  arrange(upvotes) |>
  mutate(makers = fct_reorder(makers, -upvotes, sum)) |>
  ggplot(aes(x = makers, y = upvotes)) +
  geom_col(color = "#d1d1d1") +
  theme_minimal()

upvotes_by_year |>
  ggplot(aes(x = release_year, y = total_upvotes)) +
  geom_col()

ggplot() +
  geom_point(data = top_10_hunters, aes(x = plot_position_x, y = plot_position_y, size = total_upvotes)) +
  geom_text(data = top_10_hunters, aes(x = plot_position_x, plot_position_y - 0.25, label = hunter)) +
  scale_size(range = c(8.78 * 2, 22.98 * 2)) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(legend.position = "none")

min(top_10_hunters$total_upvotes)
