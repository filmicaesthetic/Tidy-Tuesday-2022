# week 41

# load packages
pacman::p_load(tidyverse, tidytuesdayR, lubridate, showtext)

# get data
tuesdata <- tidytuesdayR::tt_load(2022, week = 41)
yarn <- tuesdata$yarn

# identify companies with 1 product and 100+ products
companies_count <- yarn |>
  group_by(yarn_company_name) |>
  summarise(count_by_company = n(),
            num_check = ifelse(count_by_company == 1, "one", 
                               ifelse(count_by_company > 100, "100+", NA))) |>
  filter(!is.na(num_check))

# join num_check to main dataset
yarn_ext <- yarn |>
  left_join(companies_count, by = "yarn_company_name")

# identify weights with 10+ products in each category
weights <- yarn_ext |>
  filter(!is.na(yarn_weight_name)) |>
  group_by(yarn_weight_name) |>
  summarise(n = n()) |>
  arrange(-n) |>
  filter(n > 10)

# summarise by group
yarn_grp <- yarn_ext |>
  filter(yarn_weight_name %in% weights$yarn_weight_name) |>
  group_by(yarn_weight_name, num_check) |>
  summarise(rating_average = mean(rating_average, na.rm = TRUE),
            grp_n = n()) |>
  filter(grp_n > 10) |>
  na.omit() |>
  ungroup()

# finalise dataset for plotting
yarn_plot <- yarn_grp |>
  mutate(yarn_weight_name = str_wrap(yarn_weight_name, 10)) |>
  pivot_wider(id_cols = yarn_weight_name, names_from = "num_check", values_from = rating_average) |>
  rowwise() |>
  mutate(top = max(c(`100+`, one)),
         btm = min(c(`100+`, one)),
         mid = mean(c(top, btm)),
         dif_perc = (one - `100+`) / `100+`,
         `Yarn products per company` = ifelse(`100+` > one, "100+", "one")) |>
  na.omit() |>
  pivot_longer(cols = c(`100+`, one), names_to = "num_check", values_to = "rating_average")

# import fonts from google
font_add_google("Codystar", "Codystar")
font_add_google("Roboto", "Roboto")
showtext_auto()

# create custom palette for plot
pal <- c("100+" = "#a0a8a8",
         "one" = "#13ad99")

# create plot
yarn_plot |>
  mutate(yarn_weight_name = fct_reorder(yarn_weight_name, rating_average, mean)) |>
  ggplot(aes(x = yarn_weight_name, y = rating_average)) +
  geom_segment(aes(y = btm, yend = top, x = yarn_weight_name, xend = yarn_weight_name, color = `Yarn products per company`), size = 1) +
  geom_point(aes(color = num_check, size = rating_average)) +
  geom_text(aes(y = mid, label = paste0(ifelse(dif_perc > 0, "+", ""), scales::percent(dif_perc, accuracy = 0.1)), color = `Yarn products per company`), nudge_x = 0.15, angle = 90, size = 3.5) +
  geom_text(aes(y = top, label = paste0(yarn_weight_name), color = `Yarn products per company`), lineheight = 0.85, vjust = 0, nudge_y = 0.03, size = 4) +
  annotate(geom = "text", x = 1, y = 4.735, hjust = 0, vjust = 1, lineheight = 0.8, label = "One Knit\nWonders", size = 18, family = "Codystar") +
  annotate(geom = "text", x = 1, y = 4.58, hjust = 0, vjust = 1, lineheight = 0.8, size = 4, label = str_wrap("Yarn produced by companies with only 1 product reviewed on Ravelry received higher ratings (1-5) than yarn produced by companies with 100+ products across almost all yarn weights.", 48), family = "Roboto") +
  ylim(c(3.8,4.8)) +
  scale_color_manual(values = pal) +
  scale_size(range = c(6, 8)) +
  labs(x = "Yarn Weight", y = "Average Rating", caption = "data: Ravelry | visualisation: @filmicaesthetic") +
  theme_minimal() +
  guides(size = "none") +
  theme(plot.subtitle = element_text(margin = margin(t = 10, b = 10, unit = "pt")),
        legend.position = "none",
        text = element_text(family = "Roboto", color = "#1d1d1d"),
        plot.background = element_rect(color = "#e8eded", fill = "#e8eded"),
        panel.background = element_rect(color = "#e8eded", fill = "#e8eded"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "#d1d1d1")
  )

