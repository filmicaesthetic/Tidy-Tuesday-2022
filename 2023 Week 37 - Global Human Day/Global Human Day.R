
pacman::p_load(tidyverse, sf, rnaturalearth, rnaturalearthdata, countrycode, RColorBrewer, showtext, ggpubr)

font_add_google("Roboto", "Roboto")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2023, week = 37)

all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity

avg_lkp <- global_human_day |>
  select(Subcategory, avghoursperday = hoursPerDay)

unique(all_countries$Category)

largest_variance <- all_countries |>
  left_join(country_regions, by = "country_iso3") |>
  left_join(avg_lkp, by = "Subcategory") |>
  #filter(Subcategory == "Meals") |>
  group_by(country_iso3, country_name, region_name, Category, Subcategory) |>
  summarise(hours = sum(hoursPerDayCombined),
            avg_hours = sum(avghoursperday),
            hours_var = (hours - avg_hours)/avg_hours) |>
  arrange(-abs(hours_var)) |>
  group_by(country_iso3, country_name) |>
  filter(hours_var == max(hours_var))

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  rename(country_iso3 = adm0_a3) |>
  left_join(largest_variance, by = "country_iso3")

RColorBrewer::brewer.pal(n = 8, name = "Set3")

text_mult <- 3.5

gfont <- "Roboto"

ctry_map <- world |>
  filter(country_iso3 != "ATA") |>
  ggplot() +
  geom_sf() +
  geom_sf(aes(group = Subcategory, fill = Category), alpha= 0.7, color = "#5c4f3e") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_void()  +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(title = "  Outstanding Cultural Priorities", subtitle = "    The map below looks at how people in countries around the world spend their time, highlighting the\n    activity group with the largest positive percentage variance to the world average.") +
  theme(panel.background = element_rect(fill = "#f2efeb", color = "#f2efeb"),
        plot.background = element_rect(fill = "#f2efeb", color = "#f2efeb"),
        text = element_text(family = gfont),
        plot.title = element_text(size = 20 * text_mult, face = "bold", family = gfont, margin = margin(20, 0, 10, 40)),
        plot.subtitle = element_text(size = 10 * text_mult, family = gfont, margin = margin(0,0,20,20), lineheight = 0.3),
        legend.position = "none")

plot_data <- world |>
  mutate(orig_subcat = Subcategory) |>
  mutate(Subcategory = paste(Category, Subcategory, sep = " - ")) |>
  arrange(Category, Subcategory, -hours_var) |>
  mutate(row_x = as.numeric(row_number()) - 1,
         cat_x = as.numeric(as.factor(Category)),
         sub_x = as.numeric(as.factor(Subcategory))) |>
  mutate(row_space = (row_x + (3 * sub_x) - 3)) |>
  mutate(row = 50 - (1 * (row_space) %% 50),
         column = 18 * ((row_space) %/% 50) + 1) |>
  mutate(subcat_lab = ifelse(row == lag(row) - 1, NA, str_wrap(orig_subcat, 35) )) |>
  mutate(subcat_lab = ifelse(row_number() == 1, str_wrap(orig_subcat, 35), subcat_lab)) |>
  mutate(subcat_lab = ifelse(subcat_lab == "NA - NA", NA, subcat_lab)) |>
  mutate(country_name = gsub(" Special Administrative Region", "", country_name),
         country_name = gsub(" People's Democratic Republic", "", country_name),
         country_name = gsub("Democratic People's Republic of Korea", "North Korea", country_name),
         country_name = gsub("Republic of Korea", "South Korea", country_name),
         country_name = gsub("Democratic Republic of the Congo", "DR of Congo", country_name),
         country_name = gsub("United Republic of Tanzania", "Tanzania", country_name),
         country_name = gsub(" \\(Plurinational State of\\)", "", country_name),
         country_name = gsub(" \\(Bolivarian Republic of\\)", "", country_name),
         country_name = gsub(" \\(Federated States of\\)", "", country_name),
         country_name = gsub("Saint Vincent and the Grenadines", "St. Vincent & Grenadines", country_name),
         country_name = gsub("United States of America", "USA", country_name),
         country_name = gsub(" \\(Islamic Republic of\\)", "", country_name),
         country_name = gsub("United States Virgin Islands", "Virgin Islands", country_name),
         country_name = gsub(" of Great Britain and Northern Ireland", "", country_name)) |>
  filter(country_iso3 != "ATA")

max_val <- max(plot_data$hours_var, na.rm = TRUE)

plot_bar_calc <- function(val, column) {
  res <- (10 * (val / max_val)) + column
}

cat_plot <- data.frame(Category = unique(plot_data$Category)) |>
  filter(!is.na(Category)) |>
  mutate(row = rep(c(1,5), each = 4) + 58,
         column = (rep(1:4, 2) * 24) - 32)

ctry_list <- plot_data |>
  ggplot() +
  geom_text(aes(x = column, y = row, label = "I", color = Category), hjust = 0, size = 12, family = gfont) +
  geom_text(aes(x = column-1.5, y = row, label = country_name), hjust = 1, size = 1.4 * text_mult, family = gfont) +
  geom_label(aes(x = column, y = row+2, label = subcat_lab), fill = "#f2efeb", fontface = "bold", hjust = 0.5, size = 2.2 * text_mult, lineheight = 0.8, family = gfont) +
  geom_segment(aes(x = column+0.2, xend = plot_bar_calc(hours_var, column), y = row, yend = row, color = Category), size = 1) +
  geom_text(aes(x = plot_bar_calc(hours_var, column) + 0.4, y = row, label = paste0("+", scales::percent(hours_var, accuracy = 1))), hjust = 0, size = 1.1 * text_mult, family = gfont) +
  geom_point(data = cat_plot, aes(x = column, y = row, color = Category), size = 8, shape = 18) +
  geom_text(data = cat_plot, aes(x = column + 2, y = row, label = Category), size = 2.4 * text_mult, hjust = 0, family = gfont) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_void() +
  coord_equal(xlim = c(-10, 85)) +
  labs(caption = "Data: The Human Chronome Project - The global human day in PNAS") +
  theme(panel.background = element_rect(fill = "#f2efeb", color = "#f2efeb"),
        plot.background = element_rect(fill = "#f2efeb", color = "#f2efeb"),
        plot.caption = element_text(size = 5 * text_mult, margin = margin(r = 10)),
        legend.position = "none",
        text = element_text(family = gfont))

all_plot <- ggarrange(ctry_map, ctry_list, ncol = 1) + bgcolor("#f2efeb")

ggsave("2023 Week 37 - Global Human Day/test_plot.jpg", plot = all_plot, height = 10, width = 8)

