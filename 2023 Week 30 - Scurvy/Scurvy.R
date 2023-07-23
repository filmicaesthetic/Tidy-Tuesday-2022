# Tidy Tuesday Week 30 - Scurvy

pacman::p_load(dplyr, ggplot2, tidyr, ggradar, stringr, showtext)

font_add_google("Josefin Sans", "Josefin Sans")
showtext_auto()

# install ggradar from github
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)

# load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 30)
scurvy <- tuesdata$scurvy

scurvy |>
  pivot_longer(cols = c(gum_rot_d6, skin_sores_d6, weakness_of_the_knees_d6, lassitude_d6), names_to = "check_name", values_to = "results") |>
  ggplot(aes(x = as.factor(study_id), y = results)) +
  geom_col(aes(fill = treatment)) +
  coord_flip() +
  facet_wrap(~check_name)

scurvy_ext <- scurvy |>
  mutate(mdummy_1 = "0", mdummy_2 = "0", mdummy_3 = "0", mdummy_4 = "0")

scurvy_ext |>
  pivot_longer(cols = c(gum_rot_d6, skin_sores_d6, weakness_of_the_knees_d6, lassitude_d6, mdummy_1, mdummy_2, mdummy_3, mdummy_4), names_to = "check_name", values_to = "results") |>
  mutate(pain_level = as.numeric(substr(results, 1, 1))) |>
  group_by(treatment, dosing_regimen_for_scurvy, check_name) |>
  summarise(results = mean(pain_level),
            fit_for_duty_d6 = mean(as.numeric(substr(fit_for_duty_d6, 1, 1)))) |>
  ungroup() |>
  group_by(treatment) |>
  mutate(treat_label = ifelse(row_number() == 1, treatment, ""),
    desc_label = ifelse(row_number() == 1, dosing_regimen_for_scurvy, "")) |>
  ggplot(aes(x = check_name, y = results)) +
  geom_col(color = "#1d1d1d", fill = "#ffffff", alpha = 0.9, width = 1) +
  # annotate(geom = "text", aes(x = 4.5, y = 1, label = treatment)) +
  geom_text(aes(x = 4.5, y = 1, label = treat_label), family = "Josefin Sans") +
  geom_text(aes(x = 4.5, y = 2, label = str_wrap(desc_label, 30)), vjust=1, family = "Josefin Sans", lineheight = 0.65) +
  facet_wrap(~treatment, nrow = 3) +
  coord_polar() +
  theme(axis.text.x = element_blank(),
        text = element_text(family = "Josefin Sans"))

