

pacman::p_load(dplyr, ggplot2, tidyr, stringr, camcorder)

camcorder::gg_record(dir = "camcorder")

tuesdata <- tidytuesdayR::tt_load(2022, week = 52)

tlBooks <- tuesdata$tlBooks
tlFootnotes <- tuesdata$tlFootnotes

tlAll <- tlBooks |>
  left_join(tlFootnotes, by = "footnote")

tlAll |>
  #filter(year > 1800 & year < 2500) |>
  ggplot(aes(x = year)) +
  geom_histogram() +
  geom_text(aes(label = str_wrap(text,50), y = 500)) +
  scale_y_log10() +
  scale_x_log10() 
