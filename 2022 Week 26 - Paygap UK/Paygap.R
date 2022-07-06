## tidy tuesday week 26: Paygap UK

# load required packages
pacman::p_load(tidyverse, showtext, lubridate)

# import the data
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)

paygap <- tuesdata$paygap

paygap_employees <- paygap %>%
  filter(employer_size %in% c("Less than 250", "250 to 499", "500 to 999", "1000 to 4999", "5000 to 19999", "20,000 or more")) %>%
  group_by(employer_id) %>%
  mutate(year = year(due_date)) %>%
  rowwise() %>%
  mutate(min_emp = ifelse(employer_size == "Less than 250", 1,
                          ifelse(employer_size == "250 to 499", 250,
                                 ifelse(employer_size == "500 to 999", 500,
                                        ifelse(employer_size == "1000 to 4999", 1000,
                                               ifelse(employer_size == "5000 to 19999", 5000,
                                                      20000))))),
         
         max_emp = ifelse(employer_size == "Less than 250", 250,
                          ifelse(employer_size == "250 to 499", 499,
                                 ifelse(employer_size == "500 to 999", 999,
                                        ifelse(employer_size == "1000 to 4999", 4999,
                                               ifelse(employer_size == "5000 to 19999", 19999,
                                                      50000))))),
         est_total_employees = sample(seq(min_emp:max_emp), 1))

colnames(paygap_employees) <- gsub("male_", "male.", colnames(paygap_employees))

set.seed(88)

paygap_plot <- paygap_employees %>%
  filter(year == 2022) %>%
  group_by(year) %>%
  slice_sample(n = 20000, weight_by = est_total_employees, replace = TRUE) %>%
  group_by(year) %>%
  mutate(rank = rank(diff_median_hourly_percent, ties.method = "first")) %>%
  select(rank, diff_median_hourly_percent) %>%
  arrange(rank) %>%
  mutate(group = ifelse(diff_median_hourly_percent > 0, "female_below",
                ifelse(diff_median_hourly_percent == 0, "equal",
                       "male_below")),
         plot_median_diff = ifelse(group == "male_below", (1 - (-100 / (diff_median_hourly_percent - 100))) * 100, 100-diff_median_hourly_percent),
         order_calc = ifelse(diff_median_hourly_percent > 0, 200 - diff_median_hourly_percent, 
                             ifelse(diff_median_hourly_percent == 0, 0,
                                    -200 - diff_median_hourly_percent))) %>%
  arrange(order_calc) %>%
  mutate(id = n():1)

female_below <- sum(paygap_plot$diff_median_hourly_percent > 0)
male_below <- sum(paygap_plot$diff_median_hourly_percent < 0)
female_below_perc <- female_below / 20000
male_below_perc <- male_below / 20000

library(ggpattern)

font_add_google("Roboto", "Roboto")
showtext_auto()

paygap_plot %>%
  ggplot(aes(x = id, y = plot_median_diff)) +
  annotate(geom = "segment", x = 17000, xend = 17000, y = -15, yend = 115) +
  annotate(geom = "rect", xmin = 17500, xmax = 20000, ymin = 0, ymax = 100, fill = "black") +
  annotate(geom = "label", x = 17000, y = 115, hjust = 0.5, label = "EQUAL PAY", family = "Roboto", size = 12) +
  annotate(geom = "label", x = 17000, y = -15, hjust = 0.5, label = "EQUAL PAY", family = "Roboto", size = 12) +
  annotate(geom = "rect", xmin = 0, xmax = 20000, ymin = 128, ymax = 130, fill = "#e6e2e1") +
  annotate(geom = "rect", xmin = 0, xmax = 20000, ymin = -30, ymax = -28, fill = "#e6e2e1") +
  annotate(geom = "rect", xmin = 0, xmax = female_below, ymin = 128, ymax = 130, fill = "#F53A3A") +
  annotate(geom = "rect", xmin = 20000 - male_below, xmax = 20000, ymin = -30, ymax = -28, fill = "#F53A3A") +
  annotate(geom = "text", x = 20000, y = -31, angle = 180, size = 35, hjust = 0, vjust = 0, family = "Bebas Neue", label = paste0(round(male_below_perc * 100, 0), "%")) +
  annotate(geom = "text", x = 0, y = 131, size = 35, hjust = 0, vjust = 0, family = "Bebas Neue", label = paste0(round(female_below_perc * 100, 0), "%")) +
  annotate(geom = "text", x = 3000, y = 131, size = 12, hjust = 0, vjust = 0, family = "Roboto", lineheight = 0.3, label = "of the 20,000 simulated sample employees work in a\nbusiness where women's median pay is less than men's.") +
  annotate(geom = "text", x = 17000, y = -31, angle = 180, size = 12, hjust = 0, vjust = 0, family = "Roboto", lineheight = 0.3, label = "of the 20,000 simulated sample employees work in a\nbusiness where men's median pay is less than women's.") +  
  annotate(geom = "text", x = 20000, y = -27, angle = 180, size = 25, family = "Bebas Neue", hjust = 0, vjust = 1, label = "MEN") +
  annotate(geom = "text", x = 0, y = 127, size = 25, hjust = 0, vjust = 1, family = "Bebas Neue", label = "WOMEN") +
  annotate(geom = "text", x = 100, y = -4, size = 9, color = "#5c5c5c", family = "Roboto", hjust = 0, lineheight = 0.3, label = "data: ons.gov.uk\nviz: @filmicaesthetic") +
  geom_col(aes(fill = group), width = 1) +
  scale_fill_manual(values = c("black", "black", "white")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), labels = c("-100%", "-75%", "-50%", "-25%", "0%"),
                     sec.axis = sec_axis(~.*-1, breaks = c(0, -25, -50, -75, -100), labels = c("0%", "-25%", "-50%", "-75%", "-100%"))) +
  theme_minimal() +
  labs(title = "UK PAYGAP 2022", caption = "UK PAYGAP 2022") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(angle = 180, hjust = 1, family = "Roboto", color = "#5c5c5c", size = 28),
        axis.text.y.left = element_text(family = "Roboto", color = "#5c5c5c", size = 28),
        axis.ticks.y = element_line(colour = "#F53A3A"),
        text = element_text(family = "Bebas Neue", size = 20),
        plot.title = element_text(size = 90),
        plot.caption = element_text(size = 90, angle = 180, hjust = 0))

ggsave("paygap_test.png", width = 6, height = 10)
