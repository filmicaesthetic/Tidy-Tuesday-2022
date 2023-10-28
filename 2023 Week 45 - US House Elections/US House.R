# us house elections

pacman::p_load(tidyverse, showtext, ggtext)

# load fonts from google
font_add_google("Rubik", "Rubik")
font_add_google("DM Serif Display", "DM Serif Display")
showtext_auto()

# import data from tidytuesday
tuesdata <- tidytuesdayR::tt_load(2023, week = 45)

house <- tuesdata$house |>
  filter(special == FALSE,
         unofficial == FALSE) |>
  group_by(year, state_cen, office, district, stage) |>
  mutate(win = candidatevotes == max(candidatevotes)) |>
  ungroup()


top_x <- house |>
  group_by(candidate, party) |>
  summarise(votes = sum(candidatevotes)) |>
  filter(!(candidate %in% c("BLANK VOTE/SCATTERING", "BLANK", "BLANKS", 
                            "SCATTERING", "EXHAUSTED BALLOT", "WRITEIN", 
                            "BLANK VOTE/VOID VOTE/SCATTERING", "OVER VOTE",
                            "OVERVOTES", "OTHER"))) |>
  arrange(-votes) |>
  head(10)

rel_year <- house |>
  filter(!(candidate %in% c("BLANK VOTE/SCATTERING", "BLANK", "BLANKS", 
                            "SCATTERING", "EXHAUSTED BALLOT", "WRITEIN", 
                            "BLANK VOTE/VOID VOTE/SCATTERING", "OVER VOTE",
                            "OVERVOTES", "OTHER"))) |>
  group_by(candidate) |>
  mutate(first_year = min(year)) |>
  mutate(rel_year = year - first_year) |>
  group_by(candidate) |>
  arrange(year) |>
  mutate(cumulative_total = cumsum(candidatevotes),
         end_total = sum(candidatevotes),
         votes_per_year = sum(candidatevotes) / max(rel_year),
         wins = sum(win),
         win_rate = wins / n()
         ) |>
  select(candidate, first_year, year, rel_year, party, cumulative_total, 
         candidatevotes, end_total, votes_per_year, 
         wins, win_rate
         ) |>
  ungroup() |>
  arrange(end_total) |>
  mutate(candidate = as.factor(candidate))


rel_year |>
  filter(rel_year > 0) |>
  mutate(vote_per_year = cumulative_total / rel_year ) |>
  arrange(-vote_per_year)


top_x_rel <- rel_year |>
  filter(candidate %in% c(top_x$candidate)) |>
  mutate(col_pal = "topx")

bottom_x_rel <- rel_year |>
  filter(!(candidate %in% c(top_x$candidate))) |>
  mutate(col_pal = "other") |>
  filter(end_total > 10000)


pal <- c("DEMOCRAT"="#2c5ba8",
         "REPUBLICAN"="#ba272c")

top_avg_votes <- rel_year |>
  group_by(candidate, party) |>
  filter(rel_year == max(rel_year), rel_year >= 4) |>
  mutate(votes_per_election = candidatevotes / n()) |>
  arrange(-votes_per_election) |>
  head(10)

bottom_avg_votes <- rel_year |>
  group_by(candidate, party) |>
  filter(n() > 3, rel_year == max(rel_year), rel_year >= 30) |>
  mutate(votes_per_election = candidatevotes / n()) |>
  arrange(votes_per_election) |>
  head(10)

top_x_plot <- rel_year |>
  group_by(candidate, wins, win_rate, first_year, end_total) |>
  summarise(elections = n(),
            party = first(party),
            ly_check = sum(year[year == 2022])>0) |>
  arrange(-end_total) |>
  head(10) |>
  ungroup() |>
  mutate(plot_id = row_number(),
         plot_point = ifelse(ly_check == TRUE, 1, NA))
  

pacman::p_load(plotly)

plot_scale <- 5
bg_color <- "#e8e1d3"
bg_color_2 <- "#d9d0bf"
table_start <- 45.5

# generate and save plot
png("2023 Week 45 - US House Elections/Vote Goats.jpg", width = 3500, height = 2000)

top_x_rel |>
  ggplot(aes(x = rel_year, y = cumulative_total)) +
  geom_line(data = bottom_x_rel, aes(group = candidate), linewidth = 1.5, color = bg_color_2) +
  geom_point(data = rel_year |> filter(year == 2022) |> filter(candidate %in% top_x$candidate), aes(group = candidate, color = party, size = end_total, fill = party), shape = 21, stroke = 1 * plot_scale) +
  geom_path(data = top_x_rel, aes(group = candidate, color = party), linewidth = 3) +
  geom_point(data = rel_year |> filter(year == 2022) |> filter(!(candidate %in% top_x$candidate)), aes(group = candidate, color = party, size = end_total/3), shape = 21, stroke = 0.5 * plot_scale, alpha = 0.5) +
  geom_text(data = top_x_plot, aes(x = table_start, y = 2000000 - (plot_id * 120000), label = candidate), family = "Rubik", hjust = 1, size = 2 * plot_scale) +
  geom_rect(data = top_x_plot, aes(x = 1, y = 1, xmin = table_start + 0.4, xmax = table_start + 0.4 + ((end_total / max(end_total)) * 4.2), ymin = 2000000 - (plot_id * 120000) + 8000 - 28000, ymax = 2000000 - (plot_id * 120000) - 8000 - 28000, fill = party)) +
  geom_text(data = top_x_plot, aes(x = table_start + 0.4, y = 2000000 - (plot_id * 120000)+13000, label = scales::comma(end_total)), family = "Rubik", hjust = 0, size = 1.2 * plot_scale) +
  geom_text(data = top_x_plot, aes(x = table_start + 4.9, y = 2000000 - (plot_id * 120000), label = paste0("Wins: ", wins, " (", round(win_rate * 100, 1), "%)")), family = "Rubik", hjust = 0, size = 1.2 * plot_scale) +
  geom_point(data = top_x_plot, aes(x = table_start + 8.6, y = 2000000 - (plot_id * 120000), color = party, size = plot_point * 300000 * plot_scale)) +
  annotate(geom = "text", x = table_start + 2.4, y = 2100000, label = "Top candidates by votes", size = 3 * plot_scale, family = "DM Serif Display") +
  scale_linewidth(range = c(0.2,1)*plot_scale) +
  scale_size(range = c(1, 3.5)*plot_scale) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = c(seq(4,44, by = 4))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Years since first election", y = "Total cumulative votes", title = "**US House of Representatives:** Vote GOATs",
       subtitle = paste("Congress's top 10 all-time vote-getters includes <span style='color:#2c5ba8'>**8 Democrats**</span> and <span style='color:#ba272c'>**2 Republicans**</span>: former Speaker of the House,<br><i>**Nancy Pelosi**</i>, leads the pack with a total of",scales::comma(max(top_x_plot$end_total)),"votes from 18 elections (and 18 wins) throughout her 36 year career.<br><i>**F James Sensenbrenner JR**</i> is the Republican with the highest number of votes received, during his time in Congress from 1980 - 2021", sep = " "),
       caption = "<i>**Data**: MIT Election Data and Science Lab (MEDSL) | **Visualisation**: @filmicaesthetic</i>") + 
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Rubik", size = 8 * plot_scale),
        plot.title = element_markdown(family = "DM Serif Display", size = 20 * plot_scale, margin = margin(70, 0, 20, 0, unit = "pt"), hjust = 0.04),
        plot.subtitle = element_markdown(family = "Rubik", size = 6.5 * plot_scale, margin = margin(20, 40, 50, 40, unit = "pt"), hjust = 0, lineheight = 1.8),
        plot.background = element_rect(fill = bg_color, color = bg_color),
        plot.caption = element_markdown(size = 5 * plot_scale, margin = margin(50, 40, 30, 0)),
        panel.background = element_rect(fill = bg_color, color = bg_color),
        panel.grid.minor.y = element_line(color = bg_color_2),
        panel.grid.major.y = element_line(color = bg_color_2),
        axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 25, 0, 40)))

dev.off()
