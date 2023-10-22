pacman::p_load(tidyverse, viridis, geomtextpath, showtext, stringr, lubridate)

# load fonts from google
font_add_google("Rubik", "Rubik")
font_add_google("DM Serif Display", "DM Serif Display")
showtext_auto()

# import data from tidytuesday
tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

# create dataframes
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# plot colour set up
bg_color <- "#1d1a1f"
bg_color_2 <- "#3e356b"
fg_color_1 <- "#f7f4f9"
fg_color_2 <- "#ebe6ef"
mid_color <- "#c959a4"

# summarise dataset for plotting
songs_ordered <- taylor_all_songs |>
  filter(artist == "Taylor Swift") |>
  left_join(album_number, by = "album_name") |>
  mutate(select_release = ifelse(is.na(album_release), track_release, album_release)) |>
  arrange(select_release, album_number, track_number) |>
  mutate(order = row_number(),
         duration_cumsum = cumsum(duration_ms) / 1000,
         duration_cumsum_lag = lag(duration_cumsum)) |>
  mutate(duration_cumsum_lag = ifelse(is.na(duration_cumsum_lag), 0, duration_cumsum_lag)) |>
  mutate(beats_per_song = tempo * (duration_ms / 60000)) |>
  mutate(beats_per_song_cumsum = cumsum(beats_per_song),
         beats_per_song_cumsum_lag = lag(beats_per_song_cumsum)) |>
  mutate(beats_per_song_cumsum_lag = ifelse(is.na(beats_per_song_cumsum_lag), 0, beats_per_song_cumsum_lag)) |>
  mutate(album_label = ifelse(track_number == 1, 
                              paste0("<span style='color:",mid_color,";'>",year(album_release)," </span>",
                                           "<strong style='color:",fg_color_2,";'>",album_name,"</strong>"), 
                              NA))

# summarise albums data for plot
album_plot <- songs_ordered |> 
  group_by(album_name) |> 
  summarise(time = seconds_to_period(sum(duration_ms) / 1000),
            xmin = min(duration_cumsum_lag), 
            xmax = max(duration_cumsum), 
            ymin = 100, 
            ymax = mean(tempo) + 100) |>
  filter(!is.na(album_name)) |>
  mutate(time_format = sprintf('%02g:%02g:%02g', round(time@hour), round(minute(time)), round(second(time))))

# values to help with adjusting plot design
album_gap_value <- 50
gfont <- "Rubik"
titlegfont <- "DM Serif Display"
total_duration <- max(songs_ordered$duration_cumsum)
x_nudge <- total_duration * 0.025
total_beats <- sum(songs_ordered$beats_per_song)
plot_scale <- 2

# create dataframe to build custom tempo axis
tempo_df <- data.frame(tempo = c(seq(0, 200, by = 20))) |>
  mutate(y_tempo = tempo + 100,
         tempo_label = paste0("<strong style='color:",fg_color_2,";'>",tempo," </strong>",
                              "<span style='color:",mid_color,";'>bpm</span>"),
         xmin = x_nudge / 2,
         xmax = total_duration + (x_nudge * 1.5))

# generate and save plot
png("2023 Week 42 - Taylor Swift/beat_by_beat.jpg", width = 2500, height = 3000)

songs_ordered |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = total_duration + (x_nudge * 2), ymin = 70, ymax = Inf), fill = bg_color) +
  geom_rect(data = album_plot, aes(xmin = xmin + x_nudge, xmax = xmax + x_nudge, ymin = 290, ymax = 290), fill = fg_color_2, color = bg_color) +
  geom_segment(data = tempo_df, aes(x = xmin, xend = xmax, y = y_tempo, yend = y_tempo), color = bg_color_2) +
  geom_segment(data = album_plot, aes(x = xmin + x_nudge + album_gap_value, xend = xmax + x_nudge - album_gap_value, y = 310, yend = 310), color = fg_color_2) +
  geom_point(data = album_plot, aes(x = xmin + x_nudge + album_gap_value, y = 310), color = fg_color_2, size = 2) +
  geom_point(data = album_plot, aes(x = xmax + x_nudge - album_gap_value, y = 310), color = fg_color_2, size =2) +
  geom_segment(data = album_plot, aes(x = xmin + x_nudge + album_gap_value, xend = xmin + x_nudge + album_gap_value, y = 310, yend = ifelse(album_name == "The Taylor Swift Holiday Collection", 341, 326)), color = fg_color_2) +
  geom_rect(aes(xmin = duration_cumsum_lag + x_nudge, xmax = duration_cumsum + x_nudge, ymin = 100, ymax = 100 + tempo, fill = beats_per_song), linewidth = 0.65 * plot_scale, colour = bg_color) +
  geom_textpath(aes(x = duration_cumsum_lag+150 + x_nudge, y = ifelse(album_name == "The Taylor Swift Holiday Collection", 337, 322), label = toupper(album_label)), rich = TRUE, vjust = 1, spacing = 120 * plot_scale, hjust = 0, size = 3.4 * plot_scale, upright = FALSE, angle=180, family = gfont, color = fg_color_1) +
  geom_textpath(data = tempo_df, aes(x = 0, y = y_tempo, label = tempo_label), family = gfont, spacing = 80 * plot_scale, size = 2.4  * plot_scale, rich = TRUE) +
  geom_textpath(data = album_plot, aes(x = xmin + x_nudge + album_gap_value, y = 307, label = toupper(time_format)), vjust = 1, spacing = 120 * plot_scale, hjust = 0, size = 1.8 * plot_scale, upright = FALSE, angle=180, family = gfont, color = fg_color_2) +
  #annotate(geom = "text", x = 0, y = 0, label = paste0(scales::comma(round(total_beats, -3)), "\nBEATS"), lineheight = 0.85, size = 16 * plot_scale, family = titlegfont, fontface = "bold", color = fg_color_2) +
  annotate(geom = "text", x = 0, y = 0, label = "Taylor\nSwift", lineheight = 0.87, size = 18 * plot_scale, family = titlegfont, fontface = "bold", color = fg_color_2) +
  #annotate(geom = "text", x = 0, y = 50, label = "Taylor Swift", size = 10 * plot_scale, family = titlegfont, fontface = "bold", color = fg_color_1) +
  scale_fill_viridis(option = "mako") +
  ylim(c(0, 346)) +
  xlim(c(0,total_duration+(2 * x_nudge))) +
  labs(title = "125,000 beats", caption = "data: Genius & Spotify via W. Jake Thompson | visualisation: @filmicaesthetic", subtitle = " This chart represents the duration and tempo of all Taylor Swift's songs,\nvisualising the 125,000 beats that make up her discography to date.", fill = "Total beats per song") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(300, units = "pt"),
        legend.key.height = unit(20, units = "pt"),
        legend.title = element_text(size = 25, family = gfont, color = fg_color_2, hjust = 0.05),
        legend.text = element_text(size = 20, family = gfont, color = fg_color_2),
        plot.title = element_text(family = titlegfont, size = 150, margin = margin(50, 50, 5, 50, unit = "pt"), color = fg_color_1, hjust = 0.1),
        plot.subtitle = element_text(family = gfont, lineheight = 1.2, size = 32, margin = margin(20, 50, 0, 50, unit = "pt"), color = fg_color_2, hjust = 0.1),
        plot.background = element_rect(fill = bg_color_2, color = bg_color_2),
        panel.background = element_rect(fill = bg_color_2, color = bg_color_2),
        plot.caption = element_text(size = 10 * plot_scale, color = fg_color_2, family = gfont, margin = margin(50, 0, 50, 0), hjust = 0.96),
        text = element_text(family = gfont, size = 10)
  )

dev.off()
