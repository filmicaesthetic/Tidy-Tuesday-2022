## birbs

pacman::p_load(dplyr, ggplot2, tidyr, camcorder, gganimate, maps, mapdata, stringr, av, tweenr, lubridate)

dir.create("2023 Week 2 - Birdwatch")

camcorder::gg_record(dir = "2023 Week 2 - Birdwatch/camcorder", width = 10, height = 6)

tuesdata <- tidytuesdayR::tt_load(2023, week = 2)

public <- tuesdata$PFW_2021_public
countsite <- tuesdata$PFW_count_site_data_public_2021

public_us <- public |>
  filter(grepl("US-", subnational1_code),
         subnational1_code != "US-AK",
         subnational1_code != "US-HI")

top_birds <- public_us |>
  group_by(species_code) |>
  summarise(n = n(),
            qty = sum(how_many),
            states = sum(n_distinct(subnational1_code))) |>
  arrange(-n) |>
  head(20)

state_longlat <- public_us |>
  group_by(subnational1_code) |>
  summarise(longitude = mean(longitude),
            latitude = mean(latitude)) |>
  ungroup()

public_plot <- public_us |>
  mutate(date_mon = as.Date(paste0(Year, "-", str_pad(Month, 2, pad = 0), "-01"))) |>
  filter(species_code == top_birds$species_code[6]) |>
  group_by(subnational1_code, date_mon, species_code) |>
  summarise(qty = sum(how_many)) |>
  ungroup() |>
  group_by(date_mon) |>
  mutate(perc = qty / sum(qty) * 100) |>
  ungroup() |>
  complete(subnational1_code, date_mon, species_code) |>
  left_join(state_longlat, by = "subnational1_code") |>
  mutate(qty = ifelse(is.na(qty), 0, qty),
         perc = ifelse(is.na(perc), 0, perc))

state_pos <- public_plot |>
  group_by(subnational1_code) |>
  summarise(latitude = first(latitude),
            longitude = first(longitude)) |>
  arrange(longitude * latitude)

unique_dates <- unique(public_plot$date_mon)


##### vvvvv

bird_dfs <- function(unique_dates) {
  
  for (i in 1:length(unique_dates)) {
    
    birds <- 2000
    date_it <- unique_dates[i]
    
    pp <- public_plot |>
      filter(date_mon == unique_dates[i]) |>
      mutate(perc_rnd = round(perc * birds / 100),
             perc_diff = perc %% 1,
             abv_blw = ifelse(perc_diff >= 0.5, "abv", "blw"))
    
    add_to <- pp |>
      filter(abv_blw == "blw") |>
      arrange(-perc_diff) |>
      head(20)
    
    take_from <- pp |>
      filter(abv_blw == "abv") |>
      arrange(perc_diff) |>
      head(20)
    
    diff <- sum(pp$perc_rnd) - birds
    
    print(diff)
    
    # if less results - add one
    if (diff < 0) {
      add_list <- add_to |>
        head(abs(diff))
      
      pp <- pp |>
        rowwise() |>
        mutate(perc_rnd = ifelse(subnational1_code %in% add_list$subnational1_code, perc_rnd + 1, perc_rnd))
    }
    
    if (diff > 0) {
      take_list <- take_from |>
        head(abs(diff))
      
      pp <- pp |>
        rowwise() |>
        mutate(perc_rnd = ifelse(subnational1_code %in% take_list$subnational1_code, perc_rnd - 1, perc_rnd))
    }
    
    print(sum(pp$perc_rnd))
    
    pp_sel <- pp |>
      select(subnational1_code, perc_rnd)
    
    it_list <- state_pos |>
      left_join(pp_sel, by = "subnational1_code")
    
    print(sum(it_list$perc_rnd))
    
    it_birds <- data.frame()
    
    for (j in 1:nrow(it_list)) {
      
      st <- it_list$subnational1_code[j]
      
      it <- it_list |>
        filter(subnational1_code == st) |>
        sample_n(size = perc_rnd, replace = TRUE)
      
      it_birds <- rbind(it_birds, it)
      
    }
    
    it_birds$bird_id <- 1:birds
    it_birds$date_mon <- date_it
    
    print(i)
    
    assign(paste0("birds_",i), it_birds, envir = .GlobalEnv)
    
  }
  
}

# create birds dataframes
bird_dfs(unique_dates)

# create in-between states for the data manually
all_birds <- birds_1 |>
  keep_state(40) |>
  tween_state(birds_2, 'sine', 30) |>
  keep_state(40) |>
  tween_state(birds_3, 'sine', 30) |>
  keep_state(40) |>
  tween_state(birds_4, 'sine', 30) |>
  keep_state(40) |>
  tween_state(birds_5, 'sine', 30) |>
  keep_state(40) |>
  tween_state(birds_6, 'sine', 30) |>
  keep_state(40) |>
  mutate(date_text = month(date_mon))
  

chk <- all_birds |>
  group_by(subnational1_code, date_text, .frame) |>
  summarise(n = n())

all_birds_plot <- all_birds |>
  group_by(bird_id) |>
  mutate(shift_state = ifelse(latitude == lag(latitude), 0, 1)) |>
  mutate(shift_state = ifelse(is.na(shift_state), 0, shift_state)) |>
  ungroup() |>
  group_by(subnational1_code, date_text, .frame) |>
  mutate(group_bird_id = row_number()) |>
  ungroup() |>
  group_by(date_text, .id) |>
  mutate(date_mon_id = row_number()) |>
  ungroup() |>
  mutate(shift_perc = ifelse(date_mon_id <= 40, 0,  1 - (abs(date_mon_id - 40 -  ((max(date_mon_id - 40)) / 2)) / ((max(date_mon_id - 40))) * 2))) |>
  mutate(shift_val = group_bird_id / 10,
         long_shift_val = group_bird_id / 20,
         shift = shift_state * shift_val * shift_perc * 0.5,
         long_shift = shift_state * long_shift_val * shift_perc * 0.5,
         longitude = longitude + long_shift,
         latitude = latitude + shift,
         shape = ifelse((.frame %/% 3) %% 2 == 0, "a", "b"))

state <- map_data("state")

state_lkp <- read.csv("2023 Week 2 - Birdwatch/state_lkp.csv")

state_lkp <- state_lkp |>
  mutate(subnational1_code = paste0("US-",Postal.Abbr.),
         region = tolower(State)) |>
  select(region, subnational1_code)

state <- state |>
  left_join(state_lkp, by = "region") |>
  left_join(public_plot, by = "subnational1_code") |>
  select(subnational1_code, long, lat, group) |>
  unique()

state_plot <- public_plot |>
  left_join(state, by = "subnational1_code")

bg_col <- "#f1f1f1"
ln_col <- "#777777"

pal <- c("a" = 17,
         "b" = 20)

p <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), color = ln_col, fill = bg_col) + 
  geom_point(data = public_plot, aes(x = longitude, y = latitude, size = perc), color = ln_col) +
  geom_point(data = all_birds_plot, aes(x = longitude, y = latitude, group = seq_along(bird_id), shape = shape), color = ln_col, alpha = 0.5, size = 0.5) +
  scale_shape_manual(values = pal) +
  transition_time(time = date_mon) +
  guides(fill="none",
         size = "none") + 
  theme(plot.background = element_rect(fill = bg_col, color = bg_col),
        panel.background = element_rect(fill = bg_col, color = bg_col),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  coord_fixed(1.3)

b <- animate(p, duration = 20, fps = 20, width = 800, height = 500, renderer = av_renderer())
anim_save("2023 Week 2 - Birdwatch/output.mp4", b)
