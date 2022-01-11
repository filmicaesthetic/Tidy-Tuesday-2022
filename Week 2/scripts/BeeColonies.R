#####
#Tidy Tuesday Week 2 - Bee Colonies
#####

library(tidyverse)
library(geojsonio)
library(broom)
library(rgdal)
library(RColorBrewer)
library(mapproj)
library(gganimate)
library(extrafont)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

colony_grp <- colony %>% filter(year == 2021) %>% group_by(state) %>% summarise(colony_added = sum(as.numeric(colony_added), na.rm = FALSE), colony_n = sum(as.numeric(colony_n), na.rm = FALSE), colony_lost = sum(as.numeric(colony_lost), na.rm = FALSE), added_to_lost = ((colony_added - colony_lost) / colony_n * 100))

spdf_fortified <- spdf_fortified %>%
  left_join(. , colony_grp, by=c("id"="state")) 

myPalette <- colorRampPalette(c("#6e1602", "#bf2315", "#ebc923", "#62ba23", "#5ee615"))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(-40, 40), na.value = "#f2e5a2", name = "% CHANGE")

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes(x = long, y = lat, group = group, fill = added_to_lost), color="#d9b91e", size = 2) +
  geom_text(data=centers, aes(x=x, y=y, label=id), family = "Tahoma", fontface= "bold", size = 3, color = "#3b3206" ) +
  sc +
  ggtitle("US Bee Colony Growth in 2021", subtitle = "Net Number of Colonies Added and Lost as a % of Total Colonies") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#ebc923", color = "#ebc923"),
        plot.title = element_text(family = "Tahoma", face = "bold", color = "#3b3206", size = 20, hjust = 0.5),
        plot.subtitle = element_text(family = "Tahoma", color = "#3b3206", size = 10, hjust = 0.5),
        legend.title = element_text(family = "Tahoma", color = "#3b3206", size = 8),
        legend.text = element_text(family = "Tahoma", color = "#3b3206", size = 8)
  ) +
  coord_map()
