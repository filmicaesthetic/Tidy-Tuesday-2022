# Flower Map with sf
# map code from: https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/
# Francesco Bailo


library(sf)
library(osmdata)
library(raster)

df_calc <- data.frame(lon = long_df$long,
                      lat = long_df$lat)
df_calc <- unique(df_calc)

my_points.df <- df_calc
my_points.sf <- st_as_sf(my_points.df, coords = c("lon", "lat"), crs = 4326)

my_bbox <- c(xmin = min(my_points.df$lon),
             xmax = max(my_points.df$lon),
             ymin = min(my_points.df$lat),
             ymax = max(my_points.df$lat))

my_bbox.m <- 
  matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'], 
           my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']),
         ncol = 2)
my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

my_bbox_buff_2500.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 2500) %>% # 2.5 kilometers
  st_transform(crs = 4326)
my_bbox_buff_5000.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 5000) %>% # 5 kilometers
  st_transform(crs = 4326)
my_bbox_buff_25000.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 25000) %>% # 25 kilometers
  st_transform(crs = 4326)


dem.raster <- getData("SRTM", lat = mean(df_calc$lat, na.rm = TRUE), lon = mean(df_calc$lon, na.rm = TRUE), download = TRUE)
dem.raster <- crop(dem.raster, as(my_bbox_buff_25000.sf, 'Spatial'), snap='out')

dem.m  <-  rasterToPoints(dem.raster)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

slope.raster <- terrain(dem.raster, opt='slope')
aspect.raster <- terrain(dem.raster, opt='aspect')
hill.raster <- hillShade(slope.raster, aspect.raster, 30, 270)
hill.m <- rasterToPoints(hill.raster)
hill.df <-  data.frame(hill.m)
colnames(hill.df) <- c("lon", "lat", "hill")
hill.sf <- st_as_sf(hill.df, coords = c("lon", "lat"), crs = 4326)


osm_lakes.sf <- 
  opq(bbox = st_bbox(my_bbox_buff_25000.sf)) %>%
  add_osm_feature(key = 'water', value = 'lake') %>%
  osmdata_sf()

osm_lakes.sf <- osm_lakes.sf$osm_multipolygons

osm_rivers.sf <- 
  opq(bbox = st_bbox(my_bbox_buff_25000.sf)) %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()

osm_rivers.sf <- osm_rivers.sf$osm_lines

long_adj <- long_df %>% group_by(sending_city, country_marker) %>% 
  summarise(max_dist = max(long_df$adj_cumulative[long_df$country_marker == country_marker & long_df$sending_city == sending_city]) / 2) %>%
  group_by(sending_city) %>%
  mutate(rank = rank(max_dist, ties.method = "first"))

long_rank <- merge(long_df, long_adj, by = c("sending_city", "country_marker"), all.x = TRUE)

# create levels dataframe
lvl_df <- data.frame(rank = c(seq(1:57)),
                     level = c(1, rep(2, 8), rep(3, 16), rep(4, 32)),
                     lat_mult = c(1, rep(rep(c(1, -1), each = 4),7)),
                     long_mult = c(1, 1, 1, rep(rep(c(-1, 1), each = 4),6), -1, -1, -1, -1, 1, 1))

# merge with data
long_rank <- merge(long_rank, lvl_df, by = "rank", all.x = TRUE)

long_rank <- long_rank %>% group_by(sending_city) %>% mutate(group_max = max(max_dist))

long_rank <- long_rank %>% 
  mutate(new_lat = lat + (lat_mult * 1.2 * group_max * sin(plyr::round_any(90 * ((sin((rank - 1) * pi * ((level / 2) * 1.5)) / 2) + 0.5), (45/level)))),
         new_long = long + (lat_mult * 1.2 * group_max * cos(plyr::round_any(90 * ((cos((rank - 1) * pi * ((level / 2) * 1.5)) / 2) + 0.5), (45/level)))))

# testing
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

plyr::round_any((cos(((x / 4) - 1) * pi) / 2), 0.5)

((cos(x) / 2) + 0.5)

for (i in x) {
  p <- (plyr::round_any(cos((x / 4) * pi) / 2, 0.5))
  print(p)
}

x / 4
cos((x / 4) * pi)
plyr::round_any(cos((x / 4) * pi))
plyr::round_any(cos((x / 4) * pi)) / 2
plyr::round_any(cos((x / 4) * pi)) / 2 + 0.5

plot(((cos(x) / 2) + 0.5))

plyr::round_any(90 * ((cos((x - 1) * pi * (1 * 1.5)) / 2) + 0.5), (45/1))

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_tile(data = hill.df, aes(lon, lat, fill = hill), alpha = .75) +
  geom_sf(data = hill.sf, color = "black") +
  scale_fill_gradientn(colours = c("#4b753e", "#60994e", "#a0d96f")) +
  geom_sf(data = osm_lakes.sf, fill = '#9ecae1', colour = NA) +
  #geom_sf(data = osm_rivers.sf, colour = '#9ecae1', size = 0.05) +
  #geom_sf(data = my_bbox_buff_2500.sf, fill = NA) +
  coord_sf(xlim = c(st_bbox(my_bbox_buff_25000.sf)['xmin'], st_bbox(my_bbox_buff_25000.sf)['xmax']), 
           ylim = c(st_bbox(my_bbox_buff_25000.sf)['ymin'], st_bbox(my_bbox_buff_25000.sf)['ymax'])) +
  geom_image(data = long_rank, aes(x = new_long, y=new_lat, image=flower, size = I(adj_cumulative))) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#e8ddd3", color = "#e8ddd3"),
        plot.background = element_rect(fill = "#e8ddd3", color = "#e8ddd3"))
