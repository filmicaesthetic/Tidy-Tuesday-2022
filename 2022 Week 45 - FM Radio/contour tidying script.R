

raw_contour <- read_delim(
  "2022 Week 45 - FM Radio/data/FM_service_contour_current.txt",
  delim = "|"
)

conv_contour <- raw_contour |>
  select(-last_col()) |>
  set_names(nm = c(
    "application_id", "service", "lms_application_id", "dts_site_number", "transmitter_site",
    glue::glue("deg_{0:360}")
  )) |>
  filter(service == "FM ") |>
  sample_n(100)

lng_contour <- conv_contour |>
  separate(
    transmitter_site, 
    into = c("site_lat", "site_long"), 
    sep = " ,") |>
  pivot_longer(
    names_to = "angle",
    values_to = "values",
    cols = deg_0:deg_360
  ) |>
  mutate(
    angle = str_remove(angle, "deg_"),
    angle = as.integer(angle)
  ) |>
  separate(
    values,
    into = c("deg_lat", "deg_lng"),
    sep = " ,"
  )

contour <- na.omit(lng_contour)

# create plot
ggplot(data = contour, aes(x = deg_lng, y = deg_lat, group = application_id)) +
  geom_polygon(alpha = 0.4) +
  theme_minimal() +
  coord_map(projection = "mercator") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())

