## Museums
pacman::p_load(dplyr, ggplot2, glmnet)

tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums

museums |>
  ggplot(aes(x = Area_Deprivation_index_services)) +
  geom_histogram()

museums_sel <- museums |>
  select(tar = Subject_Matter, contains("Area_Deprivation")) |>
  mutate(tar = ifelse(tar == "Local_Histories", 1, 0))

museums_fil <- museums_sel[complete.cases(museums_sel),]

museums_bal <- museums_fil |>
  group_by(tar) |>
  sample_n(800)
  

lm1 <- lm(data = museums_bal, tar ~ .)

summary(lm1)

lm1_best <- step(lm1, direction = "both", trace = 0)

summary(lm1_best)

museums_bal$prediction <- predict(lm1_best)
museums_bal$prediction_rnd <- round(predict(lm1_best))

museums_tbl <- museums_bal |>
  group_by(tar, prediction_rnd) |>
  summarise(n = n()) 

museums_tbl

sum(museums_tbl$n[museums_tbl$tar == museums_tbl$prediction_rnd]) / sum(museums_tbl$n)

museums_bal |>
  ggplot(aes(x = as.factor(tar), y = prediction)) +
  geom_point(position = position_jitter(w = 0.3))
