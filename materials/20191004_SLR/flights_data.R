library(nycflights13)
library(tidyverse)

flights_subset <- flights %>%
  filter(carrier == "9E", origin == "JFK", month == 1) %>%
  filter(distance %in% c(1029, 589, 765)) %>%
  select(distance, air_time, dest) %>%
  drop_na()

setwd("~/Documents/teaching/2019/spring/stat242/stat242_s2019/materials/20190227_SLR")
write_csv(flights_subset, "flights_data.csv")

fit <- lm(air_time ~ distance, data = flights_subset)
flights_subset2 <- flights_subset %>%
  mutate(
    residual = residuals(fit)
  )

ggplot(data = flights_subset2, mapping = aes(x = residual, color = factor(distance))) +
  geom_density()
