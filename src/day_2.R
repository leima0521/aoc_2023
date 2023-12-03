# Day 2: Cube Conundrum
library(tidyverse)

# Part 1 --------------------------------------------------
input <- read_delim("input/day_2.txt", col_names = FALSE)
names(input) <- c("game", "x")
input <- input %>% mutate(game = as.integer(word(game, 2)))
input <- input %>% separate(x, into = paste0("r", c(1:6)), sep = ";")

input_long <- input %>% pivot_longer(cols = -game, names_to = "round", values_to = "v")
input_long <- input_long %>%
  filter(!is.na(v)) %>%
  mutate(
    round = as.integer(str_sub(round, 2, 2)),
    blue = str_extract(v, "(\\d)+(?= blue)"),
    red = str_extract(v, "(\\d)+(?= red)"),
    green = str_extract(v, "(\\d)+(?= green)")
  ) %>%
  mutate_at(vars(blue, red, green), as.integer) %>%
  replace_na(list(blue = 0, red = 0, green = 0))

input_long <- input_long %>%
  mutate(possible = red <= 12 & green <= 13 & blue <= 14) %>%
  group_by(game) %>%
  mutate(possible_game = min(possible) == TRUE)

possible_games <- input_long %>%
  filter(possible_game == TRUE) %>%
  distinct(game)
sum(possible_games$game)
testthat::expect_equal(sum(possible_games$game), 2528)


# Part 2 --------------------------------------------------
input_long <- input_long %>%
  group_by(game) %>%
  mutate_at(
    vars(blue, red, green),
    list(max = max)
  ) %>%
  mutate(power = blue_max * red_max * green_max) 

power <- input_long %>%
  distinct(game, power)
sum(power$power)
testthat::expect_equal(sum(power$power), 67363)

