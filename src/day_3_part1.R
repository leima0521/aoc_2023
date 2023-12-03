# Day 3: Gear Ratios
library(tidyverse)

# input <- c(
#   "467..114..",
#   "...*......",
#   "..35..633.",
#   "......#...",
#   "617*......",
#   ".....+.58.",
#   "..592.....",
#   "......755.",
#   "...$.*....",
#   ".664.598.."
# )
# input <- as.data.frame(input)


# Part 1 ------------------------------------------------------------
input <- read_table("input/day_3.txt", col_names = "input")

input <- input %>%
  mutate(
    special_loc = str_locate_all(string = input, pattern = "[^0-9.]"),
    special_loc_possible = pmap(list(special_loc, lag(special_loc), lead(special_loc)), c),
    special_loc_possible = map(special_loc_possible, ~ .x[.x != -1]),
    special_loc_possible = map(special_loc_possible, ~ c(.x, .x - 1, .x + 1)),
    num = str_extract_all(input, "\\d+"),
    num_begin_loc = gregexpr(pattern = "\\d+", input),
    num_len = map(num, str_length)
  )

input_unnest <- input %>% unnest(num:num_len)
input_unnest <- input_unnest %>%
  mutate(
    num_end_loc = num_begin_loc + num_len - 1,
    num_possible_beg = map2_lgl(num_begin_loc, special_loc_possible, ~ any(.x %in% .y)),
    num_possible_end = map2_lgl(num_end_loc, special_loc_possible, ~ any(.x %in% .y)),
    num_possible = num_possible_beg == TRUE | num_possible_end == TRUE,
    num_to_sum = ifelse(num_possible == FALSE, 0, num)
  )
sum(as.integer(input_unnest$num_to_sum))


