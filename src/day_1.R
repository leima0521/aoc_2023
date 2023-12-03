# Day 1: Trebuchet 

library(tidyverse)

input <- read_table("input/day_1.txt", col_names = FALSE)
names(input) <- "elf_text"

# Part I ----
input <- input %>%
  mutate(
    elf_num = gsub("[^0-9.-]", "", elf_text),
    elf_num_part1 = as.integer(str_c(str_sub(elf_num, 1, 1), str_sub(elf_num, -1, -1)))
  )

sum(input$elf_num_part1)
testthat::expect_equal(sum(input$elf_num_part1), 54605)

# Part II ----
# test data
# input <- data.frame(
#   elf_text = c(
#     "two1nine",
#     "eightwothree",
#     "abcone2threexyz",
#     "xtwone3four",
#     "4nineeightseven2",
#     "zoneight234",
#     "7pqrstsixteen"
#   )
# )

convert_text_to_num <- function(text) {
  num_in_text <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  if (text %in% num_in_text) {
    as.character(which(num_in_text == text))
  } else {
    text
  }
}

num_pattern <- c("one|two|three|four|five|six|seven|eight|nine|[0-9]")
text_replacement <- c(
  "oneight" = "oneeight",
  "twone" = "twoone",
  "threeight" = "threeeight",
  "fiveight" = "fiveeight",
  "sevenine" = "sevennine",
  "eightwo" = "eighttwo",
  "eighthree" = "eightthree",
  "nineight" = "nineeight"
)

# Another version is to convert all text to numbers and repeat Part 1 code
input <- input %>%
  mutate(
    elf_text_aug = str_replace_all(elf_text, text_replacement),
    elf_text_num = str_extract_all(elf_text_aug, num_pattern),
    elf_num_first = map_chr(elf_text_num, 1),
    elf_num_last = map_chr(elf_text_num, ~ .x[length(.x)]),
    elf_num_first = map(elf_num_first, convert_text_to_num),
    elf_num_last = map(elf_num_last, convert_text_to_num),
    elf_num_part2 = as.integer(str_c(elf_num_first, elf_num_last))
  )

sum(input$elf_num_part2)
testthat::expect_equal(sum(input$elf_num_part2), 55429)

