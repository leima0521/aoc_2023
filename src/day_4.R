# Day 4: Scratchcards ---
library(tidyverse)


df <- read_delim("input/day_4.txt", delim = ":", col_names = FALSE)
names(df) <- c("card", "num")
df <- df %>% separate(num, c("num", "winning_num"), sep = "\\|")
df <- df %>%
  mutate_at(vars(num, winning_num), ~ map(str_split(str_trim(.x), " "), as.numeric))

# Part 1 ---

df_num <- df %>%
  select(card, num) %>%
  unnest(num) %>%
  filter(!is.na(num))
df_winning_num <- df %>%
  select(card, winning_num) %>%
  unnest(winning_num) %>%
  filter(!is.na(winning_num)) %>%
  mutate(winning = 1)

df_num <- left_join(df_num, df_winning_num, by = c("card", "num" = "winning_num"))
df_pt <- df_num %>%
  group_by(card) %>%
  summarise(n_winning = sum(winning, na.rm = TRUE))

df_pt %>%
  filter(n_winning > 0) %>%
  ungroup() %>%
  mutate(
    pt = 2^(n_winning - 1),
    tot_pt = sum(pt)
  ) %>%
  distinct(tot_pt)

# Part 2 ---
df_pt <- df_pt %>%
  transmute(
    card_id = row_number(),
    n_winning,
    last_card_won = card_id + n_winning,
    last_card_won = ifelse(last_card_won >= max(card_id), max(card_id), last_card_won),
    cards_won = map2(card_id + 1, last_card_won, seq),
    cards_won = ifelse(n_winning == 0, list(NA), cards_won)
  )

num_copies <- rep(1, nrow(df_pt))
for (i in seq(df_pt$card_id)) {
  for (j in df_pt$cards_won[i][[1]]) {
    if (!is.na(j)) {
      num_copies[j] <- num_copies[j] + num_copies[i]
    }
  }
}
sum(num_copies)
