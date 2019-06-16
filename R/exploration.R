library(tidyverse)
library(ggthemes)

data_path <- "input/kobe-bryant-shot-selection/"
file_name <- "data.csv"
file_path <- str_c(data_path, file_name)

shots <- read_csv(file_path)  %>%
  mutate_if(is.character, factor)


# create test, validation, and test set

set.seed(1909)

shots_test <- shots %>%
  filter(is.na(shot_made_flag))

sample_size <- nrow(shots)
validation_set_share <- 0.05
test_set_size <- nrow(shots_test)


random_numbers <- runif((sample_size - test_set_size))
random_flags <- ifelse(random_numbers < (1 - validation_set_share), "train", "validation") %>% factor()
table(random_flags)

shots_train_val_list <- shots %>%
  filter(!is.na(shot_made_flag)) %>%
  bind_cols(random_flag = random_flags) %>%
  group_split(random_flag, keep = FALSE)
str(shots_train_val_list)

shots_train <- shots_train_val_list[[1]]
shots_validation <- shots_train_val_list[[2]]

## target variable
shots_train %>% count(shot_made_flag)

## features
generate_features <- function(df) {
  df <- df %>%
    mutate(
      shot_made_flag = ifelse(shot_made_flag == 1, "made", "missed") %>% 
        factor(levels = c("missed", "made")),
      home_court = ifelse(str_detect(matchup, "vs."), "home", "away") %>% 
        factor(levels = c("home", "away")),
      crunch_time_quarter = minutes_remaining < 2,
      crunch_time_game = minutes_remaining < 2 & period == 4,
      remaining_seconds_period = minutes_remaining * 60 + seconds_remaining,
      remaining_seconds_game = (5 - period) * minutes_remaining * 60 + seconds_remaining, # TODO: consider overtime!
      period = factor(period),
      opponent_season = str_c(opponent, season, sep = "-")
    )
  df
}

drop_variables <- function(df) {
  df <- df %>% 
    select(-team_id, -team_name, -matchup, -shot_id, -game_event_id, -game_id, -game_date, 
               -seconds_remaining)
  df
}

prepare_df <- function(df) {
  df <- generate_features(df) %>%
    drop_variables()
  df
}


shots_train <- prepare_df(shots_train)
shots_validation <- prepare_df(shots_validation) 
shots_test <- prepare_df(shots_test) 





## data visualisation

# location 
shots_train %>%
  ggplot(aes(loc_x, loc_y, col = combined_shot_type)) +
  geom_point(alpha = 0.5, size = 0.1) +
  theme_bw() + 
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
    ) +
  scale_color_tableau() +
  labs(title = "", x = NULL, y = NULL, col = "Combined shot type") +
  facet_wrap(vars(shot_made_flag))

# combined shot type
shots_train %>%
  ggplot(aes(combined_shot_type, fill = shot_made_flag)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_tableau() +
  labs(
    title = "Kobe's legendary bank shots", subtitle = "Shot success by combined shot type", 
    x = NULL, y = NULL, fill = "Shot success")

# action type
shots_train %>%
  nest(-action_type) %>%
  mutate(fgp = map_dbl(data, ~mean(.$shot_made_flag == "made"))) %>%
  unnest() %>%
  ggplot(aes(reorder(action_type, fgp), fill = shot_made_flag)) +
  geom_bar(position = "fill", aes(alpha = ..count..)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_tableau() +
  labs(
    title = "Kobe's legendary bank shots", subtitle = "Shot success by action type", 
    x = NULL, y = NULL, fill = "Shot success")


# shot zone
shots_train %>%
  ggplot(aes(reorder(shot_zone_basic, shot_made_flag), fill = shot_made_flag)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_hc() + 
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_tableau() +
  labs(
    title = "", subtitle = "Shot success by shot zone", 
    x = NULL, y = NULL, fill = "Shot success")

# field goal percentage by seconds remaining in quarter
shots_train %>%
  mutate(period = factor(ifelse(period %in% c("5", "6", "7"), "OT", period))) %>%
  group_by(period, remaining_seconds_period) %>%
  summarize(
    fgp = mean(shot_made_flag == "made"),
    fga = n()) %>%
  ungroup() %>%
  filter(fga > 2) %>%
  ggplot(aes(remaining_seconds_period, fgp)) +
  geom_col() +
  scale_x_reverse() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  facet_wrap(vars(period))

