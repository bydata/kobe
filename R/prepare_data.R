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

