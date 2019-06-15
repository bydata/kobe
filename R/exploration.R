library(tidyverse)
library(ggthemes)

data_path <- "input/kobe-bryant-shot-selection/"
file_name <- "data.csv"
file_path <- str_c(data_path, file_name)

shots <- read_csv(file_path)


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
      remaining_seconds_game = (5 - period) * minutes_remaining * 60 + seconds_remaining,
      period = factor(period),
      opponent_season = str_c(opponent, season, sep = "-")
    ) %>%
    mutate_if(is.character, factor)
  df
}

drop_variables <- function(df) {
  df <- df %>% 
    select(-team_id, -team_name, -matchup, -shot_id, -game_event_id, -game_id, -game_date, 
               -seconds_remaining)
  df
}


shots_train <- generate_features(shots_train) %>% drop_variables()
shots_validation <- generate_features(shots_validation) %>% drop_variables()
shots_test <- generate_features(shots_test) %>% drop_variables()





## data visualisation

# location 
shots_train %>%
  ggplot(aes(loc_x, loc_y, col = combined_shot_type)) +
  geom_point(alpha = 0.8, size = 1) +
  theme_bw() + 
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
    ) +
  scale_color_tableau() +
  labs(title = "", x = NULL, y = NULL, col = "Combined shot type") +
  facet_wrap(~ shot_made_fct)

# shot type
shots_train %>%
  ggplot(aes(reorder(combined_shot_type, shot_made_flag), fill = shot_made_flag)) +
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
    title = "Kobe's legendary bank shots", subtitle = "Shot success by shot type", 
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

