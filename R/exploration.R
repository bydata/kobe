

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

