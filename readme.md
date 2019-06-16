The pipeline for analyzing Kobe Bryant’s shot selection consists of
several R scripts which have to be executed in the following order.

Load and prepare dataset
------------------------

    source("R/prepare_data.R")

    ## -- Attaching packages --------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1     v purrr   0.3.2
    ## v tibble  2.1.2     v dplyr   0.8.1
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   action_type = col_character(),
    ##   combined_shot_type = col_character(),
    ##   season = col_character(),
    ##   shot_type = col_character(),
    ##   shot_zone_area = col_character(),
    ##   shot_zone_basic = col_character(),
    ##   shot_zone_range = col_character(),
    ##   team_name = col_character(),
    ##   game_date = col_date(format = ""),
    ##   matchup = col_character(),
    ##   opponent = col_character()
    ## )

    ## See spec(...) for full column specifications.

Visual Exploratory Data Analysis
--------------------------------

    source("R/exploration.R")

Modelling
---------

    #source("R/modelling.R")
