devtools::install_github("BillPetti/baseballr")
library(baseballr)
library(dplyr)
library(tidyverse)
library(xgboost)
library(caret)
library(vip)
library(gt)
library(gtExtras)
library(mlbplotR)

annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}


format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

pbp <- data.frame()
for (year in 2021:2023) {
  pbp_szn <- annual_statcast_query(year)
  pbp <- rbind(pbp, pbp_szn)
}


bat_qual_2023 <- fg_batter_leaders(startseason = 2023, endseason = 2023, qual = "y") 

data <- pitches %>%
  mutate(year = as.numeric(substring(game_date, 1, 4))) %>%

batting_data <- data %>%
  group_by(year, batter) %>%
  mutate(
    woba_value = ifelse(is.na(woba_value), 0, woba_value),
    ab = row_number() - 1,
    one = cumsum(lag(events == "single", default = 0)),
    two = cumsum(lag(events == "double", default = 0)),
    three = cumsum(lag(events == "triple", default = 0)),
    hr = cumsum(lag(events == "home_run", default = 0)),
    wlk = cumsum(lag(events == "walk", default = 0)),
    so = cumsum(lag(events == "strikeout", default = 0)),
    sf = cumsum(lag((events == "sac_fly") + (events == "sac_fly_double_play"), default = 0)),
    sb = cumsum(lag((events == "sac_bunt") + (events == "sac_bunt_double_play"), default = 0))
  ) %>%
  select(batter, woba_value, ab, one, two, three, hr, wlk, so, sf, sb)

pitching_data <- data %>%
  group_by(year, pitcher) %>%
  mutate(
    p = row_number() - 1,
    hbp = cumsum(lag(events == "hit_by_pitch", default = 0)),
    hr_ag = cumsum(lag(events == "home_run", default = 0)),
    wlk_ag = cumsum(lag(events == "walk", default = 0)),
    so_ag = cumsum(lag(events == "strikeout", default = 0))
  ) %>%
  select(pitcher, p, hbp, hr_ag, wlk_ag, so_ag) 

final_data <- cbind(batting_data, pitching_data) %>%
  mutate(year = year...1) %>%
  select(-year...1, -year...13)

xgboost_train <- final_data %>%
  filter(year != 2023)

xgboost_test <- final_data %>%
  filter(year == 2023)

labels_train <- as.matrix(xgboost_train[,2])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(3:11,13:17)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(3:11,13:17)])

cwoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(cwoe_model)
vi(cwoe_model)

cw_predict <- predict(cwoe_model, xgboost_testfinal)
cw_actual <- as.matrix(xgboost_test[,2])
postResample(cw_predict, cw_actual)

cw_predictions <- as.data.frame(
  matrix(predict(cwoe_model, as.matrix(final_data[,c(3:11,13:17)])))
)

all_stats <- cbind(final_data, cw_predictions) %>%
  select(year, batter, woba_value, pred_woba = V1)

all_stats <- all_stats %>%
  group_by(year, batter) %>%
  summarize(cw = sum(woba_value), pred_cw = sum(pred_woba), cwoe = cw - pred_cw)

stats_2023 <- all_stats %>%
  filter(year == 2023, batter %in% bat_qual_2023$xMLBAMID)

qual <- bat_qual_2023 %>%
  select(batter = xMLBAMID, name = PlayerName, team = team_name)

stats_2023 <- left_join(stats_2023, qual, by = "batter")

stats_2023$team[which(stats_2023$team == "ARI")] <- "AZ"
stats_2023$team[which(stats_2023$team == "CHW")] <- "CWS"
stats_2023$team[which(stats_2023$team == "KCR")] <- "KC"
stats_2023$team[which(stats_2023$team == "SDP")] <- "SD"
stats_2023$team[which(stats_2023$team == "SFG")] <- "SF"
stats_2023$team[which(stats_2023$team == "TBR")] <- "TB"
stats_2023$team[which(stats_2023$team == "WSN")] <- "WSH"

top10 <- stats_2023 %>%
  arrange(-cwoe) %>%
  filter(row_number() <= 10) %>%
  mutate(cwoe = round(cwoe, 1)) %>%
  ungroup() %>%
  select(name, team, cwoe)

bot10 <- stats_2023 %>%
  arrange(cwoe) %>%
  filter(row_number() <= 10) %>%
  mutate(cwoe = round(cwoe, 1)) %>%
  ungroup() %>%
  select(name, team, cwoe)

t10 <- top10 %>% gt() %>% 
  gt_fmt_mlb_logo(columns = "team") %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(name, team, cwoe)
  ) %>%
  data_color(
    columns = cwoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Hitter**"),
    team = md("**Team**"),
    cwoe = md("**CWOE**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Top 10 CWOE (Cumulative wOBA Value Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Hitters Only"
  ) 

gtsave(t10, "t10.png")

b10 <- bot10 %>% gt() %>% 
  gt_fmt_mlb_logo(columns = "team") %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(name, team, cwoe)
  ) %>%
  data_color(
    columns = cwoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    name = md("**Hitter**"),
    team = md("**Team**"),
    cwoe = md("**CWOE**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Bottom 10 CWOE (Cumulative wOBA Value Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Hitters Only"
  ) 

gtsave(b10, "b10.png")

all_stats_p <- cbind(final_data, cw_predictions) %>%
  ungroup() %>%
  select(year, pitcher, woba_value, pred_woba = V1)

all_stats_p <- all_stats_p %>%
  group_by(year, pitcher) %>%
  summarize(cw = sum(woba_value), pred_cw = sum(pred_woba), cwoe = cw - pred_cw)

stats_2023_p <- all_stats_p %>%
  filter(year == 2023, pitcher %in% pitch_qual_2023$xMLBAMID)

qual_p <- pitch_qual_2023 %>%
  select(pitcher = xMLBAMID, name = PlayerName, team = team_name)

stats_2023_p <- left_join(stats_2023_p, qual_p, by = "pitcher")

stats_2023_p$team[which(stats_2023_p$team == "ARI")] <- "AZ"
stats_2023_p$team[which(stats_2023_p$team == "CHW")] <- "CWS"
stats_2023_p$team[which(stats_2023_p$team == "KCR")] <- "KC"
stats_2023_p$team[which(stats_2023_p$team == "SDP")] <- "SD"
stats_2023_p$team[which(stats_2023_p$team == "SFG")] <- "SF"
stats_2023_p$team[which(stats_2023_p$team == "TBR")] <- "TB"
stats_2023_p$team[which(stats_2023_p$team == "WSN")] <- "WSH"

top10_p <- stats_2023_p %>%
  arrange(cwoe) %>%
  filter(row_number() <= 10) %>%
  mutate(cwoe = round(cwoe, 1)) %>%
  ungroup() %>%
  select(name, team, cwoe)

bot10_p <- stats_2023_p %>%
  arrange(-cwoe) %>%
  filter(row_number() <= 10) %>%
  mutate(cwoe = round(cwoe, 1)) %>%
  ungroup() %>%
  select(name, team, cwoe)

top10_p$team[which(top10_p$name == "Justin Verlander")] <- "HOU"
bot10_p$team[which(bot10_p$name == "Lance Lynn")] <- "LAD"
bot10_p$team[which(bot10_p$name == "Lucas Giolito")] <- "CLE"

t10_p <- top10_p %>% gt() %>% 
  gt_fmt_mlb_logo(columns = "team") %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(name, team, cwoe)
  ) %>%
  data_color(
    columns = cwoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    name = md("**Pitcher**"),
    team = md("**Team**"),
    cwoe = md("**CWOE Allowed**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Top 10 CWOE Allowed (Cumulative wOBA Value Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Pitchers Only"
  ) 

gtsave(t10_p, "t10_p.png")

b10_p <- bot10_p %>% gt() %>% 
  gt_fmt_mlb_logo(columns = "team") %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(name, team, cwoe)
  ) %>%
  data_color(
    columns = cwoe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Hitter**"),
    team = md("**Team**"),
    cwoe = md("**CWOE Allowed**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Bottom 10 CWOE Allowed (Cumulative wOBA Value Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Hitters Only"
  ) 

gtsave(b10_p, "b10_p.png")