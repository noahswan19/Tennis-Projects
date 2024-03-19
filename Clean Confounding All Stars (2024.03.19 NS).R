# Title: Confounding Tennis All-Stars
# By Noah Swan
# 3/19/2024


rm(list = ls())
gc()
cat("\014")
pacman::p_load(tidyverse,data.table,snakecase,glue,ggrepel)
`%notin%` <- Negate(`%in%`)
options(scipen = 999)

mutate_rowwise <- \(df,...){
  
  df |> 
    rowwise() |> 
    mutate(...) |> 
    ungroup()
  
}



##### Helper functions to load in data #####
get_list_files_github <- \(folder_name = "tennis_atp",
                           pattern_match = c("singles")){
  
  pattern_match <- c(pattern_match,".csv")
  
  pattern_match <- sapply(pattern_match, \(x){
    dplyr::case_when(
      x == "singles" ~ "matches_\\d+",
      .default = x
    ) 
  })
  pattern_match <- sapply(pattern_match, \(x){
    paste0("(?=.*",x,")")
  })
  pattern_match <- paste(pattern_match,collapse = "")
  
  req <- httr::GET(glue::glue("https://api.github.com/repos/JeffSackmann/{folder_name}/git/trees/master?recursive=1"))
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  indices <- grepl(pattern_match, filelist, perl = TRUE)
  filelist[indices]
  
}


# get file name to be read using some function
# filename can be set from get_list_files_github
get_file_name <- \(folder = "tennis_atp",
                   filename){
  
  glue::glue("https://raw.githubusercontent.com/JeffSackmann/{folder}/master/{filename}")
  
}


# function for getting first and last year for tour and type
get_year_max_matches <- \(tour = "atp",
                          type = "singles",
                          min_max = min){
  
  get_list_files_github(folder_name = glue::glue("tennis_{tour}"),
                        pattern_match = type) |> 
    # sapply(\(x) stringr::str_extract(x,"\\d+")) |>
    sapply(\(x) regmatches(x,regexec("\\d+",x))) |> 
    as.numeric() |>
    min_max()
  
}


# function for atp matches (singles, doubles, qual_chall, futures)
get_atp_matches <- \(years = 1968,
                     type = "singles"){
  
  if (min(years) < 0){
    return(get_atp_matches(1968) |> dplyr::filter(winner_seed == "Noah"))
  }
  type = ifelse(type == "singles","",paste0(type,"_"))
  if (type == "doubles"){
    years |> 
      lapply(\(x) get_file_name(folder = "tennis_atp",
                                filename = paste0("atp_matches_",type,x,".csv"))) |> 
      lapply(\(x) data.table::fread(x,showProgress = F)) |>
      lapply(\(x) dplyr::mutate(x,winner_seed = as.character(winner_seed),loser_seed = as.character(loser_seed))) |> 
      dplyr::bind_rows()
  } else{
    years |> 
      lapply(\(x) get_file_name(folder = "tennis_atp",
                                filename = paste0("atp_matches_",type,x,".csv"))) |> 
      lapply(\(x) data.table::fread(x,showProgress = F)) |>
      lapply(\(x) dplyr::mutate(x,winner_seed = as.character(winner_seed),loser_seed = as.character(loser_seed))) |> 
      dplyr::bind_rows()
  }
  
}
#####

##### Data Loading and processing #####
# load in matches
matches <- get_atp_matches(years = 2010:2023) |> 
  as_tibble() |> 
  mutate(
    match_id = str_c(tourney_date,"-",winner_id,"-",loser_id),
    year = substr(tourney_date,1,4)
  ) |> 
  rename_with(
    ~str_replace(.,"1st","first") |> str_replace("2nd","second")
  )




# we want the data at the level of a player-match
player_matches <- matches |> 
  mutate_rowwise(
    num_tiebreaks = str_extract_all(score,"\\(") |> pluck(1) |> length(),
    winner_tiebreaks_won = str_extract_all(score,"7-6\\(") |> pluck(1) |> length(),
    loser_tiebreaks_won = str_extract_all(score,"6-7\\(") |> pluck(1) |> length()
  ) |>
  rename_with(
    ~str_replace(.,"w_","winner_") |> str_replace("l_","loser_") |> to_snake_case(),
    .cols = !draw_size
  ) |> 
  pivot_longer(
    cols = starts_with("winner") | starts_with("loser"),
    names_to = c("result",".value"),
    names_pattern = "(.{5,6})_(.+|)"
  ) 


# how to order the matches
round_lookup <- tibble(
  round = c("BR","F","SF","R128","R16","R32","R64","SF"),
  round_date = c(7,7,6,1,4,3,2,5)
)

# add ordering for tourney_date along with other features
intermed <- player_matches |> 
  filter(
    "RR" %notin% round,
    .by = tourney_id
  ) |> 
  filter(
    !grepl("RET",score)
  ) |> 
  left_join(
    round_lookup
  ) |> 
  mutate(
    tourney_date = tourney_date*10 + round_date
  ) |> 
  mutate( # adding features
    rally_svpt = svpt - df - ace,
    first_lost = first_in - first_won,
    second_lost = svpt - first_in - second_won,
    first_in_per = first_in/svpt,
    first_won_per = first_won/(first_won+first_lost),
    second_won_per = second_won/(second_won+second_lost),
    hold_per = (sv_gms - (bp_faced-bp_saved))/sv_gms,
    svptw = first_won + second_won,
    svptw_per = svptw/svpt,
    rally_svptw = first_won + second_won - ace,
    rally_svptw_per = rally_svptw/rally_svpt,
    pts_lost_per_svgm = (first_lost+second_lost)/sv_gms,
    ace_per = ace/svpt,
    df_per = df/svpt
  ) 

# adding a bunch more features related to the opponent
player_matches_wfeat <- intermed |> 
  left_join(
    intermed |> 
      select(match_id,where(is.numeric),result),
    by = "match_id",
    relationship = "many-to-many"
  ) |> 
  filter(
    result.x != result.y
  ) |> 
  rename_with(
    ~str_replace(.,"\\.y","_opp") |> str_replace("\\.x","")
  ) |> 
  select(
    -c(id_opp,ht_opp,age_opp,round_date_opp,draw_size_opp,tourney_date_opp,
       match_num_opp,minutes_opp,best_of_opp,num_tiebreaks_opp)
  ) |> 
  mutate(
    rtgms = sv_gms_opp,
    aces_against = ace_opp,
    break_per = 1-hold_per_opp,
    bp_created = bp_faced_opp,
    bp_conversion = 1-(bp_saved_opp/bp_faced_opp),
    bp_per_gm = bp_created/rtgms,
    rpts = svpt_opp,
    rpw = rpts - first_won_opp - second_won_opp,
    rpw_per = rpw/rpts,
    first_rpw_per = 1-first_won_per_opp,
    second_rpw_per = 1-second_won_per_opp,
    rally_rp = rally_svpt_opp,
    rally_rpw = rally_rp - rally_svptw_opp,
    rally_rpw_per = rally_rpw/rally_rp
  ) |> 
  select(-contains("opp"))

# vector of match stats
match_stats <- c(
  names(player_matches_wfeat)[c(24:32,37:64)]
)



# tour-level averages for normalization
surface_avg_lookup <- player_matches_wfeat |> 
  summarise(
    across(
      all_of(match_stats),
      .fns = list(
        "avg" = ~mean(.,na.rm = T),
        "sd" = ~sd(.,na.rm = T)
      )
    ),
    .by = c(surface,year)
  ) |> 
  pivot_longer(
    cols = all_of(str_c(match_stats,"_avg")) | all_of(str_c(match_stats,"_sd")),
    names_to = "stat_name",
    values_to = c("stat_value")
  ) |> 
  mutate(
    type = str_extract(stat_name,"_(.{2,4})$",group = T),
    stat_name = str_remove(stat_name,"_(.{2,4})$")
  ) |> 
  pivot_wider(
    names_from = type,
    values_from = stat_value
  )

#####

##### Variance Methodology 1 #####
# players from podcast
players_of_int <-  c(
  "Alexander Zverev",
  "Hubert Hurkacz",
  "Grigor Dimitrov",
  "Jannik Sinner",
  "Felix Auger Aliassime",
  "Alejandro Davidovich Fokina",
  "Roman Safiullin",
  "Maxime Cressy"
)

# variance, but divide by mean for the surface of a given year
player_vars <- player_matches_wfeat |> 
  select(
    name,year,surface,contains("per")
  ) |> 
  pivot_longer(
    cols = contains("per"),
    names_to = "stat_name",
    values_to = "value"
  ) |> 
  left_join(
    surface_avg_lookup
  ) |> 
  mutate(
    value = (value-avg)/sd,
    .keep = "unused"
  ) |> 
  summarise(
    var_stat = var(value,na.rm = T),
    num_matches = n(),
    .by = c(name,year,stat_name)
  ) |> 
  pivot_wider(
    names_from = stat_name,
    values_from = var_stat
  ) |> 
  filter(
    num_matches >= 10,
    year == 2023
  ) |> 
  mutate(
    across(
      contains("per"),
      .fns = list(
        "percentile" = cume_dist, # get percentile of variance
        "rank" = ~dense_rank(desc(.)) # get rank of player
      )
    )
  )

# first plot highlighting podcast players
var_plot <- player_vars |> 
  select(
    name,num_matches, contains("hold_per"),contains("break_per")
  ) |> 
  ggplot(
    aes(
      x = hold_per_percentile,
      y = break_per_percentile,
      label = ifelse(
        name %in% players_of_int,
        name,
        ""
      ),
      alpha = ifelse(
        name %in% players_of_int,
        1,
        0.2
      )
    )
  )+
  geom_point(
    aes(
      color = as.factor(if_else(
        name %in% players_of_int,
        "red",
        "black"
      ))
    )
  )+
  scale_color_identity()+
  scale_alpha_identity()+
  geom_text_repel(
    max.overlaps = 1000,
    min.segment.length = 0
  )+
  theme_classic()+
  xlab("Hold Variance Percentile")+
  ylab("Break Variance Percentile")+
  ggtitle("ATP Players by Hold and Break Variance",
          subtitle = "Min. 10 Matches - Normalized by Surface")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )+
  geom_hline(aes(alpha = 0.35,yintercept = 0.5))+
  geom_vline(aes(alpha = 0.35,xintercept = 0.5))+
  geom_text(
    label = "Avg Variance",
    aes(x = .575, y = -.12),
    size = 3
  )+
  geom_text(
    label = "Avg Variance",
    aes(x = .03,y = .52),
    size = 3
  )

# ggsave(
#   "Variance Plot Players of Interest.png",
#   var_plot,
#   width = 8,
#   height = 8*4/6
# )


# plot to show new players in 85th percentile or higher
new_var_plot <- player_vars |> 
  select(
    name,num_matches, contains("hold_per"),contains("break_per")
  ) |> 
  ggplot(
    aes(
      x = hold_per_percentile,
      y = break_per_percentile,
      label = ifelse(
        hold_per_percentile >= .85 & break_per_percentile >= .85,
        name,
        ""
      ),
      alpha = ifelse(
        hold_per_percentile >= .85 & break_per_percentile >= .85,
        1,
        0.2
      )
    )
  )+
  geom_point(
    aes(
      color = as.factor(if_else(
        hold_per_percentile >= .85 & break_per_percentile >= .85,
        "red",
        "black"
      ))
    )
  )+
  scale_color_identity()+
  scale_alpha_identity()+
  geom_text_repel(
    max.overlaps = 1000,
    min.segment.length = 0
  )+
  theme_classic()+
  xlab("Hold Variance Percentile")+
  ylab("Break Variance Percentile")+
  ggtitle("ATP Players by Hold and Break Variance",
          subtitle = "Min. 10 Matches - Normalized by Surface")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )+
  geom_hline(aes(alpha = 0.35,yintercept = 0.5))+
  geom_vline(aes(alpha = 0.35,xintercept = 0.5))+
  geom_text(
    label = "Avg Variance",
    aes(x = .578, y = -.05),
    size = 3
  )+
  geom_text(
    label = "Avg Variance",
    aes(x = 0,y = .52),
    size = 3
  )+
  xlim(c(-.05,1.2))+
  ylim(c(-.05,1.2))

# ggsave(
#   "Variance Plot New Players.png",
#   new_var_plot,
#   width = 8,
#   height = 8*4/6
# )

# pull information for Zverev
player_vars |> 
  filter(
    name == "Alexander Zverev"
  ) |> 
  select(
    name,first_won_per_rank,df_per_rank,second_won_per_rank,first_in_per_rank
  )

#####

##### Modeling Methodology 2 #####
model_df <- player_matches_wfeat

# get rid of redundant features
model_features <- match_stats[
  -c(1:2,4:6,17,19,21,31,35)
]

# cummulative avg in given year for a given player as the inputs
model_variants <- model_df |>
  select(
    name,year,tourney_date,surface,match_id,rank_points,
    all_of(model_features)
  ) |> 
  mutate(
    across(
      all_of(model_features), # get lagged values for each feature
      ~lag(.,order_by = tourney_date),
      .names = "lag_{.col}"
    ),
    .by = c(name,year)
  ) |> 
  filter( # remove any missing stuff, including first match of year
    !if_any(everything(),is.na)
  ) |>
  arrange(name,year,tourney_date) |> 
  mutate(
    across(
      starts_with("lag"), # get yearly cummulative mean
      ~cummean(.),
      .names = "cum_{.col}"
    ),
    .by = c(name,year)
  ) |> 
  left_join(# add opponent rank information
    model_df |> 
      select(match_id,name,rank_points) |> 
      rename_with(~str_c(.,"_opp")),
    join_by(match_id == match_id_opp)
  ) |> 
  filter(
    name == name_opp
  ) |> 
  select(-name_opp) |> 
  filter(!is.na(rank_points_opp)) |> 
  fastDummies::dummy_cols("surface") |> 
  mutate(
    dataset_class = case_when(
      year < 2022 ~ "training",
      year == 2022 ~ "test",
      T ~ "analysis"
    )
  ) |> 
  nest(
    .by = dataset_class
  ) |> 
  expand_grid(
    outcome = model_features
  ) |> 
  mutate_rowwise(
    data = list(
      data |> 
        select(name,year,tourney_date,match_id,
               all_of(outcome),starts_with("lag"),contains("rank_points"),
               starts_with("surface_")) |> 
        rename_with(
          ~str_replace(.,".+","outcome"),
          .cols = !starts_with("lag") & !c("name","year","tourney_date","match_id","rank_points_opp","rank_points") &
            !starts_with("surface_")
        )
    )
  ) |> 
  pivot_wider(
    names_from = dataset_class,
    values_from = data
  ) |> 
  filter(
    outcome %in% c(
      "first_won_per",
      "second_won_per",
      "bp_faced",
      "hold_per",
      "first_rpw_per",
      "second_rpw_per",
      "break_per",
      "bp_created"
    )
  )

# model variants includes training, test, and analysis datasets for different models


# model tune grid for variations
set.seed(1234)
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 400, by = 100), # tuning the parameters
  eta = c(0.025, 0.05, 0.15),
  max_depth = c(3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds
  verboseIter = FALSE, # no training log
)

# function to create model

create_model <- \(train_x,
                  train_y){
  set.seed(1234)
  train_results <- caret::train(
    x = train_x,
    y = train_y,
    trControl = tune_control,
    tuneGrid = tune_grid,
    method = "xgbTree",
    verbose = TRUE,
    verbosity = 0
  )
  
  set.seed(1234)
  fin_model <- caret::train(
    x = train_x,
    y = train_y,
    trControl = tune_control,
    tuneGrid = train_results$bestTune,
    method = "xgbTree",
    verbose = TRUE,
    verbosity = 0
  )
  
  return(fin_model)
  
}

# train the final models
set.seed(1234)
fin_models <- model_variants |>
  mutate_rowwise(
    fin_model = list(
      create_model(
        train_x = training |> select(6:32),
        train_y = training |> pull(outcome)
      )
    ),
    test_rmse = list(
      Metrics::rmse(
        test |> pull(outcome),
        predict(fin_model,test |> select(6:32))
      )
    ),
    analysis = list(
      analysis |>
        mutate(
          pred_outcome = predict(fin_model,analysis |> select(6:32)),
          over_exp = outcome-pred_outcome
        )
    )
  )


# create dataframe that doesn't include the actual model object in the columns
# intermediate step to creating final visualizations
analysis_df <- fin_models |> 
  mutate_rowwise(
    test_rmse = Metrics::rmse(
      test |> pull(outcome),
      predict(fin_model,test |> select(6:32))
    ),
    avg_test_outcome = mean(
      test$outcome
    ),
    range_test_outcome = max(test$outcome) - min(test$outcome)
  ) |> 
  select(
    -c(training,test)
  ) |> 
  mutate_rowwise(
    over_exp = list(
      analysis |> 
        summarise(
          perf_over_exp = sum(over_exp,na.rm = T),
          avg_perf_over_exp = mean(over_exp,na.rm = T),
          abs_error = sum(abs(over_exp),na.rm = T),
          abs_diff_exp = mean(abs(over_exp),na.rm = T),
          sample = n(),
          .by = name
        ) |> 
        arrange(
          -perf_over_exp
        ) |> 
        filter(sample >= 15)
    )
  ) |> 
  filter(
    outcome %in% c(
      "hold_per","break_per",
      "first_won_per","second_won_per",
      "first_rpw_per","second_rpw_per",
      "rally_svptw_per","rally_rpw_per"
    )
  ) |> 
  select(-fin_model)


# hold % overperformers
top_hold_overperf <- analysis_df |> 
  mutate_rowwise(
    absolute_rankings = list(
      analysis |> 
        summarise(
          avg_over_exp = mean(over_exp),
          n_matches = n(),
          .by = name
        ) |> 
        filter(n_matches >= 10) |> 
        arrange(-avg_over_exp) |> 
        mutate(
          rank = row_number()
        ) 
    )
  ) |> 
  slice(3) |> 
  pull(absolute_rankings) |> 
  pluck(1) |> 
  slice(1:10) |> 
  ggplot(
    aes(x = reorder(name,-avg_over_exp),y = avg_over_exp*100)
  )+
  geom_col(fill = "#8997AF")+
  theme_classic()+
  ggtitle(
    "Top 10 Players Beating Model Expectation for Hold %",
    subtitle = "Min. 10 Matches"
  )+
  ylab("Avg %age Points Above Expectation")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )+
  geom_text(
    aes(
      label = name,
      angle = 90
    ),
    position = position_stack(vjust = 0.5)
  )


# ggsave(
#   "Hold Model OverPerform.png",
#   top_hold_overperf,
#   width = 8,
#   height = 8*4/6
# )

# break % overperformers
top_break_overperf <- analysis_df |> 
  mutate_rowwise(
    absolute_rankings = list(
      analysis |> 
        summarise(
          avg_over_exp = mean(over_exp),
          n_matches = n(),
          .by = name
        ) |> 
        filter(n_matches >= 10) |> 
        arrange(-avg_over_exp) |> 
        mutate(
          rank = row_number()
        ) 
    )
  ) |> 
  slice(4) |> 
  pull(absolute_rankings) |> 
  pluck(1) |> 
  slice(1:10) |> 
  mutate(
    name = if_else(
      name == "Alejandro Davidovich Fokina",
      "Alejandro Davidovich\nFokina",
      name
    )
  ) |> 
  ggplot(
    aes(x = reorder(name,-avg_over_exp),y = avg_over_exp*100)
  )+
  geom_col(fill = "#8997AF")+
  theme_classic()+
  ggtitle(
    "Top 10 Players Beating Model Expectation for Break %",
    subtitle = "Min. 10 Matches"
  )+
  ylab("Avg %age Points Above Expectation")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )+
  geom_text(
    aes(
      label = name,
      angle = 90
    ),
    position = position_stack(vjust = 0.5)
  )

# ggsave(
#   "Break Model OverPerform.png",
#   top_break_overperf,
#   width = 8,
#   height = 8*4/6
# )



# chaos charts

# furthest from expectation hold %
top_hold_chaos <- analysis_df |> 
  mutate_rowwise(
    absolute_rankings = list(
      analysis |> 
        summarise(
          avg_over_exp = mean(abs(over_exp)),
          n_matches = n(),
          .by = name
        ) |> 
        filter(n_matches >= 10) |> 
        arrange(-avg_over_exp) |> 
        mutate(
          rank = row_number()
        ) 
    )
  ) |> 
  slice(3) |> 
  pull(absolute_rankings) |> 
  pluck(1) |> 
  slice(1:10) |> 
  ggplot(
    aes(x = reorder(name,-avg_over_exp),y = avg_over_exp*100)
  )+
  geom_col(fill = "#AFA189")+
  theme_classic()+
  ggtitle(
    "Top 10 Players Furthest From Model Expectation for Hold %",
    subtitle = "Min. 10 Matches"
  )+
  ylab("Avg %age Points From Expectation")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )+
  geom_text(
    aes(
      label = name,
      angle = 90
    ),
    position = position_stack(vjust = 0.5)
  )


# ggsave(
#   "Hold Model Chaos.png",
#   top_hold_chaos,
#   width = 8,
#   height = 8*4/6
# )

# furthest from expectation break %
top_break_chaos <- analysis_df |> 
  mutate_rowwise(
    absolute_rankings = list(
      analysis |> 
        summarise(
          avg_over_exp = mean(abs(over_exp)),
          n_matches = n(),
          .by = name
        ) |> 
        filter(n_matches >= 10) |> 
        arrange(-avg_over_exp) |> 
        mutate(
          rank = row_number()
        ) 
    )
  ) |> 
  slice(4) |> 
  pull(absolute_rankings) |> 
  pluck(1) |> 
  slice(1:10) |> 
  mutate(
    name = if_else(
      name == "Alejandro Davidovich Fokina",
      "Alejandro Davidovich\nFokina",
      name
    )
  ) |> 
  ggplot(
    aes(x = reorder(name,-avg_over_exp),y = avg_over_exp*100)
  )+
  geom_col(fill = "#AFA189")+
  theme_classic()+
  ggtitle(
    "Top 10 Players Furthest From Model Expectation for Break %",
    subtitle = "Min. 10 Matches"
  )+
  ylab("Avg %age Points From Expectation")+
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )+
  geom_text(
    aes(
      label = name,
      angle = 90
    ),
    position = position_stack(vjust = 0.5)
  )

# ggsave(
#   "Break Model Chaos.png",
#   top_break_chaos,
#   width = 8,
#   height = 8*4/6
# )
#####