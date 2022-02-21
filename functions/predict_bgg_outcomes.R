# predict given api_games_info
predict_bgg_outcomes = function(input_games_info) {
        
        # estimate bgg average 
        estimated_average = average_workflow %>%
                predict(input_games_info) %>%
                mutate(outcome = "average") %>%
                mutate(.row = row_number()) %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # estimated usersrated
        estimated_usersrated = usersrated_workflow %>%
                predict(input_games_info) %>%
                mutate(outcome = "usersrated") %>%
                mutate(.row = row_number()) %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # estimate complexity
        estimated_complexity = complexity_workflow %>%
                predict(input_games_info) %>% 
                mutate(outcome = "complexity") %>%
                mutate(.row = row_number()) %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # now amend games info with estimate of complexity
        input_games_estimated_complexity  = estimated_complexity %>%
                left_join(., input_games_info,
                          by = c("game_id")) %>%
                select(-averageweight, -.row, -outcome) %>%
                rename(averageweight = .pred)
        
        # estimated bgg average given estimated complexity
        estimated_average_complexity = average_workflow %>%
                predict(input_games_estimated_complexity) %>%
                mutate(.row = row_number()) %>%
                mutate(outcome = "average_est_complexity") %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # estimate usersrated
        estimated_usersrated_complexity = usersrated_workflow %>%
                predict(input_games_estimated_complexity) %>%
                mutate(.row = row_number()) %>%
                mutate(outcome = "usersrated_est_complexity") %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        
        out = list("estimated_average" = estimated_average,
                   "estimated_usersrated" = estimated_usersrated,
                   "estimated_complexity" = estimated_complexity,
                   "estimated_average_complexity" = estimated_average_complexity,
                   "estimated_usersrated_complexity" = estimated_usersrated_complexity)
        
        return(out)
        
}
