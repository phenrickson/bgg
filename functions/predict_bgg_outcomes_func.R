# predict given api_games_info
predict_bgg_outcomes_func = function(input_games_info) {
        
        # # estimate bgg average 
        # estimated_average = average_workflow %>%
        #         predict(input_games_info) %>%
        #         mutate(outcome = "average") %>%
        #         mutate(.row = row_number()) %>%
        #         select(outcome, .pred, .row) %>%
        #         left_join(., input_games_info %>%
        #                           mutate(.row = row_number()) %>%
        #                           select(game_id, .row),
        #                   by = c('.row'))
        # 
        # # estimated usersrated
        # estimated_usersrated = usersrated_workflow %>%
        #         predict(input_games_info) %>%
        #         mutate(outcome = "usersrated") %>%
        #         mutate(.row = row_number()) %>%
        #         select(outcome, .pred, .row) %>%
        #         left_join(., input_games_info %>%
        #                           mutate(.row = row_number()) %>%
        #                           select(game_id, .row),
        #                   by = c('.row'))
        # 
        # estimate averageweight
        estimated_averageweight = averageweight_workflow %>%
                predict(input_games_info) %>% 
                mutate(outcome = "averageweight") %>%
                mutate(.row = row_number()) %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # now amend games info with estimate of averageweight
        input_games_estimated_averageweight  = estimated_averageweight %>%
                left_join(., input_games_info,
                          by = c("game_id")) %>%
                select(-averageweight, -.row, -outcome) %>%
                rename(averageweight = .pred)
        
        # estimated bgg average given estimated averageweight
        estimated_average= average_workflow %>%
               # predict(input_games_info) %>% 
                predict(input_games_estimated_averageweight) %>%
                mutate(.row = row_number()) %>%
                mutate(outcome = "average") %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # estimate usersrated given estimated averageweight
        estimated_usersrated = usersrated_workflow %>%
            #    predict(input_games_info) %>% 
                predict(input_games_estimated_averageweight) %>%
                mutate(.row = row_number()) %>%
                mutate(outcome = "usersrated") %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # estimated bayesaverage using previously estimated usersated and averageweight
        estimated_outcomes_xgbTree = estimated_usersrated %>%
                spread(outcome, .pred) %>%
                left_join(.,
                          estimated_average %>%
                                  spread(outcome, .pred),
                          by = c(".row", "game_id")) %>%
                mutate(usersrated = plyr::round_any(exp(usersrated), 50)) %>%
                mutate(bayesaverage = 
                               ((1000*5.5) + (usersrated*average)) /
                               (1000 + usersrated)) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row', 'game_id')) %>%
                left_join(., estimated_averageweight %>%
                                  spread(outcome, .pred),
                          by = c(".row", "game_id")) %>%
                mutate(method = "xgbTree") %>%
                select(method, everything()) %>%
                select(method, .row, game_id, average, usersrated, bayesaverage, averageweight)
        
        
        # now get estimated bayesaverage with stan
        estimated_outcomes_stan = usersrated_stan_workflow %>%
                #predict(input_games_info) %>% 
                     predict(input_games_estimated_averageweight) %>%
                mutate(.row = row_number()) %>%
                mutate(outcome = "usersrated") %>%
                select(outcome, .pred, .row) %>%
                spread(outcome, .pred) %>%
                left_join(., 
                          average_stan_workflow %>%
                          #        predict(input_games_estimated_averageweight) %>%
                          #        predict(input_games_estimated_averageweight) %>%
                                  mutate(.row = row_number()) %>%
                                  mutate(outcome = "average") %>%
                                  select(outcome, .pred, .row) %>%
                                  spread(outcome, .pred),
                          by = c(".row")) %>%
                mutate(usersrated = plyr::round_any(exp(usersrated), 50)) %>%
                mutate(bayesaverage = 
                               ((1000*5.5) + (usersrated*average)) /
                               (1000 + usersrated)) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row')) %>%
                left_join(., estimated_averageweight %>%
                                  spread(outcome, .pred),
                          by = c(".row", "game_id")) %>%
                mutate(method = "stan_lm") %>%
                select(method, .row, game_id, average, usersrated, bayesaverage, averageweight)
        
        out = list("estimated_outcomes_stan" = estimated_outcomes_stan,
                   "estimated_outcomes_xgbTree" = estimated_outcomes_xgbTree)
        
        return(out)
        
}
