get_bgg_api_predict_average_complexity = function(input_ids) {
        
        # load functions for working with API
        source(here::here("functions/get_bgg_data_from_api.R"))
        source(here::here("functions/convert_bgg_api_data_to_tables.R"))
        
        # push through api
        api_returned = get_bgg_api_data(input_ids)
        
        # convert to tabular
        suppressMessages({
                api_tables = convert_bgg_api_data_to_tables(api_returned)
        })
        
        # get models and template
        average_workflow = readr::read_rds(here::here("experiments/final_average_xgbTree_fit.Rds"))
        complexity_workflow = readr::read_rds(here::here("experiments/final_complexity_xgbTree_fit.Rds"))
        template = readr::read_rds(here::here("experiments/final_template_average.Rds"))
        
        # convert to format for model
        input_games_info = api_tables$games_info %>%
                mutate(numweights = as.character(numweights)) %>% # for some reason numweights is coming through as a character...?
                bind_rows(., template[0,]) %>% # bind to our template format
                select(one_of(names(template))) # keep only variables in that format
        
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
                mutate(outcome = "average_estimated_complexity") %>%
                select(outcome, .pred, .row) %>%
                left_join(., input_games_info %>%
                                  mutate(.row = row_number()) %>%
                                  select(game_id, .row),
                          by = c('.row'))
        
        # get output
        out = list("api_tables" = api_tables,
                   "api_games_info" = input_games_info,
                   "estimated_average" = estimated_average,
                   "estimated_complexity" = estimated_complexity,
                   "estimated_average_complexity" = estimated_average_complexity)
        
        return(out)
        
}

# not run
#foo get_bgg_api_predict_average_complexity(12)
