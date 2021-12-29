bake_and_predict_posterior <-
function(id) {
        
        require(tidyverse)
        require(magrittr)
        require(tidyverse)
        require(broom)
        require(data.table)
        require(readr)
        require(jsonlite)
        require(rstan)
        require(rstanarm)
        require(recipes)
        require(lubridate)
        
        id = as.integer(id)
        
        source(here::here("deployment/get_game_record.R"))
        
        # get training set
        all_files = list.files(here::here("deployment"))
        files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
        
        # # get dataset
        most_recent_games = all_files[grepl("games_datasets_ratings", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get most recent recipe
        most_recent_recipe = all_files[grepl("recipe_ratings", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # use function to get record from bgg
        suppressMessages({
                raw_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
        })
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        record=   bind_rows(raw_record,
                            games_datasets$train[0,])
        
        rec <- readr::read_rds(here::here("deployment", most_recent_recipe))
        
        baked_record = bake(rec, record)
        
        # to json
        req = toJSON(baked_record)
        
        ###### end getting record
        
        ###### start predicting
        # parse example from json
        # parsed_example <- jsonlite::fromJSON(req) %>%
        #         mutate_if(is.integer, as.numeric) %>%
        #         mutate(timestamp = as_datetime(timestamp))
        
        most_recent_models = all_files[grepl("trained_models_obj", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # get most recent models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        p <- c(0.05, .1, .2, 0.5, 0.8, .9, .95)
        p_names <- purrr::map_chr(p, ~paste0("perc_", .x*100))
        p_funs <- purrr::map(p, ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
                purrr::set_names(nm = p_names)
        
        # parsed_example <- jsonlite::fromJSON(req) %>%
        #         mutate_if(is.integer, as.numeric) %>%
        #         mutate(timestamp = as_datetime(timestamp))
        # 
        # load models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        preds_record = models %>%
                select(outcome_type, 
                       stan_lm) %>%
                mutate(stan_lm_fit = map(stan_lm,
                                         ~ .x %>% extract_fit_parsnip())) %>%
                mutate(stan_lm_posterior_preds = map2(.x = stan_lm_fit,
                                                      .y = stan_lm,
                                                      ~ .x$fit %>%
                                                              posterior_predict(.y %>% 
                                                                                        extract_recipe() %>%
                                                                                        bake(baked_record),
                                                                                draws = 1000) %>%
                                                              tidybayes::tidy_draws() %>%
                                                              reshape2::melt(id.vars = c(".chain",
                                                                               ".iteration",
                                                                               ".draw")) %>%
                                                              mutate(.row = as.integer(variable))))
        
        # melted record
        melted_record = baked_record %>%
                select(yearpublished, game_id, name, baverage, average) %>%
                mutate(.row = row_number()) %>%
                reshape2::melt(id.vars = c(".row",
                                           "yearpublished",
                                           "game_id",
                                           "name")) %>%
                rename(outcome = value,
                       outcome_type = variable)
        
        # get sims
        suppressWarnings({
                sims = preds_record %>%
                        select(outcome_type, stan_lm_posterior_preds) %>%
                        unnest() %>%
                        left_join(., melted_record %>%
                                          select(.row, outcome_type, yearpublished, game_id, name, outcome),
                                  by = c("outcome_type", ".row"))

        })
        
        # # get extra info
        # melted_record = baked_record %>%
        #         select(outcome_type, yearpublished, game_id, name, baverage, average) %>%
        #         reshape2::melt(id.vars = c("outcome_type",
        #                                    "yearpublished",
        #                                    "game_id",
        #                                    "name")) %>%
        #         rename(outcome = value,
        #                outcome_type = variable)
        
        
        # sims = sims %>%
        #         nest(-outcome_type) %>%
        #         mutate(outcome = case_when(outcome_type == 'baverage' ~ baked_record$baverage,
        #                                    TRUE ~ baked_record$average)) %>%
        #         mutate(game_id = baked_record$game_id) %>%
        #         mutate(name = baked_record$name) %>%
        #         mutate(yearpublished = baked_record$yearpublished)
        
        out = list("sims" = sims,
                   "baked_record" = baked_record,
                   "melted_record" = melted_record,
                   "record" = req)
        
        return(out)
        
}
