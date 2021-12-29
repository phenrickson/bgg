bake_and_predict_avgweight <-
function(id,
                                      trained_model) {
        
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
        most_recent_games = all_files[grepl("games_datasets_avgweight", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get most recent recipe
        most_recent_recipe = all_files[grepl("recipe_avgweight", all_files)] %>%
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
        
        req = toJSON(baked_record)
        
        #
        model =  enquo(trained_model)
        model = rlang::sym(paste(trained_model))
        
        # parse example from json
        parsed_example <- jsonlite::fromJSON(req) %>%
                mutate_if(is.integer, as.numeric) %>%
                mutate(timestamp = as_datetime(timestamp))
        
        most_recent_models = all_files[grepl("models_obj_avgweight", all_files)] %>%
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
        
        # geek rating
        model_avgweight<- models %>%
                filter(outcome_type == 'avgweight') %>%
                select(!!model) %>%
                pull()
        
        # get first element
        model_avgweight = model_avgweight[[1]]
        
        # now predict
        prediction_avgweight <- predict(model_avgweight, new_data = parsed_example) %>%
                as_tibble() %>%
                set_names("avgweight")
        
        # now combine
        estimate = dplyr::bind_cols(parsed_example %>%
                                            select(yearpublished, game_id, name),
                                    prediction_avgweight) %>%
                mutate_if(is.numeric, round, 2) %>%
                melt(., id.vars = c("yearpublished", "game_id", "name")) %>%
                rename(outcome = variable,
                       pred = value) %>%
                mutate(method = paste(trained_model)) %>%
                select(method, everything())
        
        out = list("estimate" = estimate,
                   "record" = req)
        
        out
        
}
