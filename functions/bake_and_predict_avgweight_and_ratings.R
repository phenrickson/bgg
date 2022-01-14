bake_and_predict_avgweight_and_ratings <-
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
                
                source(here::here("functions/get_game_record.R"))
                
                # # get training set
                # all_files = list.files(here::here("deployment"))
                # files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
                # 
                # # # get dataset
                # most_recent_games = all_files[grepl("games_datasets_avgweight", all_files)] %>%
                #         as_tibble() %>%
                #         separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                #                  extra = "merge",
                #                  fill = "left") %>%
                #         unite(name, name1:name2) %>%
                #         mutate(date = as.Date(date)) %>%
                #         filter(date == max(date)) %>%
                #         unite(path, name:file) %>%
                #         mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                #         pull(path)
                # 
                # # get most recent recipe
                # most_recent_recipe = all_files[grepl("recipe_avgweight", all_files)] %>%
                #         as_tibble() %>%
                #         separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                #                  extra = "merge",
                #                  fill = "left") %>%
                #         unite(name, name1:name3) %>%
                #         mutate(date = as.Date(date)) %>%
                #         filter(date == max(date)) %>%
                #         unite(path, name:file) %>%
                #         mutate(path = gsub("_Rds", ".Rds", path)) %>%
                #         pull(path) 
                
                # use function to get record from bgg
                suppressMessages({
                        raw_record = get_game_record(id) %>%
                                mutate(number_designers = rowSums(across(starts_with("des_"))))
                })
                
                # get training set
                games_datasets_ratings = readr::read_rds(here::here("active/games_datasets_ratings.Rdata"))
                games_datasets_complexity = readr::read_rds(here::here("active/games_datasets_complexity.Rdata"))
                
                # get models
                models_ratings <- readr::read_rds(here::here("active/models_ratings.Rds"))
                models_complexity <- readr::read_rds(here::here("active/models_complexity.Rds"))
                
                # get recipes
                rec_ratings <- readr::read_rds(here::here("active/recipe_ratings.Rdata"))
                rec_complexity <- readr::read_rds(here::here("active/recipe_complexity.Rdata"))
                
                # bind
                record=   bind_rows(raw_record,
                                    games_datasets_complexity$train[0,])
                
                # bakey
                baked_record = bake(rec_complexity, record)  %>%
                        mutate_at(c("baverage"), replace_na, 0)
                
                # to json
                req = toJSON(baked_record)
                
                #
                # model =  enquo(trained_model)
                # model = rlang::sym(paste(trained_model))
                # 
                # parse example from json
                parsed_example <- jsonlite::fromJSON(req) %>%
                        mutate_if(is.integer, as.numeric) %>%
                        mutate(timestamp = as_datetime(timestamp))
                
                # get avgweight
                model_avgweight<- models_complexity %>%
                        filter(outcome_type == 'avgweight') %>%
                        select(xgbTree_fit) %>%
                        pull()
                
                # get first element
                model_avgweight = model_avgweight[[1]]
                
                # now predict
                prediction_avgweight <- predict(model_avgweight, new_data = parsed_example) %>%
                        as_tibble() %>%
                        set_names("avgweight")
                
                # now get the estimated weight
                estimated_weight = dplyr::bind_cols(parsed_example %>%
                                                            select(yearpublished, game_id, name),
                                                    prediction_avgweight) %>%
                        mutate_if(is.numeric, round, 2) %>%
                        melt(., id.vars = c("yearpublished", "game_id", "name")) %>%
                        rename(outcome = variable,
                               pred = value) %>%
                        mutate(method = "xgbTree") %>%
                        select(method, everything()) %>%
                        mutate(pred = case_when(pred > 5 ~ 5,
                                                pred < 1 ~ 1,
                                                TRUE ~ pred))
                
                # actual record
                actual_weight_and_ratings = record %>%
                        select(timestamp, yearpublished, game_id, name, average, baverage, avgweight, usersrated)
                
                # remove pieces we don't need at this point
                rm(games_datasets_complexity,
                   model_avgweight,
                   models_complexity,
                   baked_record, 
                   req)
                
                # ##### now push into models trained for baverage and average #####
                # 
                # # get training set
                # most_recent_games = all_files[grepl("games_datasets_ratings", all_files)] %>%
                #         as_tibble() %>%
                #         separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                #                  extra = "merge",
                #                  fill = "left") %>%
                #         unite(name, name1:name2) %>%
                #         mutate(date = as.Date(date)) %>%
                #         filter(dteate == max(date)) %>%
                #         unite(path, name:file) %>%
                #         mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                #         pull(path)
                # 
                # # get most recent recipe
                # most_recent_recipe = all_files[grepl("recipe_ratings", all_files)] %>%
                #         as_tibble() %>%
                #         separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                #                  extra = "merge",
                #                  fill = "left") %>%
                #         unite(name, name1:name3) %>%
                #         mutate(date = as.Date(date)) %>%
                #         filter(date == max(date)) %>%
                #         unite(path, name:file) %>%
                #         mutate(path = gsub("_Rds", ".Rds", path)) %>%
                #         pull(path)
                # 
                # # use function to get record from bgg
                # # then put in the estimated weight
                # suppressMessages({
                #         raw_record = get_game_record(id) %>%
                #                 mutate(number_designers = rowSums(across(starts_with("des_")))) %>%
                #                 mutate(avgweight = estimated_weight$pred) # adding in estimated weight
                # })
                
                # update the weight variable in the raw record
                raw_record$avgweight = estimated_weight$pred
                
                # bind raw to trained
                record=   bind_rows(raw_record,
                                    games_datasets_ratings$train[0,])
                
                # bake with rartings recie
                baked_record = bake(rec_ratings, record)
                
                # to json
                req = toJSON(baked_record)
                
                # get specified model
                model =  enquo(trained_model)
                model = rlang::sym(paste(trained_model))
                
                # parse example from json
                parsed_example <- jsonlite::fromJSON(req) %>%
                        mutate_if(is.integer, as.numeric) %>%
                        mutate(timestamp = as_datetime(timestamp))
                
                # most_recent_models = all_files[grepl("trained_models_obj", all_files)] %>%
                #         as_tibble() %>%
                #         separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                #                  extra = "merge",
                #                  fill = "left") %>%
                #         unite(name, name1:name3) %>%
                #         mutate(date = as.Date(date)) %>%
                #         filter(date == max(date)) %>%
                #         unite(path, name:file) %>%
                #         mutate(path = gsub("_Rds", ".Rds", path)) %>%
                #         pull(path)
                # 
                # # get most recent models
                # models = readr::read_rds(here::here("deployment", most_recent_models))
                # 
                # geek rating
                model_baverage <- models_ratings %>%
                        filter(outcome_type == 'baverage') %>%
                        select(!!model) %>%
                        pull()
                
                # get first element
                model_baverage = model_baverage[[1]]
                
                # average rating
                model_average <- models_ratings  %>%
                        filter(outcome_type == 'average') %>%
                        select(!!model) %>%
                        pull()
                
                # get first element
                model_average = model_average[[1]]
                
                # now predict
                prediction_baverage <- predict(model_baverage, new_data = parsed_example) %>%
                        as_tibble() %>%
                        set_names("baverage")
                
                prediction_average <- predict(model_average, new_data = parsed_example) %>%
                        as_tibble() %>%
                        set_names("average")
                
                # now combine
                estimated_rating = dplyr::bind_cols(parsed_example %>%
                                                            select(yearpublished, game_id, name),
                                                    prediction_baverage,
                                                    prediction_average) %>%
                        mutate_if(is.numeric, round, 2) %>%
                        melt(., id.vars = c("yearpublished", "game_id", "name")) %>%
                        rename(outcome = variable,
                               pred = value) %>%
                        mutate(method = paste(trained_model)) %>%
                        select(method, everything())
                
                out = list("actual_weight_and_rating" = actual_weight_and_ratings, 
                           "estimated_weight" = estimated_weight, 
                           "estimated_rating" = estimated_rating,
                           "record" = req)
                
                out
                
        }
