get_models_and_training_data <-
function() {
        
        # get training set
        all_files = list.files(here::here("deployment"))
        files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
        
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
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        # recipe
        rec <- readr::read_rds(here::here("deployment", most_recent_recipe))
        
        # models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        
        out = list("datasets" = games_datasets,
                   "recipe" = rec,
                   "models" = models)
        
        return(out)
        
}
