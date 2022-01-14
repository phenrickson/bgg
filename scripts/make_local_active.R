# load data to active folder
# set packages
source(here::here("scripts/load_packages.R"))

### get and register most recent adjusted ratings
adjusted_ratings_files = c(list.files(here::here("adjusted_bgg_ratings/data")))

# most recent adjusted ratings
most_recent_adjusted_ratings = adjusted_ratings_files %>%
        as_tibble() %>%
        separate(value, c("date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, date:file) %>%
        mutate(path = gsub("_csv", ".csv", path)) %>%
        pull(path)

### get and register active version from predict_ratings
predict_ratings_files = c(list.files(here::here("predict_ratings/data")),
                          list.files(here::here("predict_ratings/models")))

# recipe
most_recent_recipe_ratings = predict_ratings_files[grepl("prepped_recipe_ratings", predict_ratings_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# model
most_recent_models_ratings = predict_ratings_files[grepl("models_ratings", predict_ratings_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# dataset
most_recent_games_datasets_ratings = predict_ratings_files[grepl("games_datasets_ratings", predict_ratings_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# remove
rm(predict_ratings_files)

### get and register active version from predict_complexity
predict_complexity_files = c(list.files(here::here("predict_complexity/data")),
                          list.files(here::here("predict_complexity/models")))

# recipe
most_recent_recipe_complexity = predict_complexity_files[grepl("recipe_avgweight", predict_complexity_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# model
most_recent_models_complexity = predict_complexity_files[grepl("models_avgweight", predict_complexity_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# dataset
most_recent_games_datasets_complexity = predict_complexity_files[grepl("games_datasets_avgweight", predict_complexity_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name3) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# remove
rm(predict_complexity_files)

### load in unsupervised files
unsupervised_files = list.files(here::here("find_game_comparables/outputs"))

# get recipe
most_recent_unsupervised_recipe = unsupervised_files[grepl("recipe_prep", unsupervised_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# get most recent clusters
most_recent_unsupervised_clusters = unsupervised_files[grepl("unsupervised_clusters", unsupervised_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# unsupervised obj
most_recent_unsupervised_obj = unsupervised_files[grepl("unsupervised_obj", unsupervised_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        mutate(path = gsub("_Rds", ".Rds", path)) %>% 
        pull(path)

# get most recent neighbors
most_recent_unsupervised_neighbors = unsupervised_files[grepl("unsupervised_neighbors", unsupervised_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

### get flattened
local_files = list.files(here::here("local"))

most_recent_flattened= local_files[grepl("games_flattened", local_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

most_recent_games= local_files[grepl("games_datasets", local_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

### load all
# games datasets
games_datasets = readr::read_rds(here::here("local", most_recent_games))

# combine to make games
games = bind_rows(games_datasets$train,
                  games_datasets$test) %>%
        select(-mech_realtime) %>%
        select(-mech_negotiation) %>%
        select(-mech_deduction) %>%
        mutate_if(is.numeric, replace_na, 0) %>%
        filter(avgweight > 0.99) %>%
        filter(!is.na(yearpublished)) %>%
        filter(cat_expansion_for_basegame !=1) %>%
        filter(cat_fan_expansion != 1) %>%
        select(-cat_expansion_for_basegame,
               -cat_fan_expansion)
# unsupervised obj
unsupervised_obj = readr::read_rds(here::here("find_game_comparables/outputs", most_recent_unsupervised_obj)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
# neighbors
unsupervised_neighbors = readr::read_rds(here::here("find_game_comparables/outputs", most_recent_unsupervised_neighbors)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
# clusters
unsupervised_clusters = readr::read_rds(here::here("find_game_comparables/outputs", most_recent_unsupervised_clusters)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
# unsupervised recipe
recipe_prep = readr::read_rds(here::here("find_game_comparables/outputs", most_recent_unsupervised_recipe))
# flattened
games_flattened = readr::read_rds(here::here("local", most_recent_flattened)) %>%
        select(game_id,
               name,
               yearpublished,
               average,
               baverage,
               avgweight,
               playingtime,
               usersrated,
               minplayers,
               maxplayers)

# adjusted_ratings
adjusted_ratings = fread(here::here("adjusted_bgg_ratings/data", most_recent_adjusted_ratings)) %>%
        as_tibble()
models_complexity = readr::read_rds(here::here("predict_complexity/models", most_recent_models_complexity))
recipe_complexity = readr::read_rds(here::here("predict_complexity/models", most_recent_recipe_complexity))
models_ratings = readr::read_rds(here::here("predict_ratings/models", most_recent_models_ratings))
recipe_ratings = readr::read_rds(here::here("predict_ratings/models", most_recent_recipe_ratings))
games_datasets_complexity = readr::read_rds(here::here("predict_complexity/data", most_recent_games_datasets_complexity))
games_datasets_ratings = readr::read_rds(here::here("predict_ratings/data", most_recent_games_datasets_ratings))

# load others
publisher_list = readr::read_rds("local/publisher_list.Rdata")
top_designers = readr::read_rds("local/top_designers.Rdata")
top_artists = readr::read_rds("local/top_artists.Rdata")
playercounts = readr::read_rds("local/playercounts.Rdata")

# trim down to only what is needed
unsupervised_obj_light = unsupervised_obj %>%
        select(dataset, pca_with_data) %>%
        unnest()

# trim down to component loadings
unsupervised_obj_components = unsupervised_obj %>% 
        mutate(pca_components = map(pca_trained, ~ .x %>% 
                                            tidy(id = "pca"))) %>%
        select(dataset, pca_components) %>%
        unnest()

# now write all to local
readr::write_rds(games, file = here::here("active/games.Rdata"))
readr::write_rds(adjusted_ratings, file = here::here("active/adjusted_ratings.Rdata"))
readr::write_rds(games_datasets, file = here::here("active/games_datasets.Rdata"))
readr::write_rds(games_flattened, file = here::here("active/games_flattened.Rdata"))
readr::write_rds(unsupervised_obj, file = here::here("active/unsupervised_obj.Rdata"))
readr::write_rds(unsupervised_clusters, file = here::here("active/unsupervised_clusters.Rdata"))
readr::write_rds(unsupervised_obj_light, file = here::here("active/unsupervised_obj_light.Rdata"))
readr::write_rds(unsupervised_obj_components, file = here::here("active/unsupervised_obj_components.Rdata"))
readr::write_rds(unsupervised_neighbors, file = here::here("active/unsupervised_neighbors.Rdata"))
readr::write_rds(recipe_prep,  file = here::here("active/unsupervised_recipe_prep.Rdata"))
readr::write_rds(publisher_list, file = here::here("active/publisher_list.Rdata"))
readr::write_rds(top_designers, file = here::here("active/top_designers.Rdata"))
readr::write_rds(top_artists, file = here::here("active/top_artists.Rdata"))
readr::write_rds(playercounts, file = here::here("active/playercounts.Rdata"))
readr::write_rds(models_complexity, file = here::here("active/models_complexity.Rds"))
readr::write_rds(recipe_complexity, file = here::here("active/recipe_complexity.Rdata"))
readr::write_rds(models_ratings, file = here::here("active/models_ratings.Rds"))
readr::write_rds(recipe_ratings, file = here::here("active/recipe_ratings.Rdata"))
readr::write_rds(games_datasets, file = here::here("active/games_datasets.Rdata"))
readr::write_rds(games_datasets_ratings, file = here::here("active/games_datasets_ratings.Rdata"))
readr::write_rds(games_datasets_complexity, file = here::here("active/games_datasets_complexity.Rdata"))

print("files loaded to active")

# remove
rm(list=ls())

