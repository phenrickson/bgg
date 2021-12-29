# who: phil henrickson
# what: functions to be used in project

# function to read data from github repository
get_bgg_data_from_github<-function(input_date) {
        
        url = paste("https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/", input_date, ".csv", sep="")
        
        data <- read_csv(url,
                         show_col_types = F) %>%
                mutate(date = input_date,
                       ID = as.integer(ID),
                       github_url = url) %>%
                rename(game_id = ID,
                       game_name = Name,
                       game_release_year = Year,
                       bgg_rank = Rank,
                       bgg_average = Average,
                       bayes_average = `Bayes average`,
                       users_rated = `Users rated`,
                       bgg_url = URL,
                       thumbnail = Thumbnail) %>%
                select(date, everything())
        
        return(data)
        
}

# dump
dump("get_bgg_data_from_github", file="functions/get_bgg_data_from_github.R")

get_collection <- function(username_string) {
        
        source(here::here("functions/retry.R"))
        
        # get collection data from specified users
        collection_obj<- suppressWarnings({
                        retry(bggCollection$new(username = username_string),
                                                 maxErrors = 5,
                                                 sleep=10)
                        })
        
        # expand
        collection_obj$expand(variable_names = c("name",
                                                 "type",
                                                 "yearpublished",
                                                 "rating",
                                                 "numplays",
                                                 "own",
                                                 "preordered",
                                                 "prevowned",
                                                 "fortrade",
                                                 "want",
                                                 "wanttoplay",
                                                 "wanttobuy",
                                                 "wishlist",
                                                 "wishlistpriority"))
        
        # convert to dataframe
        collection_data<-collection_obj$data %>%
                rename(game_id = objectid) %>%
                mutate(username = username_string,
                       date = Sys.Date(),
                       name = gsub(",", " ", name, fixed = T)) %>%
                mutate_if(is.logical, .funs = ~ case_when(. == T ~ 1,
                                                          .== F ~ 0)) %>%
                select(username,
                       date,
                       game_id,
                       type,
                       rating,
                       own,
                       preordered,
                       prevowned,
                       fortrade,
                       want,
                       wanttoplay,
                       wanttobuy,
                       wishlist,
                       wishlistpriority)
        
        # check for duplicates
        dupes = which(duplicated(collection_data$game_id)==T)
        
        if (length(dupes) > 0) {
                collection_data_out = collection_data[-dupes]
        } else {
                collection_data_out = collection_data
        }
        
        # convert to tibble
        collection_data_out = collection_data_out %>%
                as_tibble()
        
        return(collection_data_out)
        
}

# dump
dump("get_collection", file="functions/get_collection.R")


# function for getting game record
# function for grabbing one game and getting its data in one record for model
get_game_record<-function(insert_id) {
        
        if(!require(tidyverse) ){ cat("function requires tidyverse package")}
        if(!require(magrittr) ){ cat("function requires magrittr package")}
        if(!require(splitstackshape) ){ cat("function requires splitstackshape package")}
        if(!require(bggAnalytics) ){ cat("function requires bggAnalytics package")}
        
        # push ID through API
        games_obj<-bggGames$new(ids = insert_id,
                                chunk_size=500)
        
        # expand the resulting pull from the API
        # takes about 10 min?
        games_obj$expand(variable_names = c(
                "objectid",
                "name",
                "type",
                "rank",
                "yearpublished",
                "average",
                "baverage",
                "stddev",
                "usersrated",
                "avgweight",
                "weightvotes",
                "numtrading",
                "numwanting",
                "numwishing",
                "numcomments",
                "minplayers",
                "maxplayers",
                "recplayers",
                "bestplayers",
                "playingtime",
                "minplaytime",
                "maxplaytime",
                "minage",
                "description",
                "mechanics",
                "mechanicsid",
                "category", 
                "categoryid",
                "publishers", 
                "publishersid", 
                "designers",
                "designersid",
                "artists",
                "artistsid",
                "expansions",
                "expansionsid")
        )
        
        # get xml
        games_xml<-games_obj$xml
        
        ### Getting data ready for model
        # the flattened out data, which contains concatenated strings
        games_data<-games_obj$data %>%
                as_tibble() %>%
                rename(game_id = objectid) %>%
                mutate(recplayers = gsub("\"", "", recplayers)) %>%
                mutate(timestamp = games_obj$timestamp)
        
        # next, we want the constituent pieces flattened out to create our data model
        games_list<-games_obj$fetch(c("objectid",
                                      "mechanics",
                                      "mechanicsid",
                                      "category",
                                      "categoryid",
                                      "publishers",
                                      "publishersid",
                                      "designers",
                                      "designersid",
                                      "artists",
                                      "artistsid",
                                      "expansions",
                                      "expansionsid",
                                      "recplayers",
                                      "bestplayers"))
        
        # convert to data frame of lists
        # takes about 20 minutes
        df_list<-as_tibble(do.call(cbind, games_list)) %>%
                rename(game_id = objectid,
                       mechanic = mechanics,
                       mechanic_id = mechanicsid,
                       category = category,
                       category_id = categoryid,
                       publisher = publishers,
                       publisher_id = publishersid,
                       designer = designers,
                       designer_id = designersid,
                       artist = artists,
                       artist_id = artistsid,
                       expansion = expansions,
                       expansion_id = expansionsid) %>%
                select(game_id, everything())
        
        # game and categors
        game_categories <- df_list %>%
                select(game_id, category_id, category) %>%
                unnest(cols = c("game_id", "category_id", "category")) %>%
                arrange(game_id, category_id)
        
        # game and mechanics
        game_mechanics <- df_list %>%
                select(game_id, mechanic_id, mechanic) %>%
                unnest(cols = c("game_id", "mechanic_id", "mechanic")) %>%
                arrange(game_id, mechanic_id)
        
        # game and designers
        game_designers <- df_list %>%
                select(game_id, designer_id, designer) %>%
                unnest(cols = c("game_id", "designer_id", "designer")) %>%
                arrange(game_id, designer_id, designer)
        
        # game and publishers
        game_publishers <- df_list %>%
                select(game_id, publisher_id, publisher) %>%
                unnest(cols = c("game_id", "publisher_id", "publisher")) %>%
                arrange(game_id, publisher_id)
        
        # game and publishers
        game_artists <- df_list %>%
                select(game_id, artist_id, artist) %>%
                unnest(cols = c("game_id", "artist_id", "artist")) %>%
                arrange(game_id, artist_id)
        
        ### daily pull of games data with timestamp
        game_daily<-games_data %>%
                select(game_id, 
                       name, 
                       type, 
                       yearpublished, 
                       rank, 
                       average, 
                       baverage, 
                       stddev, 
                       usersrated, 
                       avgweight, 
                       minplayers, 
                       maxplayers, 
                       playingtime,
                       minplaytime, 
                       maxplaytime, 
                       minage, 
                       numtrading, 
                       numwanting, 
                       numwishing, 
                       numcomments, 
                       timestamp)
        # pivots
        # categories
        if (nrow(game_categories) == 0) {categories_pivot = data.frame(game_id = games_data$game_id)} else {
                
                categories_pivot <- game_categories %>%
                        mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                        mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                        mutate(has_category = 1) %>%
                        select(-category_id) %>%
                        pivot_wider(names_from = c("category"),
                                    values_from = c("has_category"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # mechanics
        if (nrow(game_mechanics) == 0) {mechanics_pivot = data.frame(game_id = games_data$game_id)} else {
                mechanics_pivot = game_mechanics %>%
                        mutate(mechanic = gsub("\\)", "", gsub("\\(", "", mechanic))) %>%
                        mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                        mutate(has_mechanic = 1) %>%
                        select(-mechanic_id) %>%
                        pivot_wider(names_from = c("mechanic"),
                                    values_from = c("has_mechanic"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # designers
        if (nrow(game_designers) == 0) {designers_pivot = data.frame(game_id = games_data$game_id)} else {
                designers_pivot = game_designers %>%
                        mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                        mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                        mutate(has_designer = 1) %>%
                        select(-designer_id) %>%
                        pivot_wider(names_from = c("designer"),
                                    values_from = c("has_designer"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # publishers
        if (nrow(game_publishers) == 0) {publishers_pivot = data.frame(game_id = games_data$game_id)} else {
                publishers_pivot = game_publishers %>%
                        mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                        mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                        mutate(has_publisher = 1) %>%
                        select(-publisher_id) %>%
                        pivot_wider(names_from = c("publisher"),
                                    values_from = c("has_publisher"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # artists
        if (nrow(game_artists) == 0) {artists_pivot = data.frame(game_id = games_data$game_id)} else {
                artists_pivot = game_artists %>%
                        mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                        mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                        mutate(has_artist = 1) %>%
                        select(-artist_id) %>%
                        pivot_wider(names_from = c("artist"),
                                    values_from = c("has_artist"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        
        # combine into one output
        game_out <- game_daily %>%
                left_join(., categories_pivot,
                          by = "game_id") %>%
                left_join(., mechanics_pivot,
                          by = "game_id") %>%
                left_join(., designers_pivot,
                          by = "game_id") %>%
                left_join(., publishers_pivot,
                          by = "game_id") %>%
                left_join(., artists_pivot,
                          by = "game_id")
        
        return(game_out)
        
}

# dump
dump("get_game_record", file="functions/get_game_record.R")

# function for creating training and test sets from gcp data model
combine_and_split_bgg_datasets = function(datasets_list,
                                   min_users,
                                   year_split,
                                   publisher_list,
                                   top_designers,
                                   top_artists
) {
        
        # combine all
        train = datasets_list$active_games %>%
                select(timestamp, game_id, name, average, baverage, usersrated) %>%
                filter(usersrated > min_users) %>%
                left_join(., games_info %>% # join game info
                                  select(game_id, yearpublished, avgweight, minage, minplayers, maxplayers, playingtime),
                          by = c("game_id")) %>%
                filter(yearpublished <= year_split) %>% # use games prior to 2020 as our training set
                left_join(., game_categories %>% # join categories
                                  mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                                  mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                                  mutate(has_category = 1) %>%
                                  select(-category_id) %>%
                                  pivot_wider(names_from = c("category"),
                                              values_from = c("has_category"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_mechanics %>% # join mechanics
                                  mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                                  mutate(has_mechanic = 1) %>%
                                  select(-mechanic_id) %>%
                                  pivot_wider(names_from = c("mechanic"),
                                              values_from = c("has_mechanic"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_designers %>% # join designers
                                  filter(designer_id %in% top_designers$designer_id) %>%
                                  mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                                  mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                                  mutate(has_designer = 1) %>%
                                  select(-designer_id) %>%
                                  pivot_wider(names_from = c("designer"),
                                              values_from = c("has_designer"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                # get number of designers
                left_join(., datasets_list$game_designers %>% 
                                  group_by(game_id) %>%
                                  summarize(number_designers = n_distinct(designer_id)),
                          by = c("game_id")) %>%
                mutate(number_designers = replace_na(number_designers, 0)) %>%
                left_join(., datasets_list$game_publishers %>% # join publishers
                                  filter(publisher_id %in% publisher_list) %>%
                                  mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                                  mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                                  mutate(has_publisher = 1) %>%
                                  select(-publisher_id) %>%
                                  pivot_wider(names_from = c("publisher"),
                                              values_from = c("has_publisher"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., 
                          datasets_list$game_artists %>%
                                  filter(artist_id %in% top_artists$artist_id) %>%
                                  mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                                  mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                                  mutate(has_artist = 1) %>%
                                  select(-artist_id) %>%
                                  pivot_wider(names_from = c("artist"),
                                              values_from = c("has_artist"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id"))
        
        
        # combine all
        test = datasets_list$active_games %>%
                select(timestamp, game_id, name, average, baverage, usersrated) %>%
                left_join(., games_info %>% # join game info
                                  select(game_id, yearpublished, avgweight, minage, minplayers, maxplayers, playingtime),
                          by = c("game_id")) %>%
                filter(yearpublished > year_split) %>% 
                left_join(., game_categories %>% # join categories
                                  mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                                  mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                                  mutate(has_category = 1) %>%
                                  select(-category_id) %>%
                                  pivot_wider(names_from = c("category"),
                                              values_from = c("has_category"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_mechanics %>% # join mechanics
                                  mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                                  mutate(has_mechanic = 1) %>%
                                  select(-mechanic_id) %>%
                                  pivot_wider(names_from = c("mechanic"),
                                              values_from = c("has_mechanic"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_designers %>% # join designers
                                  filter(designer_id %in% top_designers$designer_id) %>%
                                  mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                                  mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                                  mutate(has_designer = 1) %>%
                                  select(-designer_id) %>%
                                  pivot_wider(names_from = c("designer"),
                                              values_from = c("has_designer"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                # get number of designers
                left_join(., datasets_list$game_designers %>% 
                                  group_by(game_id) %>%
                                  summarize(number_designers = n_distinct(designer_id)),
                          by = c("game_id")) %>%
                mutate(number_designers = replace_na(number_designers, 0)) %>%
                left_join(., datasets_list$game_publishers %>% # join publishers
                                  filter(publisher_id %in% publisher_list) %>%
                                  mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                                  mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                                  mutate(has_publisher = 1) %>%
                                  select(-publisher_id) %>%
                                  pivot_wider(names_from = c("publisher"),
                                              values_from = c("has_publisher"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., 
                          datasets_list$game_artists %>%
                                  filter(artist_id %in% top_artists$artist_id) %>%
                                  mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                                  mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                                  mutate(has_artist = 1) %>%
                                  select(-artist_id) %>%
                                  pivot_wider(names_from = c("artist"),
                                              values_from = c("has_artist"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id"))
        
        out = list("train" = train,
                   "test" = test)
        
        return(out)
        
}
dump("combine_and_split_bgg_datasets", file="functions/combine_and_split_bgg_datasets.R")

# function for predicting and baking ratings
bake_and_predict_ratings<- function(id,
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
        # most_recent_games = all_files[grepl("games_datasets_ratings", all_files)] %>%
        #         as_tibble() %>%
        #         separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
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
        
        # use function to get record from bgg
        suppressMessages({
                raw_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
        })
        
        # get data used in model development
        games_datasets = readr::read_rds(here::here("active/games_datasets_ratings.Rdata"))
        # get models
        models = readr::read_rds(here::here("active/models_ratings.Rds"))
        # get recipe
        rec <- readr::read_rds(here::here("active/recipe_ratings.Rdata"))
        
        # bind to record
        record=   bind_rows(raw_record,
                            games_datasets$train[0,])
        
        # bake
        baked_record = bake(rec, record)
        
        # to json... do i even need to do this/
        req = toJSON(baked_record)
        
        # specify input model
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
        
        # get most recent models
        # models = readr::read_rds(here::here("deployment", most_recent_models))
        
        # geek rating
        model_baverage <- models %>%
                filter(outcome_type == 'baverage') %>%
                select(!!model) %>%
                pull()
        
        # get first element
        model_baverage = model_baverage[[1]]
        
        # average rating
        model_average <- models %>%
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
        estimate = dplyr::bind_cols(parsed_example %>%
                                            select(yearpublished, game_id, name),
                                    prediction_baverage,
                                    prediction_average) %>%
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

dump("bake_and_predict_ratings", file="functions/bake_and_predict_ratings.R")

bake_and_predict_posterior <- function(id) {
        
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

dump("bake_and_predict_posterior", file="functions/bake_and_predict_posterior.R")

plot_posterior = function(posterior_preds) {
        
        ### get background data for context
        # get training set
        all_files = list.files(here::here("deployment"))
        
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
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        # quantiles
        baverage_quantiles = quantile(games_datasets$train$baverage, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>%
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'baverage')
        
        # average quanties
        average_quantiles = quantile(games_datasets$train$average, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>% 
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'average')
        
        quantile_data = list("baverage" = baverage_quantiles,
                             "average" = average_quantiles)
        
        # make a dummy plot
        dummy = data.frame(outcome_type = "baverage",
                           min = 5,
                           max = 9) %>%
                bind_rows(data.frame(outcome_type = "average",
                                     min = 4,
                                     max = 9)) %>%
                melt(id.vars = c("outcome_type")) %>%
                set_names(., c("outcome_type", "variable", "range"))
        
        quantile_dummy = bind_rows(quantile_data$baverage %>%
                                           filter(perc %in% c(50, 90, 99)),
                                   quantile_data$average %>%
                                           filter(perc %in% c(50, 90, 99)))
        
        # make dummy plot
        dummy_plot = ggplot(dummy, aes(x=range))+
                facet_wrap(~outcome_type,
                           ncol = 1,
                           scales = "free_x")+
                theme_phil()+
                xlab("Predicted Value")+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 50),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 90),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 99),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 50),
                          aes(x=value,
                              y = 60),
                          size =2,
                          label = 'median game on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 90),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 10% on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 99),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 1% on bgg')+
                #   coord_cartesian(xlim = c(4, 10))+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())
        
        
        #### add sims from game to plot
        plot_obj = posterior_preds$sims 
        # plot
        plot = dummy_plot + 
                geom_histogram(data = plot_obj,
                               aes(x=value),
                               bins = 100,
                               alpha = 0.7,
                               fill = 'grey60',
                               color = '#F0F0F0')+
                geom_vline(data = plot_obj,
                           aes(xintercept=outcome),
                           alpha=0.9,
                           col = "blue")+
                geom_text(data = plot_obj,
                          aes(x=outcome),
                          label = "current bgg rating",
                          size = 2,
                          y= 30,
                          alpha=0.9,
                          col = "blue")+
                facet_wrap(name~outcome_type,
                           ncol = 1)+
                xlab("Predicted Value")+
                ylab("# of Simulations")+
                coord_cartesian(xlim = c(4, 10),
                                default = T)
        
        
        return(plot)
        
}

dump("plot_posterior", file = "functions/plot_posterior.R")


get_models_and_training_data = function() {
        
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

dump("get_models_and_training_data", file = "functions/get_models_and_training_data.R")

# # function for predicting and baking ratings
# bake_and_predict_ratings<- function(id,
#                                     trained_model) {
#         
#         require(tidyverse)
#         require(magrittr)
#         require(tidyverse)
#         require(broom)
#         require(data.table)
#         require(readr)
#         require(jsonlite)
#         require(rstan)
#         require(rstanarm)
#         require(recipes)
#         require(lubridate)
#         
#         id = as.integer(id)
#         
#         source(here::here("deployment/get_game_record.R"))
#         
#         # get training set
#         all_files = list.files(here::here("deployment"))
#         files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
#         
#         # # get dataset
#         most_recent_games = all_files[grepl("games_datasets_ratings", all_files)] %>%
#                 as_tibble() %>%
#                 separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
#                          extra = "merge",
#                          fill = "left") %>%
#                 unite(name, name1:name2) %>%
#                 mutate(date = as.Date(date)) %>%
#                 filter(date == max(date)) %>%
#                 unite(path, name:file) %>%
#                 mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
#                 pull(path)
#         
#         # get most recent recipe
#         most_recent_recipe = all_files[grepl("recipe_ratings", all_files)] %>%
#                 as_tibble() %>%
#                 separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
#                          extra = "merge",
#                          fill = "left") %>%
#                 unite(name, name1:name3) %>%
#                 mutate(date = as.Date(date)) %>%
#                 filter(date == max(date)) %>%
#                 unite(path, name:file) %>%
#                 mutate(path = gsub("_Rds", ".Rds", path)) %>%
#                 pull(path)
#         
#         # use function to get record from bgg
#         suppressMessages({
#                 raw_record = get_game_record(id) %>%
#                         mutate(number_designers = rowSums(across(starts_with("des_"))))
#         })
#         
#         # get training set
#         games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
#         
#         record=   bind_rows(raw_record,
#                             games_datasets$train[0,])
#         
#         rec <- readr::read_rds(here::here("deployment", most_recent_recipe))
#         
#         baked_record = bake(rec, record)
#         
#         req = toJSON(baked_record)
#         
#         #
#         model =  enquo(trained_model)
#         model = rlang::sym(paste(trained_model))
#         
#         # parse example from json
#         parsed_example <- jsonlite::fromJSON(req) %>%
#                 mutate_if(is.integer, as.numeric) %>%
#                 mutate(timestamp = as_datetime(timestamp))
#         
#         most_recent_models = all_files[grepl("trained_models_obj", all_files)] %>%
#                 as_tibble() %>%
#                 separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
#                          extra = "merge",
#                          fill = "left") %>%
#                 unite(name, name1:name3) %>%
#                 mutate(date = as.Date(date)) %>%
#                 filter(date == max(date)) %>%
#                 unite(path, name:file) %>%
#                 mutate(path = gsub("_Rds", ".Rds", path)) %>%
#                 pull(path)
#         
#         # get most recent models
#         models = readr::read_rds(here::here("deployment", most_recent_models))
#         
#         # geek rating
#         model_baverage <- models %>%
#                 filter(outcome_type == 'baverage') %>%
#                 select(!!model) %>%
#                 pull()
#         
#         # get first element
#         model_baverage = model_baverage[[1]]
#         
#         # average rating
#         model_average <- models %>%
#                 filter(outcome_type == 'average') %>%
#                 select(!!model) %>%
#                 pull()
#         
#         # get first element
#         model_average = model_average[[1]]
#         
#         # now predict
#         prediction_baverage <- predict(model_baverage, new_data = parsed_example) %>%
#                 as_tibble() %>%
#                 set_names("baverage")
#         
#         prediction_average <- predict(model_average, new_data = parsed_example) %>%
#                 as_tibble() %>%
#                 set_names("average")
#         
#         # now combine
#         estimate = dplyr::bind_cols(parsed_example %>%
#                                             select(yearpublished, game_id, name),
#                                     prediction_baverage,
#                                     prediction_average) %>%
#                 mutate_if(is.numeric, round, 2) %>%
#                 melt(., id.vars = c("yearpublished", "game_id", "name")) %>%
#                 rename(outcome = variable,
#                        pred = value) %>%
#                 mutate(method = paste(trained_model)) %>%
#                 select(method, everything())
#         
#         out = list("estimate" = estimate,
#                    "record" = req)
#         
#         out
#         
# }
# 
# dump("bake_and_predict_ratings", file="functions/bake_and_predict_ratings.R")

# function for baking and estimating bgg complexity
bake_and_predict_avgweight<- function(id,
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
        
        # get function for game record
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
        # 
        # use function to get record from bgg
        suppressMessages({
                raw_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
        })
        
        # get training set
        # get data used in model development
        games_datasets = readr::read_rds(here::here("active/games_datasets.Rdata"))
        # get models
        models = readr::read_rds(here::here("active/models_complexity.Rds"))
        # get recipe
        rec <- readr::read_rds(here::here("active/recipe_complexity.Rdata"))

        record=   bind_rows(raw_record,
                            games_datasets$train[0,])
        
        baked_record = bake(rec, record)
        
        req = toJSON(baked_record)
        
        #
        model =  enquo(trained_model)
        model = rlang::sym(paste(trained_model))
        
        # parse example from json
        parsed_example <- jsonlite::fromJSON(req) %>%
                mutate_if(is.integer, as.numeric) %>%
                mutate(timestamp = as_datetime(timestamp))
        
        # most_recent_models = all_files[grepl("models_obj_avgweight", all_files)] %>%
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

dump("bake_and_predict_avgweight", file="functions/bake_and_predict_avgweight.R")

# estimate avgweight in order to estimate bgg complexity
# function for predicting and baking avgweight
bake_and_predict_avgweight_and_ratings<- function(id,
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
        games_datasets = readr::read_rds(here::here("active/games_datasets_ratings.Rdata"))
        
        # get models
        models_ratings <- readr::read_rds(here::here("active/models_ratings.Rds"))
        models_complexity <- readr::read_rds(here::here("active/models_complexity.Rds"))
        
        # get recipes
        rec_ratings <- readr::read_rds(here::here("active/recipe_ratings.Rdata"))
        rec_complexity <- readr::read_rds(here::here("active/recipe_complexity.Rdata"))

        # bind
        record=   bind_rows(raw_record,
                            games_datasets$train[0,])

        # bakey
        baked_record = bake(rec_complexity, record)
        
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
        rm(model_avgweight,
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
                            games_datasets$train[0,])
        
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

dump("bake_and_predict_avgweight_and_ratings", file="functions/bake_and_predict_avgweight_and_ratings.R")

# functions for adding color to flextables
# color functions for flextable
# geek rating
baverage_func<- function(x) {
        
        breaks = seq(6, 8.6, 0.1)
        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}
dump("baverage_func", file="functions/baverage_func.R")

# avg rating
average_func<- function(x) {
        
        breaks = seq(6, 9.9, 0.1)
        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}
dump("average_func", file="functions/average_func.R")

# avgweight
avgweight_func<- function(x) {
        
        breaks<-seq(1, 5, 0.1)
        #  breaks = weight_deciles
        colorRamp=colorRampPalette(c("white", "red"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}
dump("avgweight_func", file="functions/avgweight_func.R")

# function for getting game comparables
get_game_comparables = function(id) {
        
        # load function
        source(here::here("functions/get_game_record.R"))
        source(here::here("functions/baverage_func.R"))
        source(here::here("functions/average_func.R"))
        source(here::here("functions/avgweight_func.R"))
        source(here::here("functions/theme_phil.R"))
        
        # load active files
        unsupervised_obj = 
                readr::read_rds(here::here("active", "unsupervised_obj.Rdata"))
        
        unsupervised_neighbors = 
                readr::read_rds(here::here("active", "unsupervised_neighbors.Rdata")) 
        
        recipe_prep = 
                readr::read_rds(here::here("active", "unsupervised_recipe_prep.Rdata"))
        
        games_flattened = 
                readr::read_rds(here::here("active", "games_flattened.Rdata"))  %>%
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
        
        # check to see if id is in the unsupervised object
        check_obs = unsupervised_obj %>%
                filter(dataset == 'fundamentals, mechanics, and categories') %>%
                select(dataset, pca_with_data) %>%
                unnest(c(dataset, pca_with_data)) %>%
                filter(game_id == id) %>%
                nrow()
        
        # get table of games
        active_games = unsupervised_obj %>% 
                filter(dataset == 'fundamentals, mechanics, and categories') %>%
                select(pca_with_data) %>% unnest() %>%
                arrange(desc(baverage)) %>%
                mutate(rank = row_number())
        
        ### if game record is in our previously run analysis, we can just look it up
        
        if(check_obs > 0) {
                
                # get game name
                game = unsupervised_neighbors %>%
                        filter(game_id == id) %>%
                        pull(name) %>%
                        unique()
                
                neighbors_table = unsupervised_neighbors %>%
                        filter(game_id == id) %>%
                        #    filter(dataset == 'fundamentals, mechanics, and categories') %>%
                        select(dataset, game_id, name, neighbor_game_id, neighbor_name, similarity, dist, perc, yearpublished, rank, average, baverage, avgweight) %>%
                        filter(yearpublished < 2022) %>%
                        mutate(game_id = as.character(game_id),
                               neighbor_game_id = as.character(neighbor_game_id),
                               yearpublished = as.character(yearpublished)) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage) %>%
                        group_by(dataset) %>%
                        arrange(dataset, dist) %>%
                        mutate(rank = row_number()) %>%
                        ungroup() %>%
                        filter(rank <=25) %>%
                        rename(Comparing_By = dataset,
                               Similarity = similarity,
                               ID = neighbor_game_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor_name,
                               Rank = rank) %>%
                        select(Comparing_By, Similarity, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2)
                
        } else {
                
                print("game not in existing dataset; pulling game info from BGG and calculating...")
                
                # if the game isn't present, we need to go grab it and then add it to our existing games
                game_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
                
                game = game_record %>%
                        pull(name)
                
                # nest for placemen
                # bak
                baked_game = recipe_prep %>%
                        prep(recipe_prep$template, strings_as_factor = F) %>%
                        bake(new_data = bind_rows(game_record, recipe_prep$template[0,]))
                
                # nest
                nested_game_data<- baked_game %>%
                        mutate(dataset = "fundamentals, mechanics, and categories") %>%
                        nest(-dataset) %>%
                        bind_rows(., baked_game %>%
                                          mutate(dataset = "fundamentals and mechanics") %>%
                                          select(-starts_with("cat_"),
                                                 -number_categories) %>%
                                          nest(-dataset))
                
                ### get comparables for game
                comps_obj = unsupervised_obj %>%
                        select(dataset, pca_trained) %>% # get pca
                        left_join(., nested_game_data,
                                  by = "dataset") %>%
                        mutate(pca_rotation = map2(.x = pca_trained,
                                                   .y = data,
                                                   ~ .x %>% bake(new_data = .y))) %>%
                        mutate(pca_with_data = map2(.x = pca_rotation,
                                                    .y = data,
                                                    ~ .x %>%
                                                            select(game_id, starts_with("PC")) %>%
                                                            left_join(., .y,
                                                                      by = "game_id"))) %>%
                        mutate(type = "game") %>%
                        select(dataset, pca_with_data, type) %>%
                        bind_rows(., unsupervised_obj %>%
                                          select(dataset, pca_with_data) %>%
                                          unnest() %>%
                                          filter(game_id != id) %>%
                                          nest(-dataset, -type) %>%
                                          rename(pca_with_data = data)) %>%
                        mutate(pca_with_data = map(pca_with_data, ~.x %>%
                                                           rename_all(funs(gsub("PC0", "PC", gsub("PC00", "PC", make.names(names(.x)))))))) %>%
                        unnest() %>%
                        nest(-dataset) %>%
                        rename(pca_with_data = data)
                
                # now combine
                unsupervised_obj = comps_obj %>%
                        left_join(., 
                                  unsupervised_obj %>%
                                          select(dataset, kmeans, norm_trained, pca_trained),
                                  by = "dataset") %>%
                        mutate(pca_dist = map(.x = pca_with_data, 
                                              ~ dist(.x %>%
                                                             select(PC1:PC10) %>%
                                                             as.matrix(), 
                                                     method="euclidean") %>%
                                                      as.matrix() %>%
                                                      as.data.frame() %>%
                                                      magrittr::set_rownames(.x$game_id) %>%
                                                      magrittr::set_colnames(.x$game_id))) %>%
                        mutate(obs_dist = map(pca_dist, ~ .x %>%
                                                      rownames_to_column("game_id") %>%
                                                      filter(game_id == id) %>%
                                                      gather('closest','dist',-game_id) %>%
                                                      filter(dist > 0) %>%
                                                      filter(!is.na(dist)) %>% 
                                                      group_by(game_id) %>% 
                                                      arrange(dist) %>% 
                                                      slice_min(dist, n=50, with_ties = T) %>%
                                                      mutate(dist_rank=row_number()))) %>%
                        mutate(neighbors = map(obs_dist,
                                               ~ left_join(.x, game_record %>%
                                                                   mutate(game_id = as.character(game_id)),
                                                           by = c("game_id")) %>%
                                                       select(game_id, name, closest, dist, dist_rank) %>% 
                                                       left_join(., active_games %>%
                                                                         mutate(game_id = as.character(game_id)) %>%
                                                                         rename(neighbor_game_id = game_id,
                                                                                neighbor_name = name),
                                                                 by = c("closest" = "neighbor_game_id")))) %>%
                        mutate(scale_data = map2(.x = norm_trained,
                                                 .y = pca_with_data,
                                                 ~ .x %>% bake(new_data = .y %>%
                                                                       select(-starts_with("PC")) %>%
                                                                       filter(game_id == id)) %>%
                                                         select(-timestamp,
                                                                -game_id,
                                                                -name,
                                                                -average,
                                                                -baverage,
                                                                -usersrated,
                                                                -yearpublished))) %>%
                        mutate(clusters = map2(.x = kmeans,
                                               .y = scale_data,
                                               ~ clue::cl_predict(.x, 
                                                                  newdata = .y)))
                
                # extract neighbors and report
                neighbors_table= unsupervised_obj %>%
                        select(dataset, neighbors) %>% 
                        unnest() %>%
                        rename(neighbor = neighbor_name,
                               neighbor_id = closest) %>%
                        mutate(similarity = 100*1/(1+ sqrt(dist))) %>%
                        select(dataset, game_id, name, neighbor, neighbor_id, similarity, dist, dist_rank) %>%
                        left_join(., active_games %>%
                                          mutate(game_id = as.character(game_id)) %>%
                                          rename(neighbor_id = game_id,
                                                 neighbor = name),
                                  by = c("neighbor_id", "neighbor")) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage)  %>%
                        filter(yearpublished < 2022) %>% # set years allowed for nearest neighbors
                        group_by(dataset) %>%
                        mutate(rank = row_number()) %>%
                        filter(rank <=25) %>%
                        ungroup() %>%
                        mutate_if(is.numeric, round, 2) %>%
                        #  select(-game_id, -similarity, -dist, -dist_rank) %>%
                        mutate(yearpublished = as.character(yearpublished),
                               neighbor_id = as.character(neighbor_id)) %>%
                        rename(Comparing_By = dataset,
                               Similarity = similarity,
                               ID = neighbor_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor,
                               Rank = rank) %>%
                        select(Comparing_By, Similarity, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2)
                
        }
        

        # convert to flextable
        neighbors_table_ft = neighbors_table %>%
                select(Rank, Similarity, Published, ID, Neighbor, BGGRating, GeekRating, Complexity) %>%
                filter(Rank <=25) %>%
             #   select(-Game) %>%
                flextable() %>%
                flextable::autofit() %>%
             #   add_footer(paste("Most similar games to", game, "using complexity, playing time, player count, mechanics, and categories", sep=" ")) %>%
                # bg(., i = ~ Comparing_By =='fundamentals, mechanics, and categories',
                #    bg = 'grey100') %>%
                # bg(., i = ~ Comparing_By == 'fundamentals and mechanics',
                #    bg = 'grey90') %>%
                bg(., j = c("GeekRating"),
                   bg = baverage_func) %>%
                bg(., j = c("BGGRating"),
                   bg = average_func) %>%
                bg(., j = c("Complexity"),
                   bg = avgweight_func)
        
        
        # now make plot
        # make visualization to compare observations on the principal components
        df = unsupervised_obj[1,]$pca_with_data[[1]] %>%
                select(game_id, name, PC1:PC10)
        
        # selected principal components
        profile_df = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T)) %>%
                ungroup() 
        
        # make a plot
        plot_df = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                mutate(variable = case_when(variable == 'PC1' ~ 'PC1_Complexity',
                                            variable == 'PC2' ~ 'PC2_Thematic',
                                            variable == 'PC3' ~ 'PC3_Economy',
                                            variable == 'PC4' ~ 'PC4_Cooperation')) %>%
                filter(!is.na(variable)) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T))
        
        # full
        plot_df2 = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T))
               # do(data.frame(., perc = ecdf(.$value)(.$value)))

        # jitter
        pos <- position_jitter(width = 0.15, seed = 2)
        pos2 <- position_jitter(width = 0.075, seed = 2)
        
        # make background plot
        background = plot_df %>%
                ggplot(., aes(x=variable,
                              y = scale))+
                geom_jitter(alpha=0.1,
                            col = 'grey60',
                            position = pos)+
                theme_phil()+
                geom_hline(yintercept = 0,
                           linetype = 'dashed',
                           alpha = 0.8)+
                theme(legend.position = 'top',
                      legend.title = element_blank())+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())+
                ggtitle(paste("Which games are similar to ", game, "?", sep= ""),
                        subtitle = str_wrap("Placing games on first four principal components of variation: complexity, theme, economy, and coooperation.", 125))
        
        # closest IDs
        compare = c(neighbors_table %>%
                            filter(Rank <=6) %>%
                            pull(ID))
        
        # all neighbor IDs
        compare_full = c(neighbors_table %>%
                            pull(ID))
        # all neighbors
        neighbors = c(game, neighbors_table %>%
                              group_by(Neighbor) %>%
                              mutate(dupe = n_distinct(Neighbor)) %>%
                              mutate(Neighbor = case_when(Neighbor == game ~ paste(Neighbor, Published, sep="_"),
                                                          dupe > 1 ~ paste(Neighbor, Published, sep="_"),
                                                          TRUE ~ Neighbor)) %>%
                              ungroup() %>%
                              pull(Neighbor)) %>%
                rev()
        
        # place on principal components
        compare_plot = background + 
                # geom_jitter(data = plot_df %>%
                #                           filter(game_id %in% id),
                #                   aes(x = variable,
                #                       size = highlight,
                #                       y = scale),
                #             color = "black",
                #             size = 2.5,
                #             position = pos2)+
                geom_jitter(data = plot_df %>%
                                    filter(game_id %in% c(id, compare)),
                            aes(x = variable,
                                color = name,
                                y = scale),
                            size = 2,
                            position = pos2)+
                geom_label_repel(data = plot_df %>%
                                         filter(game_id %in% c(id,compare)),             
                                 aes(x = variable,
                                     color = name,
                                     y=scale,
                                     label = name),
                                 position = pos2,
                                 max.overlaps=15,
                                 show.legend=F,
                                 size = 3)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option="magma",
                                      begin = 0.2,
                                      end = 0.8)
        
        ### remake but with all components
        # make background plot
        background2 = plot_df2 %>%
                ggplot(., aes(x=variable,
                              label = name,
                              y = scale))+
                geom_text(vjust = 0.1,
                          color = "grey60",
                          size =3,
                          position = pos,
                          check_overlap=T)+
                theme_phil()+
                geom_hline(yintercept = 0,
                           linetype = 'dashed',
                           alpha = 0.8)+
                theme(legend.position = 'top',
                      legend.title = element_blank())+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
        
        # full components
        compare_plot2 = background2 + 
                # geom_jitter(data = plot_df %>%
                #                           filter(game_id %in% id),
                #                   aes(x = variable,
                #                       size = highlight,
                #                       y = scale),
                #             color = "black",
                #             size = 2.5,
                #             position = pos2)+
                # geom_jitter(data = plot_df2 %>%
                #                     filter(game_id %in% c(id, compare_full)),
                #             aes(x = variable,
                #                 color = name,
                #                 y = scale),
                #             size = 2,
                #             position = pos2)+
                geom_text(data = plot_df2 %>%
                                         filter(game_id %in% c(id,compare_full)),             
                                 aes(x = variable,
                                     color = name,
                                     y=scale,
                                     label = name),
                                 position = pos,
                                 show.legend=F,
                                 size = 3)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option="magma",
                                      begin = 0.2,
                                      end = 0.8)

        # plot backgrond for pc1 and pc2
        pc_plot_background = df %>%
                ggplot(., aes(x=PC1,
                              label = name,
                              y = PC2))+
                geom_point(alpha = 0.15,
                           col = "grey60")+
                geom_text(check_overlap=T,
                          vjust = 0.5,
                          col = "grey80",
                          size =2)+
                theme_phil()
        
        # pc 1 and 2 plot
        pc_plot = pc_plot_background +
                geom_point(data = df %>%
                                   filter(game_id %in% c(id, compare)),
                           aes(x=PC1,
                               label = name,
                               color = name,
                               y=PC2),
                           size = 3)+
                geom_label_repel(data = df %>%
                                   filter(game_id %in% c(id, compare)),
                           aes(x=PC1,
                               label = name,
                               color = name,
                               y=PC2),
                           size =3,
                           max.overlaps = 15)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option = "magma",
                                      begin = 0.2,
                                      end = 0.8)+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
        
        
        # tile plot
        tile_plot = profile_df %>%
                filter(game_id %in% c(id, compare_full)) %>%
           #     mutate(name = paste(name, game_id, sep="_")) %>%
                mutate(name = factor(abbreviate(name,
                                                minlength=30),
                                     levels = abbreviate(neighbors,
                                                         minlength=30))) %>%
                mutate(scale_round = round(scale, 2)) %>%
                ggplot(., aes(x=variable,
                              y=name,
                              color = scale,
                              fill = scale))+
                geom_tile()+
                theme_phil()+
                theme(legend.title = element_text())+
                scale_color_viridis(option="magma",
                                    limits = c(-5,5),
                                    oob = scales::squish)+
                scale_fill_viridis(option="magma",
                                   limits = c(-5,5),
                                   oob = scales::squish)+
                guides(color = "none",
                       fill = guide_colorbar(barheight=0.5,
                                             barwidth=10,
                                                  title = "Z Score",
                                           #  title = "              Z Score",
                                             title.position = "top"))+
                theme(legend.position = "top")+
                xlab("")+
                ylab("")+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())

        out = list("neighbors_table" = neighbors_table_ft,
                   "neighbors_data" = neighbors_table,
                   "neighbors_plot" = compare_plot,
                   "neighbors_plot_full" = compare_plot2,
                   "tile_plot" = tile_plot,
                   "pc_plot" = pc_plot)
        
        return(out)
        
}

dump("get_game_comparables", file="functions/get_game_comparables.R")

# function for rerunning on errors
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
        
        require(futile.logger)
        require(utils)
        
        attempts = 0
        retval = try(eval(expr))
        while (isError(retval)) {
                attempts = attempts + 1
                if (attempts >= maxErrors) {
                        msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
                        flog.fatal(msg)
                        stop(msg)
                } else {
                        msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                                      capture.output(str(retval)))
                        flog.error(msg)
                        warning(msg)
                }
                if (sleep > 0) Sys.sleep(sleep)
                retval = try(eval(expr))
        }
        return(retval)
}

dump("retry", file = "functions/retry.R")

# function for cleaning up names
rename_func<-function(x) {
        
        x<-gsub("cat_memory", "cat_memory_game", x)
        x<-gsub("cat_spiessecret_agents", "cat_spies_secret_agents", x)
        x<-gsub("cat_deduction", "cat_deduction_game", x)
        x<-gsub("cat_novelbased", "cat_novel_based", x)
        x<-gsub("cat_","", x)
        x<-gsub("mech_","", x)
        x<-gsub("pub_","", x)
        x<-gsub("des_","", x)
        x<-gsub("avgweight", "Average Weight", x)
        x<-gsub("yearpublished", "Year Published", x)
        x<-gsub("minage", "Min Age", x)
        x<-gsub("playingtime", "Playing Time", x)
        x<-gsub("maxplayers", "Max Players", x)
        x<-gsub("minplayers", "Min Players", x)
        x<-gsub("_", " ", x)
        
        x = str_to_title(x)
        x = gsub("World War Ii", "World War II", x)
        x = gsub("Gmt", "GMT", x)
        x = gsub("Cmon", "CMON", x)
        x = gsub("Zman", "ZMan", x)
        x = gsub("Movies Tv", "Movies TV", x)
        x = gsub("Auctionbidding", "Auction Bidding", x)
        x = gsub("Postnapoleonic", "Post Napoleonic", x)
        x
        
}

dump("rename_func", file = "functions/rename_func.R")

