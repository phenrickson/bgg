# convert to tabular form
convert_bgg_api_data_to_tables = 
        function(input_api_returned) {
                
                # get function
                source(here::here("functions/pivot_and_dummy_types.R"))
        
        # extract tables
       # print(paste("creating tables"))
        
        # types
        game_types= input_api_returned$game_categories %>%
                as_tibble() %>%
                type_convert() %>%
                mutate(id = as.integer(id),
                       type = gsub("boardgame", "", type))
        
        # playercounts
        game_playercounts = input_api_returned$game_playercounts %>%
                as_tibble() %>%
                type_convert()
        
        # descriptions
        game_descriptions = input_api_returned$game_descriptions %>%
                as_tibble()
        
        # names
        game_names = input_api_returned$game_names %>%
                as_tibble() 
        
        # images
        game_images = input_api_returned$game_image %>%
                as_tibble() %>%
                left_join(., 
                          input_api_returned$game_thumbnails %>%
                                  as_tibble(),
                          by = c("game_id"),
                )
        
        # features
        game_features = input_api_returned$game_features %>%
                as_tibble() %>%
                type_convert() %>%
                select(game_id,
                       yearpublished,
                       averageweight,
                       average,
                       bayesaverage,
                       usersrated,
                       stddev,
                       minage,
                       minplayers,
                       maxplayers,
                       playingtime,
                       minplaytime,
                       maxplaytime,
                       numcomments,
                       numweights,
                       owned,
                       trading,
                       wanting,
                       wishing
                )
        
        # ranks
        game_ranks = input_api_returned$game_ranks %>%
                as_tibble()  %>%
                filter(name %in% c("boardgame", 
                                   "childresngames",
                                   "cgs",
                                   "familygames",
                                   "partygames",
                                   "strategygames",
                                   "thematic",
                                   "wargames")) %>%
                mutate(bayesaverage = case_when(bayesaverage == 'Not Ranked' ~ NA_character_,
                                                TRUE ~ bayesaverage)) %>%
                select(game_id, name, value, bayesaverage) %>%
                type_convert() %>%
                rename(rank = value) %>%
                select(-bayesaverage) %>%
                pivot_wider(id_cols = c("game_id"),
                            names_from = c("name"),
                            names_prefix = c("rank_"),
                            values_from = c("rank"))
        
        ## Combine           
        # combine features and ranks
        game_info = game_features %>%
                # left_join(., game_ranks,
                #           by = c("game_id")) %>%
                left_join(., game_names %>%
                                  filter(type == 'primary') %>%
                                  select(game_id, value) %>%
                                  rename(name = value),
                          by = c("game_id")) %>%
                select(game_id,
                       name, 
                       everything()) %>%
                mutate(timestamp = Sys.time())
        
        ## pivot mechanics, familiess, etc, to tabular form
        
        # categories
        game_categories = pivot_and_dummy_types(game_types,
                                                "category")
        
        # family
        game_families= pivot_and_dummy_types(game_types,
                                             "family") 
        # mechanics
        game_mechanics = pivot_and_dummy_types(game_types,
                                               "mechanic")
        
        # designers
        game_designers = pivot_and_dummy_types(game_types,
                                               "designer") 
        
        # publishers
        game_publishers = pivot_and_dummy_types(game_types,
                                                "publisher") 
        
        # artists
        game_artists = pivot_and_dummy_types(game_types,
                                             "artist") 
        
        # return
        games_out = game_info %>%
                left_join(., game_mechanics,
                          by = c("game_id")) %>%
                left_join(., game_families,
                          by = c("game_id")) %>%
                left_join(., game_categories,
                          by = c("game_id")) %>%
                left_join(., game_designers,
                          by = c("game_id")) %>%
                left_join(., game_publishers,
                          by = c("game_id")) %>%
                left_join(., game_artists,
                          by = c("game_id"))

        out = list("games_types" = game_types,
                   "games_info" = games_out,
                   "game_playercounts" = game_playercounts,
                   "game_descriptions" = game_descriptions,
                   "game_names" = game_names,
                   "game_images" = game_images,
                   "game_ranks" = game_ranks)
        
        return(out)
        
        }
