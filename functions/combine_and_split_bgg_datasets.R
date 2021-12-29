combine_and_split_bgg_datasets <-
function(datasets_list,
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
