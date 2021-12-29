get_game_record <-
function(insert_id) {
        
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
