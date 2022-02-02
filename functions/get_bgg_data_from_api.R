get_bgg_api_data <-
function(input_game_id) {
        
        require(httr)
        require(xml2)
        require(XML)
        require(rvest)
        require(purrr)
        
        # push through api
        url = GET(paste('https://www.boardgamegeek.com/xmlapi2/thing?id=', paste(input_game_id, collapse=","), '&stats=1', sep=""))
        
        # get url
        doc = xml2::read_xml(url)
        
        # parse
        parsed = xmlInternalTreeParse(doc, useInternalNodes = T)
        
        # get thumbnails
        info_parser = function(var) {
                
                foreach(i = 1:length(input_game_id), .combine = bind_rows) %do% {
                        getNodeSet(parsed, "//item")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>% 
                                as_tibble() %>%
                                mutate(game_id = input_game_id[i]) %>% 
                                select(game_id, everything())
                }
                
        }
        
        ### get specific output
        game_names = info_parser(var = 'name')
        
        # thumbnails
        game_thumbnails = info_parser(var = 'thumbnail') %>%
                set_names(., c("game_id", "thumbnail"))
        
        # description
        game_description = info_parser(var = 'description') %>%
                set_names(., c("game_id", "description"))
        
        # image
        game_image = info_parser(var = 'image') %>%
                set_names(., c("game_id", "image"))
        
        # categories, mechanics, etc
        game_categories= suppressWarnings(
                info_parser(var = 'link') %>%
                        select(game_id, type, id, value)
        )
        
        ## summary info
        # summary of game
        summary_parser = function(var) {
                foreach(i = 1:length(input_game_id), .combine = bind_rows) %do% {
                        getNodeSet(parsed, "//item")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>% 
                                as_tibble() %>%
                                mutate(game_id = input_game_id[i]) %>% 
                                mutate(type = paste(var)) %>%
                                select(game_id, everything()) %>%
                                select(game_id, type, value)
                }
                
        }
        
        # selected summary info
        summary = c("yearpublished",
                    "minage",
                    "minplayers",
                    "maxplayers",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime")
        
        # get game summary
        game_summary = foreach(h = 1:length(summary),
                               .combine = bind_rows) %do% {
                                       summary_parser(var = summary[h])
                               }
        
        # statistics
        stats = c("usersrated",
                  "average",
                  "bayesaverage",
                  "stddev",
                  "owned",
                  "trading",
                  "wanting",
                  "wishing",
                  "numcomments",
                  "numweights",
                  "averageweight")
        
        # function
        stats_parser = function(var) {
                foreach(i = 1:length(input_game_id), .combine = bind_rows) %do% {
                        
                        getNodeSet(parsed, "//ratings")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>%
                                as_tibble() %>%
                                mutate(game_id = input_game_id[i]) %>%
                                mutate(type = paste(var)) %>%
                                select(game_id, everything()) %>%
                                select(game_id, type, value)
                }
        }
        
        # get stats
        game_stats = foreach(h=1:length(stats), .combine = bind_rows) %do% {
                stats_parser(var = stats[h])
        }
        
        # get ranks
        # function
        ranks_parser = function(var) {
                foreach(i = 1:length(input_game_id), .combine = bind_rows) %do% {
                        
                        getNodeSet(parsed, "//ranks")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>%
                                as_tibble() %>%
                                mutate(game_id = input_game_id[i]) %>%
                                mutate(type = paste(var)) %>%
                                select(game_id, everything())
                }
        }
        
        # get ranks
        game_ranks = ranks_parser('rank')
        
        poll_parser = function(input_game_id) {
                foreach(i = 1:length(input_game_id), 
                        .combine = bind_rows, 
                        .errorhandling = 'pass') %do% {
                                
                                poll = getNodeSet(parsed, "//item")[[i]]['poll'][[1]] # getting first element from the poll
                                results = getNodeSet(poll, 'results')  %>%
                                        map(., xmlToList)
                                
                                check = length(unlist(results))
                                
                                if (check > 3) {
                                        # player counts with votes
                                        numplayers = results %>% 
                                                map(., ".attrs") %>% 
                                                do.call(rbind, .) %>% 
                                                as_tibble() %>% 
                                                pull() %>%
                                                rep(each = 3) %>%
                                                as_tibble() %>%
                                                rename(numplayers = value)
                                        
                                        # the votes
                                        votes = results %>% map(., as.data.frame) %>% 
                                                map(., t) %>%
                                                do.call(rbind, .) %>% 
                                                as_tibble() %>%
                                                filter(value %in% c("Best", "Recommended", "Not Recommended"))
                                        
                                        # combine and out
                                        return = bind_cols(
                                                numplayers,
                                                votes) %>%
                                                mutate(game_id = input_game_id[i]) %>% 
                                                select(game_id, numplayers, value, numvotes) 
                                }
                                
                                else {
                                        return = tibble(game_id = input_game_id[i]) 
                                }
                                
                                return
                                
                        }
                
        }
        
        # votes for each games
        game_playercounts = poll_parser(input_game_id)
        
        
        ## pivot some of this for output
        game_features = game_summary %>%
                spread(type, value) %>%
                left_join(., 
                          game_stats %>%
                                  spread(type, value),
                          by = c("game_id"))
        
        # combine output
        output = list("timestamp" = Sys.time(),
                      "game_description" = game_description,
                      "game_names" = game_names,
                      "game_thumbnails" = game_thumbnails,
                      "game_image" = game_image,
                      "game_features" = game_features,
                      "game_playercounts" = game_playercounts,
                      "game_categories" = game_categories,
                      "game_ranks" = game_ranks)
        
        return(output)
        
}

# # save function
# dump("get_bgg_api_data",
#      file = here::here("functions/get_bgg_data_from_api.R"))
