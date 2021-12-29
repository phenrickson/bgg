get_collection <-
function(username_string) {
        
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
