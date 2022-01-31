# source function for reading data 
source(here::here("functions/get_bgg_data_from_github.R"))
source(here::here("scripts/load_packages.R"))

library(httr)
library(xml2)
library(XML)
library(rvest)
library(purrr)

# get todays data
bgg_today<-get_bgg_data_from_github(Sys.Date())

# get ids
bgg_ids = bgg_today %>%
        select(game_id) %>% 
        pull() %>%
        unique()

# # push through API
# samp_id = bgg_ids[1:100]

# create function to pull XML for games, then make tables out of selected output
get_bgg_api_data= function(input_game_id) {
        
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
                "numcomments")
        
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
        
        ## playercounts and poll
        poll_parser = function(var) {
                foreach(i = 1:length(input_game_id), .combine = bind_rows) %do% {
                        
                        poll = getNodeSet(parsed, "//item")[[i]]['poll'][[1]] # getting firste lement from the poll
                        results = getNodeSet(poll, 'results')  %>%
                                map(., xmlToList)
                        
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
                        out = bind_cols(
                                numplayers,
                                votes) %>%
                                mutate(game_id = input_game_id[i]) %>% 
                                select(game_id, numplayers, value, numvotes)
                        
                        out
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
        
# create batches of n size
n = 400
batches = split(bgg_ids, ceiling(seq_along(bgg_ids)/n))

# run through function
batches_returned = foreach(b = 1:length(batches),
                           .errorhandling = 'pass') %do% {
        
        # push batch 
        out = get_bgg_api_data(batches[[b]])
        
        # pause to avoid taxing the API
        Sys.sleep(10)
        
        # print
      #  print(paste("batch", b, "of", length(batches), "complete"))
        cat(paste("batch", b, "of", length(batches), "complete"), sep="\n")
        
        # return
        out
        
                           }

# pulling from API done
print(paste("saving to local"))

# save the raw output if need be
readr::write_rds(batches_returned,
                 file = here::here(paste("raw/batches_returned_", Sys.Date(), ".Rdata", sep="")))

# convert to tabular form
## extract tables
print(paste("creating tables"))

# categories
game_categories = map(batches_returned, "game_categories") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert() %>%
        mutate(id = as.integer(id),
               type = gsub("boardgame", "", type))

# playercounts
game_playercounts = map(batches_returned, "game_playercounts") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert()

# descriptions
game_descriptions = map(batches_returned, "game_description") %>%
        rbindlist(.) %>%
        as_tibble()

# names
game_names = map(batches_returned, "game_names") %>%
        rbindlist(.) %>%
        as_tibble() 

# images
game_images =  map(batches_returned, "game_image") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        left_join(., 
                  map(batches_returned, "game_thumbnails") %>%
                          rbindlist(.) %>%
                          as_tibble(),
                  by = c("game_id"),
                  )

# features
game_features = map(batches_returned, "game_features") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert() %>%
        select(game_id,
               yearpublished,
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
               owned,
               trading,
               wanting,
               wishing
               )

# ranks
game_ranks = map(batches_returned, "game_ranks") %>%
        rbindlist(.) %>%
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

# categories
print(paste("now writing to bigquery"))

## push to bigquery
### push to GCP 
# library bigrquery
library(bigrquery)
library(bigQueryR)
library(DBI)

bq_auth(email = 'phil.henrickson@aebs.com')

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# game categories
dbWriteTable(bigquerycon,
             name = "api_game_categories",
             overwrite = T,
             value = game_categories)

print(paste("game_categories loaded"))

# game names
dbWriteTable(bigquerycon,
             name = "api_game_names",
             overwrite = T,
             value = game_names)

print(paste("game_names loaded"))

# game playercounts
# game names
dbWriteTable(bigquerycon,
             name = "api_game_playercounts",
             overwrite = T,
             value = game_playercounts)

print(paste("game_playercounts loaded"))

# game description
dbWriteTable(bigquerycon,
             name = "api_game_descriptions",
             overwrite = T,
             value = game_descriptions)

print(paste("game_descriptions loaded"))

# game images
dbWriteTable(bigquerycon,
             name = "api_game_images",
             overwrite = T,
             value = game_images)

print(paste("game_images loaded"))

# game info
dbWriteTable(bigquerycon,
             name = "api_game_info",
             overwrite = T,
             value = game_info)

## all done
print(paste("done."))
