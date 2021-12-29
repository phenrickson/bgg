# push ids through bgg api

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(splitstackshape)
library(keyring)

# load bgg analytics
library(bggAnalytics)

# load big query
library(bigrquery)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# get token
bq_auth(email = 'phil.henrickson@aebs.com')

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# bgg today
source("functions/get_bgg_data_from_github.R")
source("functions/get_game_record.R")
bgg_day<-get_bgg_data_from_github(Sys.Date())

# # push ids through a flattening function
# # takes about.... 40 minutes to get every game?
# games_flattened = bgg_

# get datasets
# query table
active_games<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT * FROM bgg.active_games_daily')

# general game info
games_info<-DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.active_games_info')

# game categories
game_categories<-DBI::dbGetQuery(bigquerycon, 
                                 'SELECT 
                              a.game_id,
                              b.category_id,
                              b.category
                              FROM bgg.game_categories a
                               LEFT JOIN bgg.category_ids b 
                               ON a.category_id = b.category_id')

# game mechanics
game_mechanics<-DBI::dbGetQuery(bigquerycon, 
                                'SELECT 
                              a.game_id,
                              b.mechanic_id,
                              b.mechanic
                              FROM bgg.game_mechanics a
                               LEFT JOIN bgg.mechanic_ids b 
                               ON a.mechanic_id = b.mechanic_id')

# game publishers
game_publishers<-DBI::dbGetQuery(bigquerycon, 
                                 'SELECT 
                              a.game_id,
                              b.publisher_id,
                              b.publisher
                              FROM bgg.game_publishers a
                               LEFT JOIN bgg.publisher_ids b 
                               ON a.publisher_id = b.publisher_id')

# game designers
game_designers<-DBI::dbGetQuery(bigquerycon, 
                                'SELECT 
                              a.game_id,
                              b.designer_id,
                              b.designer
                              FROM bgg.game_designers a
                               LEFT JOIN bgg.designer_ids b 
                               ON a.designer_id = b.designer_id')

# game artists
game_artists<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT 
                              a.game_id,
                              b.artist_id,
                              b.artist
                              FROM bgg.game_artists a
                               LEFT JOIN bgg.artist_ids b 
                               ON a.artist_id = b.artist_id')

# game recplayers
game_recplayers<- DBI::dbGetQuery(bigquerycon,
                                  'SELECT * FROM bgg.game_recplayers')

# pivot
recplayers = game_recplayers %>%
        mutate(recommended = 1) %>%
        mutate(recplayers = as.numeric(gsub("\\+", "", recplayers))) %>%
        mutate(recplayers = case_when(recplayers > 8 ~ "8_or_more",
                                      TRUE ~ as.character(recplayers))) %>%
        mutate(recplayers = paste("recplayers", recplayers, sep="_")) %>%
        pivot_wider(names_from = c("recplayers"),
                    values_from = c("recommended"),
                    id_cols = c("game_id"),
                    names_sep = "_",
                    values_fn = min,
                    values_fill = 0) %>%
        select(game_id,
               recplayers_1,
               recplayers_2,
               recplayers_3,
               recplayers_4,
               recplayers_5,
               recplayers_6,
               recplayers_7,
               recplayers_8,
               recplayers_8_or_more)

# game bestplayers
game_bestplayers<- DBI::dbGetQuery(bigquerycon,
                                  'SELECT * FROM bgg.game_bestplayers')

# pivot
bestplayers = game_bestplayers %>%
        mutate(recommended = 1) %>%
        mutate(bestplayers = as.numeric(gsub("\\+", "", bestplayers))) %>%
        mutate(bestplayers = case_when(bestplayers > 8 ~ "8_or_more",
                                      TRUE ~ as.character(bestplayers))) %>%
        mutate(bestplayers = paste("bestplayers", bestplayers, sep="_")) %>%
        pivot_wider(names_from = c("bestplayers"),
                    values_from = c("recommended"),
                    id_cols = c("game_id"),
                    names_sep = "_",
                    values_fn = min,
                    values_fill = 0) %>%
        select(game_id,
               bestplayers_1,
               bestplayers_2,
               bestplayers_3,
               bestplayers_4,
               bestplayers_5,
               bestplayers_6,
               bestplayers_7,
               bestplayers_8,
               bestplayers_8_or_more)

# player counts
playercounts = game_recplayers %>%
        mutate(type = "recommended") %>%
        rename(playercount = recplayers) %>%
        bind_rows(., game_bestplayers %>%
                          mutate(type = "best") %>%
                          rename(playercount = bestplayers)) %>%
        select(game_id, type, playercount) %>%
        mutate(playercount = as.numeric(gsub("\\+", "", playercount))) %>%
        filter(playercount > 0) %>%
        mutate(playercount = case_when(playercount > 8 ~ "8_or_more",
                                       TRUE ~ as.character(playercount))) %>%
        arrange(game_id, playercount)

# set parameters for setting up training set
min_ratings = 200
split_year = 2019

# data set for inspection
data_inspection = active_games %>% 
        filter(usersrated > min_ratings) %>%
        filter(!is.na(name)) %>%
        filter(!is.na(yearpublished)) %>%
        filter(yearpublished <= split_year) %>%
        select(game_id, average, baverage) %>%
        melt(., id.vars = c("game_id")) %>%
        rename(outcome_type = variable,
               outcome = value) %>%
        left_join(., active_games %>%
                          select(-average, -baverage),
                  by = c("game_id"))

### Designer Features
min_games = 10

# rank designers with min games
top_designers = data_inspection %>%
        left_join(., game_designers,
                  by = "game_id") %>%
        select(timestamp, game_id, name, designer_id, designer, everything()) %>%
        filter(!is.na(designer)) %>%
        #filter(designer_id %in% top_designers$designer_id) %>%
        group_by(designer_id, designer, outcome_type) %>%
        summarize(median_rating = median(outcome),
                  n_games = n_distinct(game_id),
                  .groups = 'drop') %>%
        group_by(outcome_type) %>%
        filter(n_games > min_games | designer == 'Vital Lacerda') %>%
        arrange(desc(median_rating)) %>%
        mutate(rank = row_number())

### Publisher Features
publisher_list = c(51,
                   102,
                   196,
                   396,
                   1027,
                   21847,
                   10,
                   1001,
                   512,
                   4,
                   140,
                   157,
                   34,
                   28,
                   10001,
                   39,
                   37,
                   20,
                   3,
                   538,
                   52,
                   8923,
                   17,
                   5,
                   3320,
                   597,
               #     5400, matagot, dropping for now due to leakage issues with publishing in france
                   26,
                   47,
                   11652,
                   19,
                   13,
                   12024,
                   10754,
                   21608,
                   108,
                   221,
                   171,
                   93,
                   25842,
                   140,
                   28072)

### Artist Features
min_games = 10

# rank artists with min games
top_artists = data_inspection %>%
        left_join(., game_artists,
                  by = "game_id") %>%
        select(timestamp, game_id, name, artist_id, artist, everything()) %>%
        filter(!is.na(artist)) %>%
        #filter(artist_id %in% top_artists$artist_id) %>%
        group_by(artist_id, artist, outcome_type) %>%
        summarize(median_rating = median(outcome),
                  n_games = n_distinct(game_id),
                  .groups = 'drop') %>%
        group_by(outcome_type) %>%
        filter(n_games > min_games) %>%
        arrange(desc(median_rating)) %>%
        mutate(rank = row_number())

# load active games as flattened
games_flattened = active_games
readr::write_rds(games_flattened, file = paste("local/games_flattened_", Sys.Date(), ".Rdata", sep=""))

### Create Datasets

# function for creating training and test sets
source("functions/combine_and_split_bgg_datasets.R")

### run through function
games_datasets= combine_and_split_bgg_datasets(datasets_list = list("active_games" = active_games,
                                                                    "game_categories" = game_categories,
                                                                    "game_designers" = game_designers,
                                                                    "game_mechanics" = game_mechanics,
                                                                    "game_publishers" = game_publishers,
                                                                    "game_artists" = game_artists),
                                               min_users = min_ratings,
                                               year_split = split_year, # doesn't really matter for this particular exercise
                                               publisher_list = publisher_list,
                                               top_designers = top_designers,
                                               top_artists = top_artists)

# output
readr::write_rds(games_datasets, file = paste("local/games_datasets_", Sys.Date(), ".Rdata", sep=""))
readr::write_rds(publisher_list, file = "local/publisher_list.Rdata")
readr::write_rds(top_designers, file = "local/top_designers.Rdata")
readr::write_rds(top_artists, file = "local/top_artists.Rdata")
readr::write_rds(playercounts, file = "local/playercounts.Rdata")

rm(list=ls())



