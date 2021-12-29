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

#
bq_auth(email = "phil.henrickson@aebs.com")

# get game ids from most recent day
# source function for reading data 
source(here::here("functions/get_bgg_data_from_github.R"))

# get todays data from bgg
bgg_today<-get_bgg_data_from_github(Sys.Date())

# get games ids
game_ids<-bgg_today$game_id

# push through API
# takes about 5 min?
games_obj<-bggGames$new(ids = game_ids,
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

### daily pull of games data with timestamp
games_daily<-games_data %>%
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

## games
game_ids<-games_data %>%
        select(game_id,
               name) %>%
        unique() %>%
        arrange(game_id)

## categories
category_ids<-df_list %>% 
        select(game_id, 
               category, 
               category_id) %>%
        unnest(cols = c("category", "category_id")) %>%
        select(category_id, category) %>%
        unique() %>%
        arrange(category_id)

## mechanics
mechanic_ids<-df_list %>% 
        select(game_id, 
               mechanic, 
               mechanic_id) %>%
        unnest(cols = c("mechanic", "mechanic_id")) %>%
        select(mechanic_id, mechanic) %>%
        unique() %>%
        arrange(mechanic_id)


## publishers
publisher_ids<-df_list %>% 
        select(game_id, 
               publisher,
               publisher_id) %>%
        unnest(cols = c("publisher", "publisher_id")) %>%
        select(publisher_id, publisher) %>%
        unique() %>%
        arrange(publisher_id)

## designers
designer_ids<-df_list %>% 
        select(game_id, 
               designer, 
               designer_id) %>%
        unnest(cols = c("designer", "designer_id")) %>%
        select(designer_id, designer) %>%
        unique() %>%
        arrange(designer_id)

## artists
artist_ids<-df_list %>% 
        select(game_id, 
               artist, 
               artist_id) %>%
        unnest(cols = c("artist", "artist_id")) %>%
        select(artist_id, artist) %>%
        unique() %>%
        arrange(artist_id)

## expansions
expansion_ids<-df_list %>% 
        select(game_id, expansion, expansion_id) %>%
        unnest(cols = c("expansion", "expansion_id")) %>%
        select(expansion_id, expansion) %>%
        unique() %>%
        arrange(expansion_id)

### flatten ids for a table that has everything
# # could then craete views off of this to define common tables
# games_flattened<-df_list %>%
#         select(game_id, 
#                category_id,
#                publisher_id,
#                designer_id,
#                artist_id,
#                expansion_id
#                ) %>%
#         unnest(cols = c("category_id")) %>%
#         unnest(cols = c("publisher_id")) %>% 
#         unnest(cols = c("designer_id")) %>% 
#         unnest(cols = c("artist_id")) %>%
#         unnest(cols = c("expansion_id"))

## or, flatten specific tables
# game and yearpublished
game_yearpublished<- games_daily %>%
        select(game_id, yearpublished) %>%
        arrange(game_id, yearpublished)

# game and artists
game_artists <- df_list %>%
        select(game_id, artist_id) %>%
        unnest(cols = c("game_id", "artist_id")) %>%
        arrange(game_id, artist_id)

# game and categories
game_categories <- df_list %>%
        select(game_id, category_id) %>%
        unnest(cols = c("game_id", "category_id")) %>%
        arrange(game_id, category_id)

# game and mechanics
game_mechanics <- df_list %>%
        select(game_id, mechanic_id) %>%
        unnest(cols = c("game_id", "mechanic_id")) %>%
        arrange(game_id, mechanic_id)

# game and designers
game_designers <- df_list %>%
        select(game_id, designer_id) %>%
        unnest(cols = c("game_id", "designer_id")) %>%
        arrange(game_id, designer_id)

# game and publishers
game_publishers <- df_list %>%
        select(game_id, publisher_id) %>%
        unnest(cols = c("game_id", "publisher_id")) %>%
        arrange(game_id, publisher_id)

# games and expansions
game_expansions <- df_list %>%
        select(game_id, expansion_id) %>%
        unnest(cols = c("game_id", "expansion_id")) %>%
        arrange(game_id, expansion_id)

# recommended players
game_recplayers<-df_list %>% 
        select(game_id, 
               recplayers) %>%
        unnest(cols = c("game_id", "recplayers")) %>%
        arrange(game_id)

# best players
game_bestplayers<-df_list %>% 
        select(game_id, 
               bestplayers) %>%
        unnest(cols = c("game_id", "bestplayers")) %>%
        arrange(game_id)

# beep to let me know this first part is finished
beepr::beep(3)
              
### push to GCP 
# library bigrquery
library(bigrquery)
library(bigQueryR)
library(DBI)

# authenticate
# bq_auth(path = keyring::key_get(service = "GCP"),
#         email = 'phil.henrickson@aebs.com',
#          use_oob=T)

bq_auth(email = key_get(service='ae'))

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

#### Write to GCP
# append
dbWriteTable(bigquerycon,
             name = "games_daily",
             append = T,
             value = games_daily)

## overwrite
## id tables

# game ids
dbWriteTable(bigquerycon,
             name = "game_ids",
             overwrite = T,
             value = game_ids)

# artist ids
dbWriteTable(bigquerycon,
             name = "artist_ids",
             overwrite = T,
             value = artist_ids)

# category ids
dbWriteTable(bigquerycon,
             name = "category_ids",
             overwrite = T,
             value = category_ids)

# designer ids
dbWriteTable(bigquerycon,
             name = "designer_ids",
             overwrite = T,
             value = designer_ids)

# publisher ids
dbWriteTable(bigquerycon,
             name = "publisher_ids",
             overwrite = T,
             value = publisher_ids)

# expansion ids
dbWriteTable(bigquerycon,
             name = "expansion_ids",
             overwrite = T,
             value = expansion_ids)

# mechanic ids
dbWriteTable(bigquerycon,
             name = "mechanic_ids",
             overwrite = T,
             value = mechanic_ids)

## linking tables
## overwrite
dbWriteTable(bigquerycon,
             name = "game_yearpublished",
             overwrite = T,
             value = game_yearpublished)

# game artists
dbWriteTable(bigquerycon,
             name = "game_artists",
             overwrite = T,
             value = game_artists)

# game_bestplayers
dbWriteTable(bigquerycon,
             name = "game_bestplayers",
             overwrite = T,
             value = game_bestplayers)

# game_categories
dbWriteTable(bigquerycon,
             name = "game_categories",
             overwrite = T,
             value = game_categories)

# game_designers
dbWriteTable(bigquerycon,
             name = "game_designers",
             overwrite = T,
             value = game_designers)

# game_expansions
dbWriteTable(bigquerycon,
             name = "game_expansions",
             overwrite = T,
             value = game_expansions)

# game_mechanics
dbWriteTable(bigquerycon,
             name = "game_mechanics",
            overwrite = T,
             value = game_mechanics)

# game_publishers
dbWriteTable(bigquerycon,
             name = "game_publishers",
             overwrite=T,
             value = game_publishers)

# game_recplayers
dbWriteTable(bigquerycon,
             name = "game_recplayers",
             overwrite = T,
             value = game_recplayers)

rm(list=ls())

# ## id tables
# bq_game_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "game_ids"))
# 
# bq_category_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                 dataset_id = "bgg",
#                                 table_id = "category_ids"))
# 
# bq_mechanic_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "mechanic_ids"))
# 
# bq_artist_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "artist_ids"))
# 
# bq_designer_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "designer_ids"))
# 
# bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "publisher_ids"))
# 
# bq_expansion_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "expansion_ids"))
# 
# ## linking tables
# bq_game_bestplayers<-as_bq_table(list(project_id = PROJECT_ID,
#                                       dataset_id = "bgg",
#                                       table_id = "game_bestplayers"))
# 
# bq_game_categories<-as_bq_table(list(project_id = PROJECT_ID,
#                                    dataset_id = "bgg",
#                                    table_id = "game_categories"))
# 
# bq_game_mechanics<-as_bq_table(list(project_id = PROJECT_ID,
#                                      dataset_id = "bgg",
#                                      table_id = "game_mechanics"))
# 
# bq_game_artists<-as_bq_table(list(project_id = PROJECT_ID,
#                                       dataset_id = "bgg",
#                                       table_id = "game_artists"))
# 
# bq_game_designers<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "game_designers"))
# # 
# bq_game_publishers<-as_bq_table(list(project_id = PROJECT_ID,
#                                     dataset_id = "bgg",
#                                     table_id = "game_publishers"))
# # 
# bq_game_expansions<-as_bq_table(list(project_id = PROJECT_ID,
#                                      dataset_id = "bgg",
#                                      table_id = "game_expansions"))
# 
# bq_game_recplayers<-as_bq_table(list(project_id = PROJECT_ID,
#                                      dataset_id = "bgg",
#                                      table_id = "game_recplayers"))

# ### Upload tables
# bq_table_upload(bq_games_data, 
#                 values = games_daily,
#                 quiet=F)

# ## id tables
# bq_table_upload(bq_game_ids, 
#                 game_ids,
#                 write_disposition = "WRITE_OVERWRITE",
#                 quiet=F)
# 
# bq_table_upload(bq_publisher_ids, 
#                 publisher_ids,
#                 quiet=F)
# 
# bq_table_upload(bq_designer_ids, 
#                 designer_ids,
#                 quiet=F)
# 
# bq_table_upload(bq_artist_ids, 
#                 artist_ids,
#                 quiet=F)
# 
# bq_table_upload(bq_category_ids, 
#                 category_ids,
#                 quiet=F)
# 
# bq_table_upload(bq_mechanic_ids, 
#                 mechanic_ids,
#                 quiet=F)
# 
# ## linking tables
# ## id tables
# bq_table_upload(bq_game_bestplayers, 
#                 game_bestplayers,
#                 quiet=F)
# 
# bq_table_upload(bq_game_categories, 
#                 game_categories,
#                 quiet=F)
# 
# bq_table_upload(bq_game_artists, 
#                 game_artists,
#                 quiet=F)
# 
# bq_table_upload(bq_game_expansions, 
#                 game_expansions,
#                 quiet=F)
# 
# bq_table_upload(bq_game_designers, 
#                 categor
#                 quiet=F)
# 
# bq_table_upload(bq_mechanic_ids, 
#                 mechanic_ids,
#                 quiet=F)
# 
# # game ids
# bq_game_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                 dataset_id = "bgg",
#                                 table_id = "game_ids"))
# 
# # category ids
# bq_category_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                               dataset_id = "bgg",
#                               table_id = "category_ids"))
# 
# # mechanic
# bq_mechanic_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "mechanic_ids"))
# 
# # publisher
# bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                   dataset_id = "bgg",
#                                   table_id = "publisher_ids"))
# 
# # publisher
# bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                 dataset_id = "bgg",
#                                 table_id = "publisher_ids"))
# 
# # artist
# bq_artist_ids<-as_bq_table(list(project_id = PROJECT_ID,
#                                    dataset_id = "bgg",
#                                    table_id = "artist_ids"))

## linking tables



