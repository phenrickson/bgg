# load data from github to local server

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(bigrquery)
library(bigQueryR)
library(DBI)
library(keyring)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

bq_auth(email = key_get(service='ae'))

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# source function for reading data 
source(here::here("functions/get_bgg_data_from_github.R"))

## retrieve most recent
active_games_dates<-DBI::dbGetQuery(bigquerycon,
                          'SELECT DISTINCT date
                           FROM bgg.active_bgg_rankings_view')

# date grid
date_grid = seq(max(active_games_dates$date),
                as.Date(Sys.Date()),
                by = 1)

# get all from github
library(foreach)
github_files<-foreach(i = 1:length(date_grid),
                      .combine = rbind.data.frame,
                      .errorhandling = 'remove') %do% {
                              
                              bgg_day<-get_bgg_data_from_github(date_grid[i])
                              
                              print(date_grid[i])
                              
                              bgg_day
                      }

# trim down
bgg_load<- github_files %>%
        select(date,
               game_id,
               #  game_name,
               game_release_year,
               bgg_rank,
               bgg_average,
               bayes_average,
               users_rated)

# bq object
bq_bgg_load<-as_bq_table(list(project_id = PROJECT_ID,
                              dataset_id = "bgg",
                              table_id = "historical_game_rankings"))

bq_table_exists(bq_bgg_load)

# push
dbWriteTable(bigquerycon,
             name = "historical_game_rankings",
             append = T,
             value = bgg_load)

# # # append to sql table
# # bq_table_upload(bq_bgg_today,
# #                 values = bgg_load,
#                 write_disposition = "WRITE_APPEND",
#                 quiet=F)
#

# # pause
# Sys.sleep(1)

# rm
#rm(bgg_day, bgg_load_day)

print(paste(date_grid[i], "loaded"))

rm(list=ls())
