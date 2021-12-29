# push ids through bgg api

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(splitstackshape)
library(foreach)

# load bgg analytics
library(bggAnalytics)

# load function for grabbing collections
source("functions/get_collection.R")

# use function for specified users
users<-c("mrbananagrabber",
         "Quinns",
         "TomVasel",
         "rahdo",
         "Gyges")
         

# loop and combine
collections<-foreach(i = 1:length(users), .combine = rbind.data.frame) %do% {
                
        Sys.sleep(5)
        
                get_collection(users[i]) %>%
                as_tibble()
        


}

### push to GCP 
# library bigrquery
library(bigrquery)
library(bigQueryR)
library(DBI)

# authenticate
# bq_auth(path = keyring::key_get(service = "GCP"),
#         use_oob=T)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

## write
dbWriteTable(bigquerycon,
             name = "historical_collections",
             overwrite=T,
             value = collections)
