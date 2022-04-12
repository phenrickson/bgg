# load standard packages
source(here::here("scripts/load_packages.R"))
source(here::here("functions/tidy_name_func.R"))
source(here::here("functions/get_bgg_data_from_github.R"))

# load games from bgg github
bgg_today = get_bgg_data_from_github(Sys.Date())

# keep only these games
bgg_ids = bgg_today %>% 
        pull(game_id)

# connect to biquery
library(bigrquery)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# authorize
bq_auth(email = "phil.henrickson@aebs.com")

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# query table of game info to most recent load
game_info<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT * FROM bgg.api_game_info
                              where timestamp = (SELECT MAX(timestamp) as most_recent FROM bgg.api_game_info)') %>%
        mutate(numweights = as.numeric(numweights)) %>%
        mutate_at(c("averageweight",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime",
                    "yearpublished"),
                  ~ case_when(. == 0 ~ NA_real_,
                              TRUE ~ .)) %>%
        filter(game_id %in% bgg_ids)
        

# long table with game type variables
game_types= DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.api_game_categories') %>%
        filter(game_id %in% bgg_ids)

# categorical features
categorical_features_selected = readr::read_rds(here::here("/Users/phenrickson/Documents/projects/bgg_reports/",
                                                           "data",
                                                           "categorical_features_selected.Rdata"))

# player counts
games_playercounts= DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.api_game_playercounts') %>%
        filter(!is.na(numplayers)) %>%
        mutate(numberplayers = as.numeric(gsub("\\+", "", numplayers))) %>%
        filter(numvotes > 0) %>%
        mutate(playercount = case_when(numberplayers > 8 ~ "8+",
                                       TRUE ~ as.character(numberplayers))) %>%
        filter(game_id %in% bgg_ids) %>%
        group_by(game_id, playercount) %>% 
        slice_max(order_by = numvotes, n = 1, with_ties=F) %>%
        ungroup()

# descriptions
game_descriptions = DBI::dbGetQuery(bigquerycon, 
                                   'SELECT * FROM bgg.api_game_descriptions') %>%
        filter(game_id %in% bgg_ids)

# most recent complexity adjusted ratings
game_adjustedratings = fread(here::here("/Users/phenrickson/Documents/projects/bgg_reports/adjusted_ratings/",
                                        list.files("/Users/phenrickson/Documents/projects/bgg_reports/adjusted_ratings") %>%
                                                as_tibble() %>%
                                                mutate(date = as.Date(gsub(".csv", "", value))) %>%
                                                filter(date == max(date)) %>%
                                                pull(value)))

# select in full game types set
game_types_selected = game_types %>%
        left_join(., categorical_features_selected %>%
                          select(type, id, value, tidied, selected),
                  by = c("type", "id", "value")) %>%
        filter(selected == 'yes')

# pivot and spread these out
game_types_pivoted =game_types_selected %>%
        select(game_id, type, value) %>%
        mutate(type_abbrev = substr(type, 1, 3)) %>%
        mutate(value = tolower(gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", value))))) %>%
        mutate(type = paste(type, value, sep="_")) %>%
        mutate(has_type = 1) %>%
        select(-value) %>%
        pivot_wider(names_from = c("type"),
                    values_from = c("has_type"),
                    id_cols = c("game_id"),
                    names_sep = "_",
                    values_fn = min,
                    values_fill = 0)

# now join
games_dashboard = game_info %>%
        left_join(.,
                  game_types_pivoted,
                  by = "game_id") %>%
        rename(numowned = owned) %>%
        left_join(.,
                  game_adjustedratings %>%
                          select(game_id,
                                 adj_bayesaverage,
                                 starts_with("votes_")),
                  by = "game_id")

# save these locally for the dashboard
readr::write_rds(games_dashboard,
                 file = here::here("dashboards", "games_dashboard.Rdata"))

# save these locally for the dashboard
readr::write_rds(games_playercounts,
                 file = here::here("dashboards", "games_playercounts.Rdata"))

rm(list=ls())
gc()


