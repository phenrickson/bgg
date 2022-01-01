# run user notebook
library(tidyverse)
library(foreach)
# 
# # get user list
# users = list.files(here::here("predict_user_collections/user_reports")) %>%
#         as_tibble() %>%
#         mutate(value = gsub("Watch_It_Played", "Watch%20It%20Played", value)) %>%
#         mutate(value = gsub(".html", "", value)) %>%
#         mutate(value = gsub("_copy", "", value)) %>%
#         mutate(value = gsub("_2016", "", value)) %>%
#         mutate(value = gsub("_2017", "", value)) %>%
#         mutate(value = gsub("_2018", "", value)) %>%
#         mutate(value = gsub("_2019", "", value)) %>%
#         mutate(value = gsub("_2020", "", value)) %>%
#         mutate(value = gsub("_2", "", value)) %>%
#         rename(username = value) %>%
#         filter(username != 'Donkler') %>%
#         unique()

# # get list
# user_list = users$username
#year_end = 2019

#user_list = "ZeeGarcia"
#user_list = 'Watch%20It%20Played'
#user_list = "DTlibrary"
#user_list = 'monstronaut'
user_list = 'philfromqueens'

# set year end
year_end = 2019

# run
foreach(i=1:length(user_list)) %do% {
        rmarkdown::render(here::here("predict_user_collections/notebook_for_modeling_individuals.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = year_end),
                          output_file =  paste(
                                  gsub("%20", 
                                       "_",
                                       user_list[i]),
                                               year_end,
                                               sep = "_"),
                          output_dir = here::here("predict_user_collections/user_reports"))
}

rm(list=ls())
gc()
