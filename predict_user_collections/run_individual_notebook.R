# run user notebook
library(tidyverse)
library(foreach)

source(here::here("functions/get_collection.R"))
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
#user_list = 'philfromqueens'

# user_list = c("Herakleitos",
#               "Veritaas",
#               "Agent Chi",

# user_list = c("EuroCultAV",
#               "Gutterspud")

# user_list = c("Spire_Rubica",
#               "Tigerguitarist",
#               "wizkid27",
#               "Gameboss",
#               "Aural_turpitude")
# 
# # next batch
# user_list = c("jm82")
#user_list = c("wizkid27")

# # next batch
# user_list = c("Gameboss",
#               "internetfloozy",
#               "bsnyder788",
#               "enrikezido",
#               "St Vincent",
#               "Allgood322", 
#               "Agent Chi",
#               "DarrenJ23",
#               "omeletterice",
#               "yayitsk")

# # next batch
# user_list = c("achuds",
#               "RushNYC",
#               "BeeDub1515",
#               "fionajackilarious",
#               "fishingking",
#               "chewbacca390",
#               "Kameronsnewphone",
#               "Guilding_Light")

#              "GertrudeBell",
# Jimmygabioud"
#"Bhouse33",
# "Volnay",
#    "Eric",

# need to circle back to these guys
user_list = c("fishingking",
              "revengeanceful",
              "achuds")
year_list = 2018

# next batch
user_list = c("Houdinimaster11",
              "butilheiro",
              "TerminalVentures",
              "Sobeknofret",
              "Vernisious",
              "Cpf86",
              "Leadera",
              "Omnivoid07",
              "Secular_priest",
              "Tytusmk",
              "QuarkBart",
              "Jarl Gilles"
              )
year_end = 2019

# amend
user_list = gsub(" ", "%20", user_list)

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
