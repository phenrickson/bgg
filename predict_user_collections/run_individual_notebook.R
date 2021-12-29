# run user notebook
library(tidyverse)
library(foreach)

# get user list
users = list.files(here::here("predict_user_collections/individual_reports")) %>%
        as_tibble() %>%
        mutate(value = gsub(".html", "", value)) %>%
        mutate(value = gsub("_copy", "", value)) %>%
        mutate(value = gsub("_2016", "", value)) %>%
        mutate(value = gsub("_2017", "", value)) %>%
        mutate(value = gsub("_2018", "", value)) %>%
        mutate(value = gsub("_2019", "", value)) %>%
        mutate(value = gsub("_2020", "", value)) %>%
        mutate(value = gsub("_2", "", value)) %>%
        rename(username = value) %>%
        unique()

# get list
user_list = users$username
year_end = 2019

user_list = "mrbananagrabber"

# run
foreach(i=1:length(user_list)) %do% {
        rmarkdown::render(here::here("predict_user_collections/notebook_for_modeling_individuals.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = year_end),
                          output_file =  paste(user_list[i],
                                               year_end,
                                               sep = "_"),
                          output_dir = here::here("predict_user_collections/individual_reports"))
}

# create function for looping over specified user list and creating reports
# function
# run_user_analysis = function(user_list,
#                              year_end) {
#         
#         foreach(i=1:length(user_list)) %do% {
#                 rmarkdown::render(here::here("predict_user_collections/notebook_for_modeling_individuals.Rmd"), 
#                                   params = list(username = user_list[i],
#                                                 end_training_year = year_end),
#                                   output_file =  paste(user_list[i],
#                                                        year_end,
#                                                        sep = "_"),
#                                   output_dir = here::here("predict_user_collections/individual_reports"))
#         }
#         
# }

# # run
# run_user_analysis(user_list =users$username[16],
#                   year = 2019)


# user_list = c("legendarydromedary",
#               "lmageezy",
#               "C3Gaming",
#               "innerkudzu",
#               "Karmatic",
#               "camerinjohnston")
#user_list = c("jyothi")
# user_list = c("Booned",
#               "QWERTYMartin")
# user_list = c("Aeszett")

# user_list = c("mrbananagrabber",
#               "rahdo",
#               "Quinns",
#               "Ogzz",
#               "GOBbluth89")
# 
# user_list = "GOBbluth89"
# user_list = "mrbananagrabber"
# 
# user_list = "mrbananagrabber"