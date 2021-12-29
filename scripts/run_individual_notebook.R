# run user notebook
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

library(tidyverse)
library(foreach)

user_list = c("mrbananagrabber",
              "rahdo",
              "Quinns",
              "Ogzz",
              "GOBbluth89")

# run through
foreach(i=1:length(user_list)) %do% {
        rmarkdown::render(here::here("notebook_for_modeling_individuals.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = 2020),
                          output_file =  user_list[i],
                          output_dir = "individual_reports")
}