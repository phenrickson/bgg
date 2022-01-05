# run examine models
library(tidyverse)
library(foreach)

# run, update active html file
# also saves data from day of run
rmarkdown::render(here::here("adjusted_bgg_ratings/notebook_adjust_ratings.Rmd"), 
                  output_file =  "adjusted_bgg_ratings",
                  output_dir = here::here("adjusted_bgg_ratings/"))

rm(list=ls())