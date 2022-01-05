# run examine models
library(tidyverse)
library(foreach)

rmarkdown::render(here::here("predict_ratings/examine_models.Rmd"), 
                  # output_file =  paste("report_",
                  #                      Sys.Date(),
                  #                      sep=""),
                  output_file = "predicted_bgg_ratings",
                  output_dir = here::here("predict_ratings/predictions"))

rm(list=ls())