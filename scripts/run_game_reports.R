# load necessary pieces
source(here::here("scripts/load_packages.R"))
source(here::here("functions/theme_phil.R"))
source(here::here("functions/get_game_record.R"))
source(here::here("functions/get_bgg_data_from_github.R"))

# function
run_game_report = function(input_ids)
{
        
        names_df = suppressMessages({get_game_record(input_ids) %>%
                        mutate(name = tolower(gsub("[[:space:]]", "-", gsub("\\s+", " ", gsub("[[:punct:]]","", name))))) %>%
                        select(name, game_id) %>%
                        mutate(name_id = paste(name, game_id, sep="_")) %>%
                        select(game_id, name_id)
        })
        
        # run through
        foreach(i=1:length(input_ids)) %do% {
                rmarkdown::render(here::here("examine_comparables.Rmd"),
                                  params = list(id = names_df$game_id[i]),
                                  output_file =  names_df$game_id[i],
                                  output_dir = "game_reports")
        }

}

# # select agme ids
# id = 317511
# id = 283155
# id = 161533
# id = 343905
# id = 237179
id = 23540
id = 229853
id = 237179
id = 326030
id = 350890
id = 348303
id = 299684
id = 342942
id = 317511

# # run and produce report for selected ids
run_game_report(id)

# get top 100 from today
today = get_bgg_data_from_github(Sys.Date())

ids = today %>%
        arrange(desc(bayes_average)) %>%
        pull(game_id)

# run and produce report for selected ids
run_game_report(ids[1:250])

# # find game
# today %>%
#         filter(grepl("Terra Mystica", game_name))
# 
# # select agme ids
# id = 124361
# 
# # run and produce report for selected ids
# run_game_report(id)
