run_game_report <-
function(input_ids)
{
        
        names_df = suppressMessages({get_game_record(input_ids) %>%
                        mutate(name = tolower(gsub("[[:space:]]", "-", gsub("\\s+", " ", gsub("[[:punct:]]","", name))))) %>%
                        select(name, game_id) %>%
                        mutate(name_id = paste(name, game_id, sep="_")) %>%
                        select(game_id, name_id)
        })
        
        # run through
        foreach(i=1:length(input_ids)) %do% {
                rmarkdown::render(here::here("find_game_comparables/get_comparables_report.Rmd"),
                                  params = list(id = names_df$game_id[i]),
                                  output_file =  names_df$game_id[i],
                                  output_dir = here::here("find_game_comparables/game_reports"))
        }
        
}
