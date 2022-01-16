# load necessary pieces
source(here::here("scripts/load_packages.R"))
source(here::here("functions/theme_phil.R"))
source(here::here("functions/get_game_record.R"))
source(here::here("functions/get_bgg_data_from_github.R"))
source(here::here("functions/run_game_report.R"))

# # select agme ids
ids = c(340466,
        331363,
        342942,
        9209,
        228328,
        300217,
        283155,
        161533,
        343905,
        237179,
        23540,
        237179,
        326030,
        350890,
        348303,
        299684,
        342942,
        317511,
        139443)

run_game_report(ids)

rm(list=ls())


# id = 228328
# ids = c(300217,
#         283155,
#         161533)
 # id = 300217
 # id = 283155
 # id = 161533
# id = 343905
# id = 237179
# id = 23540
# id = 229853
# id = 237179
# id = 326030
# id = 350890
# id = 348303
# id = 299684
# id = 342942
# id = 317511
# id = 139443

# # get top 100 from today
# today = get_bgg_data_from_github(Sys.Date())
# 
# ids = today %>%
#         arrange(desc(bayes_average)) %>%
#         pull(game_id)
# 
# # run and produce report for selected ids
# run_game_report(ids[1:250])

# # find game
# today %>%
#         filter(grepl("Terra Mystica", game_name))
# 
# # select agme ids
# id = 124361
# 
# # run and produce report for selected ids
# run_game_report(id)
