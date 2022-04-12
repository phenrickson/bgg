# create function to scrape all bgg ids
get_all_bgg_ids = function(minpages = 1,
                           maxpages = 1000) {
        
        # packages used
        require(tidyverse)
        require(dplyr)
        require(data.table)
        require(foreach)
        require(httr)
        require(xml2)
        require(rvest)
        
        # loop
        foreach(i = minpages:maxpages, .combine = bind_rows) %do% {
                
                # get url for individual page
                url = paste("https://boardgamegeek.com/browse/boardgame/page/", i, "/?sort=rank", sep="")
                
                doc = xml2::read_html(url)
                
                # get the game nodes with game ids
                id_nodes = 
                        doc %>% html_nodes("a")
                
                # convert to dataframe
                id_df = bind_rows(
                        lapply(xml_attrs(id_nodes), 
                               function(x) data.frame(as.list(x), stringsAsFactors=FALSE))) %>%
               #         filter(grepl("/boardgame/", href)) %>%
                        filter(class == 'primary') %>%
                        as_tibble() %>%
                        mutate(page = i) %>%
                        select(page, href) %>%
                        separate(href, into = c("extra", "type", "game_id", "name"), sep="/") %>%
                        mutate(date = Sys.Date()) %>%
                        select(date, page, type, game_id, name)
                
                # get all games and then strip the text
                text <- html_nodes(doc, "tr") %>%
                        html_text() %>%
                        as_tibble() %>%
                        mutate(stripped = gsub("\\\t", "", gsub("\\\n", "", value))) %>%
                        select(stripped) %>%
                        filter(!grepl("Board Game Rank", stripped)) %>%
                        pull(stripped)
                
                # extract in between parnetheses
                list = str_extract_all(text, "\\([^()]+\\)")
                # fill empty
                list[sapply(list, is_empty)] <- NA
                
                # get yearpublished
                year_df = do.call("rbind", list) %>%
                        as_tibble()
                
                # combine
                out = bind_cols(id_df, year_df) %>%
                        mutate(url = url) %>%
                        select(date, url, everything())
                
                # output tracker
                cat(paste("page", i, "of", maxpages, "complete"), sep="\n")
                
                # pause
                # pause for 5 seconds
                Sys.sleep(5)
                
                out
        
        }

}

