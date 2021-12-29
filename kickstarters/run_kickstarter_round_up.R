# run kickstarter file

u = "https://www.reddit.com/r/boardgames/comments/reu8it/crowdfunding_roundup_dec_12_2021_20_ending_soon/"

file = as_tibble(u) %>%
        rename(link = value) %>%
        mutate(post = gsub("https://www.reddit.com/r/boardgames/comments/",
                           "", link)) %>%
        select(post, link) %>%
        separate(post, c("post_id", "post_title"), sep = "\\/") %>%
        mutate(post_date = gsub("crowdfunding_roundup_", "", gsub("_ending_soon", "", gsub("kickstarter_roundup_", "", post_title)))) %>%
        select(starts_with("post_"), everything()) %>%
        mutate(date_char = str_to_title(gsub("_", " ", substr(post_date, 1, 11)))) %>%
        pull(post_date)
        
rmarkdown::render(here::here("kickstarters/kickstarter_roundup.Rmd"), 
                  params = list(url = u),
                  output_file =  file,
                  output_dir = here::here("kickstarters/roundup_tables"))
