plot_posterior <-
function(posterior_preds) {
        
        ### get background data for context
        # get training set
        all_files = list.files(here::here("deployment"))
        
        # # get dataset
        most_recent_games = all_files[grepl("games_datasets_ratings", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        # quantiles
        baverage_quantiles = quantile(games_datasets$train$baverage, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>%
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'baverage')
        
        # average quanties
        average_quantiles = quantile(games_datasets$train$average, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>% 
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'average')
        
        quantile_data = list("baverage" = baverage_quantiles,
                             "average" = average_quantiles)
        
        # make a dummy plot
        dummy = data.frame(outcome_type = "baverage",
                           min = 5,
                           max = 9) %>%
                bind_rows(data.frame(outcome_type = "average",
                                     min = 4,
                                     max = 9)) %>%
                melt(id.vars = c("outcome_type")) %>%
                set_names(., c("outcome_type", "variable", "range"))
        
        quantile_dummy = bind_rows(quantile_data$baverage %>%
                                           filter(perc %in% c(50, 90, 99)),
                                   quantile_data$average %>%
                                           filter(perc %in% c(50, 90, 99)))
        
        # make dummy plot
        dummy_plot = ggplot(dummy, aes(x=range))+
                facet_wrap(~outcome_type,
                           ncol = 1,
                           scales = "free_x")+
                theme_phil()+
                xlab("Predicted Value")+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 50),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 90),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 99),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 50),
                          aes(x=value,
                              y = 60),
                          size =2,
                          label = 'median game on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 90),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 10% on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 99),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 1% on bgg')+
                #   coord_cartesian(xlim = c(4, 10))+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())
        
        
        #### add sims from game to plot
        plot_obj = posterior_preds$sims 
        # plot
        plot = dummy_plot + 
                geom_histogram(data = plot_obj,
                               aes(x=value),
                               bins = 100,
                               alpha = 0.7,
                               fill = 'grey60',
                               color = '#F0F0F0')+
                geom_vline(data = plot_obj,
                           aes(xintercept=outcome),
                           alpha=0.9,
                           col = "blue")+
                geom_text(data = plot_obj,
                          aes(x=outcome),
                          label = "current bgg rating",
                          size = 2,
                          y= 30,
                          alpha=0.9,
                          col = "blue")+
                facet_wrap(name~outcome_type,
                           ncol = 1)+
                xlab("Predicted Value")+
                ylab("# of Simulations")+
                coord_cartesian(xlim = c(4, 10),
                                default = T)
        
        
        return(plot)
        
}
