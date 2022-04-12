game_posterior_func = function(input_workflows,
                          input_game_data) {
        
        
        # extract game name info
        game_info = input_game_data
        
        # simulate averageweight
        sims_averageweight = input_workflows %>%
                as_tibble() %>%
                filter(outcome == 'averageweight') %>%
                filter(grepl("stan", wflow_id)) %>%
                mutate(fit = map(.workflow, ~ .x %>% 
                                         extract_fit_parsnip %$%
                                         fit)) %>%
                mutate(recipe = map(.workflow, ~ .x %>% extract_recipe)) %>%
                mutate(baked = map(recipe, ~ .x %>% bake(new_data = game_info))) %>%
                mutate(preds = map2(fit, 
                                    baked,
                                    ~ .x %>%
                                           posterior_predict(newdata =.y,
                                                             draws = 500) %>%
                                            tidy_draws %>%
                                            gather(".row", "averageweight",
                                                   -.chain, -.iteration, -.draw) %>%
                                            mutate(averageweight = case_when(averageweight > 5 ~ 5,
                                                                             averageweight < 1 ~ 1,
                                                                             TRUE ~ averageweight)))) %>%
                mutate(game_id = map(baked,
                                     ~ .x %>% select(game_id))) %>%
                select(outcome, wflow_id, outcome, game_id, preds) %>%
                unnest(c(game_id, preds)) %>%
                select(outcome, wflow_id, game_id, .draw, averageweight) %>%
                rename(.draw_averageweight = .draw)
        
        # add simulations to game info
        game_info_estimated = game_info %>% 
                select(-averageweight) %>%
                left_join(.,
                          sims_averageweight,
                          by = c("game_id"))
        
        # expand
        sims_averageweight = sims_averageweight %>% 
                select(-outcome, -wflow_id, -game_id) %>%
                nest(-.draw_averageweight) %>% 
                mutate(data = map(data, ~ .x %>% 
                                          dplyr::slice(rep(seq_len(n()), 1000)))) %>%
                unnest(data)
        
        # simulate average given estimated averageweight
        set.seed(1999)
        sims_average = input_workflows %>%
                as_tibble() %>%
                filter(outcome == 'average') %>%
                filter(grepl("stan", wflow_id)) %>%
                mutate(fit = map(.workflow, ~ .x %>% 
                                         extract_fit_parsnip %$%
                                         fit)) %>%
                mutate(recipe = map(.workflow, ~ .x %>% extract_recipe)) %>%
                select(.id, wflow_id, outcome, fit, recipe) %>%
                mutate(baked = map(recipe,
                                    ~ .x %>% bake(new_data = game_info_estimated))) %>%
                mutate(preds = map2(fit, 
                                    baked,
                                    ~ .x %>%
                                            posterior_predict(newdata =.y,
                                                              draws = 1000) %>%
                                            tidy_draws %>%
                                            gather(".row", "average",
                                                   -.chain, -.iteration, -.draw) %>%
                                            mutate(average = case_when(average > 10 ~ 10,
                                                                       average < 1 ~ 1,
                                                                       TRUE ~ average)))) %>%
                mutate(game_id = map(baked,
                                     ~ .x %>% select(game_id) %>% head(1))) %>%
                select(outcome, wflow_id, outcome, game_id, preds) %>%
                unnest(c(game_id, preds)) %>%
                select(outcome, wflow_id, game_id, average)

        set.seed(1999)
        sims_usersrated= input_workflows %>%
                as_tibble() %>%
                filter(outcome == 'usersrated') %>%
                filter(grepl("stan", wflow_id)) %>%
                mutate(fit = map(.workflow, ~ .x %>% 
                                         extract_fit_parsnip %$%
                                         fit)) %>%
                mutate(recipe = map(.workflow, ~ .x %>% extract_recipe)) %>%
                mutate(baked = map(recipe, ~ .x %>% bake(new_data = game_info_estimated))) %>%
                mutate(preds = map2(fit, 
                                    baked,
                                    ~ .x %>%
                                            posterior_predict(newdata =.y,
                                                              draws = 1000) %>%
                                            tidy_draws %>%
                                            gather(".row", "usersrated",
                                                   -.chain, -.iteration, -.draw) %>%
                                            mutate(usersrated = plyr::round_any(exp(usersrated), 100, ceiling)))) %>%
                mutate(game_id = map(baked,
                                     ~ .x %>% select(game_id) %>% head(1))) %>%
                select(outcome, wflow_id, outcome, game_id, preds) %>%
                unnest(c(game_id, preds)) %>%
                select(outcome, wflow_id, game_id, usersrated)
        
        # combine all
        # plot simulated averageweight vs simulated average
        set.seed(1)
        sims = bind_cols(sims_average,
                  sims_averageweight,
                  sims_usersrated %>%
                          select(usersrated)) %>%
                mutate(bayesaverage = ((1500*5.5) + (usersrated*average)) /
                               (1500 + usersrated)) %>%
                left_join(., game_info %>%
                                  select(game_id, name, yearpublished),
                          by = "game_id") %>%
                select(wflow_id, yearpublished, game_id,name, .draw_averageweight, average, averageweight, usersrated, bayesaverage)
        
        # # plot simulated averageweight vs simulated average
        # set.seed(1)
        # sims %>%
        #         sample_n(10000) %>% 
        #         ggplot(., aes(x=averageweight, y=average))+
        #         geom_jitter(alpha = 0.5, height=0.25, width=0.15)+theme_phil()+
        #         coord_cartesian(xlim = c(1, 5))
        # 
        # # now average vs usersrated
        # set.seed(1)
        # sims %>%
        #         sample_n(10000) %>% 
        #         ggplot(., aes(x=averageweight, y=usersrated))+
        #         geom_jitter(alpha = 0.5, height=0.5, width=0.15)+theme_phil()+
        #         coord_cartesian(xlim = c(1, 5))+
        #         scale_y_log10()
        # 
        # # now average vs usersrated
        # set.seed(1)
        # sims %>%
        #         sample_n(10000) %>% 
        #         ggplot(., aes(x=average, y=usersrated))+
        #         geom_jitter(alpha = 0.5, height=0.5, width=0.15)+theme_phil()+
        #         coord_cartesian(xlim = c(4, 10))+
        #         scale_y_log10()
        # 
        # # now average vs bayesaverage
        # set.seed(1)
        # sims %>%
        #         sample_n(10000) %>% 
        #         ggplot(., aes(x=averageweight, y=bayesaverage))+
        #         geom_jitter(alpha = 0.5, height=0.15, width=0.15)+theme_phil()+
        #         coord_cartesian(xlim = c(1, 5),
        #                         ylim = c(4, 9))+
        #         geom_smooth()
        # 
        
        # cutpoints
        cuts = c(0.05, 0.10, 0.5, 0.9, 0.95)
        
        set.seed(1)
        quantiles = sims %>%
                group_by(game_id, name, yearpublished) %>%
                summarise(quantile = scales::percent(cuts),
                          average = quantile(average, cuts),
                          averageweight = quantile(averageweight, cuts),
                          usersrated = quantile(usersrated, cuts),
                          bayesaverage = quantile(bayesaverage, cuts),
                          .groups = 'drop') %>%
                mutate(quantile = gsub("\\.0", "", quantile))
        
        table = quantiles %>%
                group_by(quantile) %>% 
                gather("variable", "value", -quantile, -game_id, -name, -yearpublished) %>% 
                spread(quantile, value) %>%
                mutate_if(is.numeric, round, 2) %>%
                mutate_if(is.numeric, as.character) %>%
                mutate(rank = case_when(variable == 'average' ~ 2,
                                        variable == 'averageweight' ~ 1,
                                        variable == 'usersrated' ~ 3,
                                        variable == 'bayesaverage' ~ 4)) %>%
                mutate(variable = case_when(variable == 'average' ~ 'Average Rating',
                                           variable == 'bayesaverage' ~ 'Geek Rating',
                                           variable == 'usersrated' ~ 'User Ratings',
                                           variable == 'averageweight' ~ 'Average Weight')) %>%
                arrange(rank) %>%
                rename(ID = game_id,
                       Name = name,
                       Published = yearpublished,
                       Outcome = variable) %>%
                # rename(Estimate = '50%',
                #        CI_Low = '5%',
                #        CI_High = '90%') %>%
          #      select(Published, ID, Name, Outcome, Estimate, CI_Low, CI_High) %>%
                select(Published, ID, Name, Outcome, `5%`, `10%`, `50%`, `90%`, `95%`) %>%
                flextable() %>%
                autofit() %>%
                bg(., j = c('50%'),
                   bg = 'grey70') %>%
                bg(., j = c('10%', '90%'),
                   bg = 'grey90') %>%
                flextable::align(align = "center",
                                 part = 'all',
                                 j = c("5%", "10%", "50%", "90%", "95%"))

        out = list("sims" = sims,
                   "quantiles" = quantiles,
                   "table" = table)
        
        return(out)
        
}

