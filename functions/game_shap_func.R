game_shap_func = function(input_workflows,
                         input_game_data) {
        
        
        # extract game name info
        game_info = input_game_data
        
        # game_info with estimated averageweight
        estimated_averageweight = input_workflows %>%
                as_tibble() %>%
                filter(outcome == 'averageweight') %>%
                filter(grepl("xgbTree", wflow_id)) %>%
                mutate(fit = map(.workflow, ~ .x %>% 
                                         extract_fit_parsnip)) %>%
                mutate(recipe = map(.workflow, ~ .x %>% extract_recipe)) %>%
                mutate(features = map(recipe, ~ .x %>% summary %>% 
                                              filter(role == 'predictor') %>%
                                              pull(variable))) %>%
                mutate(preds = map(.workflow, ~ .x %>%
                                           predict(game_info))) %>%
                select(outcome, wflow_id, preds) %>% unnest(preds)
        
        # change averageweight to estimated
        game_info$averageweight = estimated_averageweight$.pred
        
        # now run through worfkflows
        game_inputs = input_workflows %>%
                as_tibble() %>%
                mutate(tidy_outcome = str_to_title(case_when(outcome == 'bayesaverage' ~ 'geek rating',
                                                             outcome == 'average' ~ 'Average Rating',
                                                             outcome == 'averageweight' ~ 'Average Weight',
                                                             outcome == 'usersrated' ~ 'users rated',
                                                             TRUE ~ outcome))) %>%
                filter(grepl("xgbTree", wflow_id)) %>%
                mutate(fit = map(.workflow, ~ .x %>% 
                                         extract_fit_parsnip)) %>%
                mutate(recipe = map(.workflow, ~ .x %>% extract_recipe)) %>%
                mutate(features = map(recipe, ~ .x %>% summary %>% 
                                              filter(role == 'predictor') %>%
                                              pull(variable))) %>%
                mutate(preds = map(.workflow, ~ .x %>%
                                           predict(game_info))) %>%
                mutate(data = map(.workflow, ~ .x %>% 
                                          extract_recipe() %>%
                                          bake(new_data = game_info) %>%
                                          select(-name) %>%
                                          bind_cols(., game_info %>%
                                                            select(name))))
        
        # game_preds
        game_preds = game_inputs %>%
                select(tidy_outcome, outcome, wflow_id, preds, data) %>%
                mutate(data = map(data,
                                  ~ .x %>% select(game_id))) %>%
                unnest(data, preds) %>%
                mutate(.pred = case_when(outcome == 'usersrated' ~ plyr::round_any(exp(.pred),50),
                                         TRUE ~ .pred)) %>%
                mutate(.pred = round(.pred, 2))
        
        # extract the baked data
        game_data = game_inputs %>%
                head(1) %>%
                select(data) %>%
                unnest(data)
        
        # now convert to 
        game_shap = game_inputs %>%
                mutate(matrix = map2(data,
                                     features, 
                                     ~ .x %>%
                                             select(one_of(.y)) %>%
                                             as.matrix())) %>%
                mutate(mod = map(fit, ~ .x %$% fit)) %>%
                mutate(shap = map2(.x = mod,
                                   .y = matrix,
                                   ~ fastshap::explain(object =.x,
                                                       X = .y,
                                                       exact = T))) %>%
                mutate(shap_ind = map2(shap,
                                       data,
                                       ~ .x %>% 
                                               as.data.frame() %>%
                                               mutate(.row = row_number()) %>%
                                               bind_cols(., .y %>%
                                                                 select(game_id, name)))) %>%
                select(tidy_outcome, outcome, wflow_id, shap_ind) %>%
                unnest(shap_ind) 
        
        # in format for plotting
        shap_gathered = game_shap %>%
                gather("key", "shap",
                       -tidy_outcome,
                       -outcome,
                       -wflow_id,
                       -.row,
                       -game_id,
                       -name) %>%
                left_join(., 
                          game_data %>%
                                  select_if(is.numeric) %>%
                                  gather("key", "value",
                                         -game_id),
                          by = c("game_id", "key")) %>%
                group_by(outcome, wflow_id, game_id) %>%
                slice_max(.,
                          order_by = abs(shap),
                          n = 15,
                          with_ties = F) %>%
                mutate(key = paste(tidy_name_func(key),
                                   "=",
                                   round(value, 2),
                                   sep="")) %>%
                left_join(., input_game_data %>% # add actual
                                  select(game_id,
                                         average,
                                         bayesaverage,
                                         usersrated,
                                         averageweight) %>%                            
                                  gather("outcome", "actual",
                                         -game_id),
                          by = c("game_id", "outcome")) %>%
                left_join(., game_preds %>%
                                  select(outcome,
                                         .pred,
                                         game_id),
                          by = c("game_id", "outcome")) %>%
                mutate_at(vars("actual", ".pred"),
                          ~ round(.,2)) %>%
                mutate(tidy_subtitle = paste(tidy_outcome,
                                             paste("Predicted:", .pred),
                                             paste("Actual:", actual),
                                             sep = "\n"))
        # 
        # # code for plotting
        plot_shap = shap_gathered %>%
                ggplot(., aes(x=shap,
                              fill = shap,
                              y = reorder_within(key, shap, outcome)))+
                geom_col()+
                facet_wrap(tidy_subtitle~.,
                           ncol =2,
                           scales = "free")+
                theme_phil()+
                theme(legend.title = element_text()) +
                scale_fill_gradient2(low = "red",
                                     mid = "grey60",
                                     high = "blue",
                                     midpoint = 0,
                                     limits = c(-0.1, 0.1),
                                     oob = scales::squish)+
                guides(fill ="none")+
                # guides(fill = guide_colorbar(barwidth=10,
                #                              barheight = 0.5,
                #                              title = "Shapley shap",
                #                              title.position = 'top'))+
                my_caption+
                geom_vline(xintercept = 0,
                           linetype = 'dashed')+
                xlab("Shapley Values")+
                ylab("Features")+
                scale_y_reordered()+
                ggtitle(paste(paste("Game:",
                                    game_info$name),
                              paste("ID:",
                                    game_info$game_id),
                              sep="\n"),
                        str_wrap("Displaying Shapley Values to identify which features were the most influential for each model's prediction. Features that increased a game's prediction are positive (in blue), while features that decreased a prediction are negative (in red).", 125))+
                # subtitle = paste(select(game_preds %>% 
                #                                 mutate(subtitle = paste("Estimated ", tidy_outcome, ": ", .pred, sep="")), subtitle) %>%
                #                          pull(), collapse="\n"))+
                theme(plot.title = element_text(size=12),
                      strip.text.x = element_text(size = 10))
        
        
        out = list("data" = game_data,
                   "inputs" = game_inputs,
                   "preds" = game_preds,
                   "raw_shap" = game_shap,
                   "top_shap" = shap_gathered,
                   "plot" = plot_shap)
        
        return(out)
        
}
