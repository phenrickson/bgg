get_game_comparables <-
function(id) {
        
        # load function
        source(here::here("functions/get_game_record.R"))
        source(here::here("functions/baverage_func.R"))
        source(here::here("functions/average_func.R"))
        source(here::here("functions/avgweight_func.R"))
        source(here::here("functions/theme_phil.R"))
        
        # load active files
        unsupervised_obj = 
                readr::read_rds(here::here("active", "unsupervised_obj.Rdata"))
        
        unsupervised_neighbors = 
                readr::read_rds(here::here("active", "unsupervised_neighbors.Rdata")) 
        
        recipe_prep = 
                readr::read_rds(here::here("active", "unsupervised_recipe_prep.Rdata"))
        
        games_flattened = 
                readr::read_rds(here::here("active", "games_flattened.Rdata"))  %>%
                select(game_id,
                       name,
                       yearpublished,
                       average,
                       baverage,
                       avgweight,
                       playingtime,
                       usersrated,
                       minplayers,
                       maxplayers)
        
        # check to see if id is in the unsupervised object
        check_obs = unsupervised_obj %>%
                filter(dataset == 'fundamentals, mechanics, and categories') %>%
                select(dataset, pca_with_data) %>%
                unnest(c(dataset, pca_with_data)) %>%
                filter(game_id == id) %>%
                nrow()
        
        # get table of games
        active_games = unsupervised_obj %>% 
                filter(dataset == 'fundamentals, mechanics, and categories') %>%
                select(pca_with_data) %>% unnest() %>%
                arrange(desc(baverage)) %>%
                mutate(rank = row_number())
        
        ### if game record is in our previously run analysis, we can just look it up
        
        if(check_obs > 0) {
                
                # get game name
                game = unsupervised_neighbors %>%
                        filter(game_id == id) %>%
                        pull(name) %>%
                        unique()
                
                neighbors_table = unsupervised_neighbors %>%
                        filter(game_id == id) %>%
                        #    filter(dataset == 'fundamentals, mechanics, and categories') %>%
                        select(dataset, game_id, name, neighbor_game_id, neighbor_name, similarity, dist, perc, yearpublished, rank, average, baverage, avgweight) %>%
                        filter(yearpublished < 2022) %>%
                        mutate(game_id = as.character(game_id),
                               neighbor_game_id = as.character(neighbor_game_id),
                               yearpublished = as.character(yearpublished)) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage) %>%
                        group_by(dataset) %>%
                        arrange(dataset, dist) %>%
                        mutate(rank = row_number()) %>%
                        ungroup() %>%
                        filter(rank <=25) %>%
                        rename(Comparing_By = dataset,
                               Similarity = similarity,
                               ID = neighbor_game_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor_name,
                               Rank = rank) %>%
                        select(Comparing_By, Similarity, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2)
                
        } else {
                
                print("game not in existing dataset; pulling game info from BGG and calculating...")
                
                # if the game isn't present, we need to go grab it and then add it to our existing games
                game_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
                
                game = game_record %>%
                        pull(name)
                
                # nest for placemen
                # bak
                baked_game = recipe_prep %>%
                        prep(recipe_prep$template, strings_as_factor = F) %>%
                        bake(new_data = bind_rows(game_record, recipe_prep$template[0,]))
                
                # nest
                nested_game_data<- baked_game %>%
                        mutate(dataset = "fundamentals, mechanics, and categories") %>%
                        nest(-dataset) %>%
                        bind_rows(., baked_game %>%
                                          mutate(dataset = "fundamentals and mechanics") %>%
                                          select(-starts_with("cat_"),
                                                 -number_categories) %>%
                                          nest(-dataset))
                
                ### get comparables for game
                comps_obj = unsupervised_obj %>%
                        select(dataset, pca_trained) %>% # get pca
                        left_join(., nested_game_data,
                                  by = "dataset") %>%
                        mutate(pca_rotation = map2(.x = pca_trained,
                                                   .y = data,
                                                   ~ .x %>% bake(new_data = .y))) %>%
                        mutate(pca_with_data = map2(.x = pca_rotation,
                                                    .y = data,
                                                    ~ .x %>%
                                                            select(game_id, starts_with("PC")) %>%
                                                            left_join(., .y,
                                                                      by = "game_id"))) %>%
                        mutate(type = "game") %>%
                        select(dataset, pca_with_data, type) %>%
                        bind_rows(., unsupervised_obj %>%
                                          select(dataset, pca_with_data) %>%
                                          unnest() %>%
                                          filter(game_id != id) %>%
                                          nest(-dataset, -type) %>%
                                          rename(pca_with_data = data)) %>%
                        mutate(pca_with_data = map(pca_with_data, ~.x %>%
                                                           rename_all(funs(gsub("PC0", "PC", gsub("PC00", "PC", make.names(names(.x)))))))) %>%
                        unnest() %>%
                        nest(-dataset) %>%
                        rename(pca_with_data = data)
                
                # now combine
                unsupervised_obj = comps_obj %>%
                        left_join(., 
                                  unsupervised_obj %>%
                                          select(dataset, kmeans, norm_trained, pca_trained),
                                  by = "dataset") %>%
                        mutate(pca_dist = map(.x = pca_with_data, 
                                              ~ dist(.x %>%
                                                             select(PC1:PC10) %>%
                                                             as.matrix(), 
                                                     method="euclidean") %>%
                                                      as.matrix() %>%
                                                      as.data.frame() %>%
                                                      magrittr::set_rownames(.x$game_id) %>%
                                                      magrittr::set_colnames(.x$game_id))) %>%
                        mutate(obs_dist = map(pca_dist, ~ .x %>%
                                                      rownames_to_column("game_id") %>%
                                                      filter(game_id == id) %>%
                                                      gather('closest','dist',-game_id) %>%
                                                      filter(dist > 0) %>%
                                                      filter(!is.na(dist)) %>% 
                                                      group_by(game_id) %>% 
                                                      arrange(dist) %>% 
                                                      slice_min(dist, n=50, with_ties = T) %>%
                                                      mutate(dist_rank=row_number()))) %>%
                        mutate(neighbors = map(obs_dist,
                                               ~ left_join(.x, game_record %>%
                                                                   mutate(game_id = as.character(game_id)),
                                                           by = c("game_id")) %>%
                                                       select(game_id, name, closest, dist, dist_rank) %>% 
                                                       left_join(., active_games %>%
                                                                         mutate(game_id = as.character(game_id)) %>%
                                                                         rename(neighbor_game_id = game_id,
                                                                                neighbor_name = name),
                                                                 by = c("closest" = "neighbor_game_id")))) %>%
                        mutate(scale_data = map2(.x = norm_trained,
                                                 .y = pca_with_data,
                                                 ~ .x %>% bake(new_data = .y %>%
                                                                       select(-starts_with("PC")) %>%
                                                                       filter(game_id == id)) %>%
                                                         select(-timestamp,
                                                                -game_id,
                                                                -name,
                                                                -average,
                                                                -baverage,
                                                                -usersrated,
                                                                -yearpublished))) %>%
                        mutate(clusters = map2(.x = kmeans,
                                               .y = scale_data,
                                               ~ clue::cl_predict(.x, 
                                                                  newdata = .y)))
                
                # extract neighbors and report
                neighbors_table= unsupervised_obj %>%
                        select(dataset, neighbors) %>% 
                        unnest() %>%
                        rename(neighbor = neighbor_name,
                               neighbor_id = closest) %>%
                        mutate(similarity = 100*1/(1+ sqrt(dist))) %>%
                        select(dataset, game_id, name, neighbor, neighbor_id, similarity, dist, dist_rank) %>%
                        left_join(., active_games %>%
                                          mutate(game_id = as.character(game_id)) %>%
                                          rename(neighbor_id = game_id,
                                                 neighbor = name),
                                  by = c("neighbor_id", "neighbor")) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage)  %>%
                        filter(yearpublished < 2022) %>% # set years allowed for nearest neighbors
                        group_by(dataset) %>%
                        mutate(rank = row_number()) %>%
                        filter(rank <=25) %>%
                        ungroup() %>%
                        mutate_if(is.numeric, round, 2) %>%
                        #  select(-game_id, -similarity, -dist, -dist_rank) %>%
                        mutate(yearpublished = as.character(yearpublished),
                               neighbor_id = as.character(neighbor_id)) %>%
                        rename(Comparing_By = dataset,
                               Similarity = similarity,
                               ID = neighbor_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor,
                               Rank = rank) %>%
                        select(Comparing_By, Similarity, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2)
                
        }
        

        # convert to flextable
        neighbors_table_ft = neighbors_table %>%
                select(Rank, Similarity, Published, ID, Neighbor, BGGRating, GeekRating, Complexity) %>%
                filter(Rank <=25) %>%
             #   select(-Game) %>%
                flextable() %>%
                flextable::autofit() %>%
             #   add_footer(paste("Most similar games to", game, "using complexity, playing time, player count, mechanics, and categories", sep=" ")) %>%
                # bg(., i = ~ Comparing_By =='fundamentals, mechanics, and categories',
                #    bg = 'grey100') %>%
                # bg(., i = ~ Comparing_By == 'fundamentals and mechanics',
                #    bg = 'grey90') %>%
                bg(., j = c("GeekRating"),
                   bg = baverage_func) %>%
                bg(., j = c("BGGRating"),
                   bg = average_func) %>%
                bg(., j = c("Complexity"),
                   bg = avgweight_func)
        
        
        # now make plot
        # make visualization to compare observations on the principal components
        df = unsupervised_obj[1,]$pca_with_data[[1]] %>%
                select(game_id, name, PC1:PC10)
        
        # selected principal components
        profile_df = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T)) %>%
                ungroup() 
        
        # make a plot
        plot_df = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                mutate(variable = case_when(variable == 'PC1' ~ 'PC1_Complexity',
                                            variable == 'PC2' ~ 'PC2_Thematic',
                                            variable == 'PC3' ~ 'PC3_Economy',
                                            variable == 'PC4' ~ 'PC4_Cooperation')) %>%
                filter(!is.na(variable)) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T))
        
        # full
        plot_df2 = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                group_by(variable) %>%
                mutate(scale = scale(value, center=T, scale=T))
               # do(data.frame(., perc = ecdf(.$value)(.$value)))

        # jitter
        pos <- position_jitter(width = 0.15, seed = 2)
        pos2 <- position_jitter(width = 0.075, seed = 2)
        
        # make background plot
        background = plot_df %>%
                ggplot(., aes(x=variable,
                              y = scale))+
                geom_jitter(alpha=0.1,
                            col = 'grey60',
                            position = pos)+
                theme_phil()+
                geom_hline(yintercept = 0,
                           linetype = 'dashed',
                           alpha = 0.8)+
                theme(legend.position = 'top',
                      legend.title = element_blank())+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())+
                ggtitle(paste("Which games are similar to ", game, "?", sep= ""),
                        subtitle = str_wrap("Placing games on first four principal components of variation: complexity, theme, economy, and coooperation.", 125))
        
        # closest IDs
        compare = c(neighbors_table %>%
                            filter(Rank <=6) %>%
                            pull(ID))
        
        # all neighbor IDs
        compare_full = c(neighbors_table %>%
                            pull(ID))
        # all neighbors
        neighbors = c(game, neighbors_table %>%
                              group_by(Neighbor) %>%
                              mutate(dupe = n_distinct(Neighbor)) %>%
                              mutate(Neighbor = case_when(Neighbor == game ~ paste(Neighbor, Published, sep="_"),
                                                          dupe > 1 ~ paste(Neighbor, Published, sep="_"),
                                                          TRUE ~ Neighbor)) %>%
                              ungroup() %>%
                              pull(Neighbor)) %>%
                rev()
        
        # place on principal components
        compare_plot = background + 
                # geom_jitter(data = plot_df %>%
                #                           filter(game_id %in% id),
                #                   aes(x = variable,
                #                       size = highlight,
                #                       y = scale),
                #             color = "black",
                #             size = 2.5,
                #             position = pos2)+
                geom_jitter(data = plot_df %>%
                                    filter(game_id %in% c(id, compare)),
                            aes(x = variable,
                                color = name,
                                y = scale),
                            size = 2,
                            position = pos2)+
                geom_label_repel(data = plot_df %>%
                                         filter(game_id %in% c(id,compare)),             
                                 aes(x = variable,
                                     color = name,
                                     y=scale,
                                     label = name),
                                 position = pos2,
                                 max.overlaps=15,
                                 show.legend=F,
                                 size = 3)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option="magma",
                                      begin = 0.2,
                                      end = 0.8)
        
        ### remake but with all components
        # make background plot
        background2 = plot_df2 %>%
                ggplot(., aes(x=variable,
                              label = name,
                              y = scale))+
                geom_text(vjust = 0.1,
                          color = "grey60",
                          size =3,
                          position = pos,
                          check_overlap=T)+
                theme_phil()+
                geom_hline(yintercept = 0,
                           linetype = 'dashed',
                           alpha = 0.8)+
                theme(legend.position = 'top',
                      legend.title = element_blank())+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
        
        # full components
        compare_plot2 = background2 + 
                # geom_jitter(data = plot_df %>%
                #                           filter(game_id %in% id),
                #                   aes(x = variable,
                #                       size = highlight,
                #                       y = scale),
                #             color = "black",
                #             size = 2.5,
                #             position = pos2)+
                # geom_jitter(data = plot_df2 %>%
                #                     filter(game_id %in% c(id, compare_full)),
                #             aes(x = variable,
                #                 color = name,
                #                 y = scale),
                #             size = 2,
                #             position = pos2)+
                geom_text(data = plot_df2 %>%
                                         filter(game_id %in% c(id,compare_full)),             
                                 aes(x = variable,
                                     color = name,
                                     y=scale,
                                     label = name),
                                 position = pos,
                                 show.legend=F,
                                 size = 3)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option="magma",
                                      begin = 0.2,
                                      end = 0.8)

        # plot backgrond for pc1 and pc2
        pc_plot_background = df %>%
                ggplot(., aes(x=PC1,
                              label = name,
                              y = PC2))+
                geom_point(alpha = 0.15,
                           col = "grey60")+
                geom_text(check_overlap=T,
                          vjust = 0.5,
                          col = "grey80",
                          size =2)+
                theme_phil()
        
        # pc 1 and 2 plot
        pc_plot = pc_plot_background +
                geom_point(data = df %>%
                                   filter(game_id %in% c(id, compare)),
                           aes(x=PC1,
                               label = name,
                               color = name,
                               y=PC2),
                           size = 3)+
                geom_label_repel(data = df %>%
                                   filter(game_id %in% c(id, compare)),
                           aes(x=PC1,
                               label = name,
                               color = name,
                               y=PC2),
                           size =3,
                           max.overlaps = 15)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option = "magma",
                                      begin = 0.2,
                                      end = 0.8)+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
        
        
        # tile plot
        tile_plot = profile_df %>%
                filter(game_id %in% c(id, compare_full)) %>%
           #     mutate(name = paste(name, game_id, sep="_")) %>%
                mutate(name = factor(abbreviate(name,
                                                minlength=30),
                                     levels = abbreviate(neighbors,
                                                         minlength=30))) %>%
                mutate(scale_round = round(scale, 2)) %>%
                ggplot(., aes(x=variable,
                              y=name,
                              color = scale,
                              fill = scale))+
                geom_tile()+
                theme_phil()+
                theme(legend.title = element_text())+
                scale_color_viridis(option="magma",
                                    limits = c(-5,5),
                                    oob = scales::squish)+
                scale_fill_viridis(option="magma",
                                   limits = c(-5,5),
                                   oob = scales::squish)+
                guides(color = "none",
                       fill = guide_colorbar(barheight=0.5,
                                             barwidth=10,
                                                  title = "Z Score",
                                           #  title = "              Z Score",
                                             title.position = "top"))+
                theme(legend.position = "top")+
                xlab("")+
                ylab("")+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())

        out = list("neighbors_table" = neighbors_table_ft,
                   "neighbors_data" = neighbors_table,
                   "neighbors_plot" = compare_plot,
                   "neighbors_plot_full" = compare_plot2,
                   "tile_plot" = tile_plot,
                   "pc_plot" = pc_plot)
        
        return(out)
        
}
