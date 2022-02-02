pivot_and_dummy_types = function(input_data, input_type) {
        
        # pivoting
        input_data %>%
                filter(type == input_type) %>%
                mutate(type_abbrev = substr(type, 1, 3)) %>%
                mutate(value = tolower(gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", value))))) %>%
                select(game_id, type, value) %>%
                mutate(type = paste(type, value, sep="_")) %>%
                mutate(has_type = 1) %>%
                select(-value) %>%
                pivot_wider(names_from = c("type"),
                            values_from = c("has_type"),
                            id_cols = c("game_id"),
                            names_sep = "_",
                            values_fn = min,
                            values_fill = 0)
        
}