usersrated_col_func <-
        function(x) {
                breaks = c(0,
                           100,
                           500,
                           1000,
                           1500,
                           2000,
                           5000,
                           10000,
                           25000,
                           50000)
                
                colorRamp=colorRampPalette(c("white", "deepskyblue1"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=F,
                             label = FALSE)
                col_palette[mycut]
                
        }
