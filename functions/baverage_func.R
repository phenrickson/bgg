baverage_func <-
function(x) {
        
        breaks = seq(6, 8.6, 0.1)
        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}
