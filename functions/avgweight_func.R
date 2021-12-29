avgweight_func <-
function(x) {
        
        breaks<-seq(1, 5, 0.1)
        #  breaks = weight_deciles
        colorRamp=colorRampPalette(c("white", "red"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}
