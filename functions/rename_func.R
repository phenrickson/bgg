rename_func <-
function(x) {
        
        x<-gsub("cat_memory", "cat_memory_game", x)
        x<-gsub("cat_spiessecret_agents", "cat_spies_secret_agents", x)
        x<-gsub("cat_deduction", "cat_deduction_game", x)
        x<-gsub("cat_novelbased", "cat_novel_based", x)
        x<-gsub("cat_","", x)
        x<-gsub("mech_","", x)
        x<-gsub("pub_","", x)
        x<-gsub("des_","", x)
        x<-gsub("avgweight", "Average Weight", x)
        x<-gsub("yearpublished", "Year Published", x)
        x<-gsub("minage", "Min Age", x)
        x<-gsub("playingtime", "Playing Time", x)
        x<-gsub("maxplayers", "Max Players", x)
        x<-gsub("minplayers", "Min Players", x)
        x<-gsub("_", " ", x)
        
        x = str_to_title(x)
        x = gsub("World War Ii", "World War II", x)
        x = gsub("Gmt", "GMT", x)
        x = gsub("Cmon", "CMON", x)
        x = gsub("Zman", "ZMan", x)
        x = gsub("Movies Tv", "Movies TV", x)
        x = gsub("Auctionbidding", "Auction Bidding", x)
        x = gsub("Postnapoleonic", "Post Napoleonic", x)
        x
        
}
