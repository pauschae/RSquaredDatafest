library("choroplethr")
library("choroplethrMaps")
library("choroplethrZip")

#read in data
Votes = read.csv("/home/paul/Dokumente/DataFest2015/external data/ElectionDatTicketMaster.csv")

#function that adds leading zeros
addZeros = function(char){
  ifelse(nchar(char) == 4, paste0("0", char), char)
}

#prepare for plotting
Votes$region = Votes$ZIP %>% addZeros()
Votes$value  = Votes$R.VotePrct
#drop duplicates
PlotVotes = Votes %>% select(region, value) %>% unique() 
#plot
zip_choropleth(head(PlotVotes))


