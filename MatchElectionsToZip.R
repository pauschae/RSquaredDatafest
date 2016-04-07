library("dplyr")

#clean workspace
rm(list = ls())

#load elections
votes = read.csv("/home/paul/Dokumente/DataFest2015/external data/cleanElectionData.csv")

#load census
census = read.csv("/home/paul/Dokumente/DataFest2015/external data/cdzip2010.csv")

#create STCD in vote data for matching
#function that adds leading zero
addZero = function(char){
  char  = as.character(char)
  ifelse(nchar(char) == 1, paste("0", char, sep = ""), char)
}

#create the same function for four letter zip codes
addZeroZip = function(char){
  char  = as.character(char)
  ifelse(nchar(char) == 4, paste("0", char, sep = ""), char)
}

votes = votes %>% mutate(District =  sapply(votes$District, addZero))
votes = votes %>% mutate(STCD = paste(StateAbbr, District, sep = ""))

#add leading zeros to zip codes
census$ZIP = addZeroZip(census$ZIP)

#join datasets
dat = left_join(votes, census)

#write dataset 
write.csv(dat, "/home/paul/Dokumente/DataFest2015/external data/ElectionsZipRace.csv")
