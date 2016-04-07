library("dplyr")
library("magrittr")

rm(list = ls())
#load zip_codes
load("/home/paul/Dokumente/DataFest2015/data and codebook/zip_codes.RData")
ZipCodeDat = data.frame(ZIP = zip_codes, stringsAsFactors = FALSE)

#load election data
dat = read.csv("/home/paul/Dokumente/DataFest2015/external data/ElectionsZipRace.csv", colClasses = "character")

#create a look-up table so I can look up the unmerged ZIP Codes by hand and continue using that table
# LookUpTable = datLookUp %>% select(ZIP, STCD)

#write lookUptable to a file
# write.csv(LookUpTable, "/home/paul/Dokumente/DataFest2015/external data/ZIPDistrictTable2.csv")

#load  lookUpTable 
LookUpTable = read.csv("/home/paul/Dokumente/DataFest2015/external data/ZIPDistrictTable2.csv", colClasses = "character")

#add to data
ZipCodeDat = left_join(ZipCodeDat, LookUpTable, by = "ZIP") %>% na.omit()

#merge Data
datLookUp = left_join(ZipCodeDat, dat, by = "STCD")

#I select the variables from election I am Interested in
#The result is a list of zip codes from the Ticket Master Data with data on republican and democrateic vote share and the racial make up of the city
#clean variables
datLookUp$Population = datLookUp$Population %>% as.character() %>% sub(",", "", .) %>% as.numeric()

#convert votes to numeric
datLookUp$D.Votes = datLookUp$D.Votes %>% as.numeric()
datLookUp$D.VotePrct = datLookUp$D.VotePrct %>% as.numeric()
datLookUp$R.Votes = datLookUp$R.Votes %>% as.numeric()
datLookUp$R.VotePrct = datLookUp$R.VotePrct %>% as.numeric()

#convert race to numeric
datLookUp$White     = datLookUp$White %>% as.numeric()
datLookUp$Black     = datLookUp$Black %>% as.numeric()
datLookUp$Asian     = datLookUp$Asian %>% as.numeric()
datLookUp$Hispanic  = datLookUp$Hispanic %>% as.numeric()


#a function that aggregates the racial make-up of a district weighted by the population
RaceAgg = function(Race, Pop){
  PopRace = Pop*Race
  a = sum(PopRace, na.rm = TRUE)
  b = sum(Pop, na.rm = TRUE)
  a/b
}

#aggregate data at district level
datLookUp = datLookUp %>% filter(!is.na(STCD)) %>% group_by(STCD) %>% summarise(White = RaceAgg(White, Population), Black = RaceAgg(Black, Population), Asian = RaceAgg(Asian, Population), Hispanic = RaceAgg(Hispanic, Population), Population = sum(Population), D.Votes = unique(D.Votes), D.VotePrct = unique(D.VotePrct), R.Votes = unique(R.Votes), R.VotePrct = unique(R.VotePrct), State = unique(State), District = unique(District))

#merge back to the look UP table
ElectionDatTicketMaster = left_join(LookUpTable, datLookUp, by = "STCD")
#drop zip codes that were not merged
ElectionDatTicketMaster = ElectionDatTicketMaster[!is.na(ElectionDatTicketMaster$STCD),]

#save a version of the data set without ZIP Codes for Plotting
MapDat = ElectionDatTicketMaster %>% select(-ZIP, -X) %>% unique()
write.csv(MapDat, "/home/paul/Dokumente/DataFest2015/external data/ElectoralMapDat.csv")

#write dataset to file
write.csv(ElectionDatTicketMaster, "/home/paul/Dokumente/DataFest2015/external data/ElectionDatTicketMaster.csv")
