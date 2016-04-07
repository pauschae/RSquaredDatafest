library("dplyr")
library("tidyr")

#clean workspace
rm(list= ls())

#load ticketmaster data
load("/home/paul/Dokumente/DataFest2015/data and codebook/concerts.RData")

datAgg = concerts %>% group_by(venue_postal_cd_sgmt_1, minor_cat_name) %>% summarise(Sales = sum(tickets_purchased_qty)) 

#load district data
ElectionDatTicketMaster = read.csv("/home/paul/Dokumente/DataFest2015/external data/ElectionDatTicketMaster.csv", colClasses = "character")

#join electoral districts
datAgg$ZIP = datAgg$venue_postal_cd_sgmt_1
datAgg$venue_postal_cd_sgmt_1 = NULL

#actual join
datAggJoin = left_join(datAgg, ElectionDatTicketMaster, by = "ZIP") 

#aggregate over electoral districts
datAggSTCD = datAggJoin %>% group_by(STCD, minor_cat_name) %>% summarise(Sales = sum(Sales)) %>% group_by(STCD) %>% mutate(SalesPrct = Sales / sum(Sales), totalSales = sum(Sales)) %>% select(-Sales)

#rename
datAggSTCD = datAggSTCD %>% rename(Genre = minor_cat_name)

#go from long to wide form
datAggSTCD = datAggSTCD %>% spread(data = ., key = Genre, value = SalesPrct)

#convert NAs to zeros
NAtoZero = function(col){
  col[is.na(col)] = 0
  return(col)
}

datAggSTCD = datAggSTCD %>% mutate_each(., funs(NAtoZero))

#pre-aggregation
datAggSTCD = datAggSTCD %>% transmute(STCD = STCD, totalSales = totalSales, Metal = `HEAVY METAL`, Latin = `LATIN MUSIC` + TROVA, Folk = FOLK + `ETHNIC/FOREIGN` + `CHANSON FRANCAISE`, Country = `BLUEGRASS` + COUNTRY + OLDIES, Pop = POP +`ADULT CONTEMPORARY`+`BALLADS/ROMANTIC` + `ROCK/POP`+`TRIBUTE BAND`, Classical = `CLASSICAL/VOCAL`, BluesJazz = BLUES + JAZZ + FUNK, AfricanAmerican =  GOSPEL+`RAP/URBAN`+`R & B`+REGGAE+REGGAETON+SOUL, Rock = PUNK+`ALTERNATIVE ROCK`+FESTIVALS, Comedy = COMEDY, ELectronic = `DANCE MUSIC/DANCE`+`DRUM & BASS/DUBSTEP/GRIME CLUB-MFX ONLY`)

#read in election data
election = read.csv2("/home/paul/Dokumente/DataFest2015/external data/ElectoralMapDat2.csv")

#add election data
datAggSTCD = left_join(datAggSTCD, election)
datAggSTCD = datAggSTCD[!is.na(datAggSTCD$District),]

#write dataset
save(datAggSTCD, file = "/home/paul/Dokumente/DataFest2015/DataForAnalysis.RData")

