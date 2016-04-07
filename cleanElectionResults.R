library("dplyr")
library("tidyr")
library("stringr")

#functions
getFirst = function(list){
  lapply(list, function(x) x[1])
}

#read in FEC Data
dat = read.csv("/home/paul/Dokumente/DataFest2015/external data/results14.csv")

#isolate all variables that we need
dat = dat[!(dat$CANDIDATE.NAME %in% c("", "Scattered"))&(dat$GENERAL.VOTES != ""), ] %>% select(PARTY, STATE.ABBREVIATION, STATE, D, GENERAL.VOTES, GENERAL..) 

#rename variable
names(dat) = c("Party", "StateAbbr", "State", "District", "Votes", "VotePrct")

#filter out republicans and democrats
dat = dat %>% filter(Party %in% c("R", "D"))

#In some districts there are multiple candidates of the same party
dat = dat %>% group_by(Party, StateAbbr, State, District) %>% mutate(VotePrct = VotePrct %>% sub("%", "", .) %>% sub(",", ".", .) %>% as.numeric()) %>% summarise(Votes = sum(as.numeric(Votes)), VotePrct = sum(as.numeric(VotePrct)))

#In district with elections during the term I take the mean of the available data
dat = dat %>% group_by(Party, StateAbbr, State, District) %>% summarise(Votes = sum(Votes), VotePrct = sum(VotePrct)) %>% ungroup() %>% mutate(District = (District %>% str_split(., "-") %>% getFirst() %>% str_trim())) %>% group_by(Party, StateAbbr, State, District) %>% summarise(Votes = mean(Votes, na.rm = TRUE), VotePrct = mean(VotePrct, na.rm = TRUE))

#spread the data so that an election district is an observation
dat = dat %>% unite(Votes_VotePrct, Votes, VotePrct) %>% unique() %>% spread(.,  key = Party, value = Votes_VotePrct) %>% separate(D, c("D.Votes", "D.VotePrct"), sep = "_", extra = "drop") %>% separate(R, c("R.Votes", "R.VotePrct"), sep = "_", extra = "drop")

#save cleaned data
write.csv(dat, "/home/paul/Dokumente/DataFest2015/external data/cleanElectionData.csv")

