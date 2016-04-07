library("magrittr")
setwd()
#read in data
dat    = read.csv("/home/paul/Dokumente/DataFest2015/data and codebook/approved_data_purchase-v5.csv")

Artists = dat$primary_act_name %>% unique()

#save list of artists to a file
write.csv(Artists, "/home/paul/Dokumente/DataFest2015/data and codebook/Artists")


rm(list = ls())
