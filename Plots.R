library("ggplot2")
library("tidyr")
library("magrittr")
library("dplyr")

#clear workspace
rm(list=ls())

#load data
load("/home/paul/Dokumente/DataFest2015/DataForAnalysis.RData")
dat = datAggSTCD
rm(datAggSTCD)

#convert variables to numeric
dat$D.VotePrct = dat$D.VotePrct %>% as.character() %>% as.numeric() 

#melt data for plotting
head(dat)
dat = dat %>% gather(., key = Genre, value=SalesPrct, Metal:ELectronic) 

#calculate moving averages
ma = function(v, n){
  out = c()
  for(i in 1:length(v)) {
    if((i <= n)|(i >= length(v)-n)) {
      out = c(out, NA)
    } else {
      out = c(out, mean(v[(i-n):(i+n)], na.rm = TRUE))
    }
  }
  return(out)
}

dat = dat %>% group_by(Genre) %>% arrange(SalesPrct) %>% mutate(D.VotePrctMA = ma(D.VotePrct, 10)) 


#calculate means
CountryMean = mean(dat$SalesPrct[dat$Genre == "Country"])
UrbanMean = mean(dat$SalesPrct[dat$Genre == "AfricanAmerican"])
dat$SalesPrct[dat$Genre == "Country"]         =dat$SalesPrct[dat$Genre == "Country"]   -CountryMean
dat$SalesPrct[dat$Genre == "AfricanAmerican"] =dat$SalesPrct[dat$Genre == "AfricanAmerican"]  -UrbanMean

#plot
ggplot(dat[dat$Genre %in% c("Country", "AfricanAmerican"),], aes(SalesPrct, D.VotePrctMA/100, group = Genre, color = Genre))+geom_line()+geom_vline(xintercept = 0)+theme_classic()+scale_color_manual(values =  c("#9c0824", "#26456E"), labels = c("Country Music", "Urban Music"))+ylab("Stimmenanteil der Demokraten")+xlab("Anteil an den verkauften Tickets (Abweichung vom Durchschnitt)")+ theme(legend.title=element_blank()) + geom_vline(xintercept = -0.163746) + geom_hline(yintercept = 0.32)+xlim(c(-0.2,0.5))+coord_fixed(ratio = 1)


ggplot(dat[dat$Genre %in% c("Country", "AfricanAmerican"),], aes(SalesPrct, D.VotePrct/100, group = Genre, color = Genre))+geom_line()+geom_vline(xintercept = 0)+theme_classic()+scale_color_manual(values =  c("#9c0824", "#26456E"), labels = c("Country Music", "Urban Music"))+ylab("Stimmenanteil der Demokraten")+xlab("Anteil an den verkauften Tickets (Abweichung vom Durchschnitt)")+theme(legend.title=element_blank())+geom_vline(xintercept = -0.163746) + geom_hline(yintercept = 0.32)+xlim(c(-0.2,0.5))+coord_fixed(ratio = 1)
 