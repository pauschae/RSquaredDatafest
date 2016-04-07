library("rvest")
library("magrittr")
library("stringr")

#function that fills empty variables with Na
setEmptyNA = function(var){
  ifelse(length(var) == 0, NA, var)
}

#a function that extracts data from one district
getDistrictDat = function(district){
  districId    = district %>% html_attr("id") %>% setEmptyNA
  Prct = district %>% html_nodes(xpath = "./div[@class='results-dataset']/div/div/table/tbody/tr/td[@class = 'results-percentage']//span[@class = 'number']") %>% html_text() %>% setEmptyNA
  Party = district %>% html_nodes(xpath = "./div[@class='results-dataset']/div/div/table/tbody/tr") %>% html_attr("class") %>% setEmptyNA
  
  data.frame(districId, Party, Prct) 
}

#a function that gets all the election data for a given state
getStateDat = function(url, state){
  page = html(url)
  districts      = page %>% html_nodes(xpath = "//article[@class = 'results-group']")
  out = getDistrictDat(districts[1])
  if(length(districts)>1){
    for(i in 2:length(districts)){
      out = rbind(out, getDistrictDat(districts[i]))
    }
  }
  out$State = state
  return(out)
}

#get state names
urls = html("http://www.politico.com/2014-election/results/map/house") %>% html_nodes(xpath = "//h3/a") %>% html_attr("href")
state = urls %>% sub("http://www.politico.com/2014-election/results/map/house/", "", .) %>% sub("/", "", .)

#loop through states for final data set
data = getStateDat(urls[1], state = state[1])
for(i in 2:length(urls)){
  data = rbind(data, getStateDat(urls[i], state = state[i]))
}

#save data
write.csv(data, file = "/home/paul/Dokumente/DataFest2015/data/ElectionresultsHouse2015.csv")

