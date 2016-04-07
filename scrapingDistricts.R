library("rvest")
library("magrittr")
library("stringr")

#function that selects the first element from each vector in a list
getFirst = function(list){
  lapply(list, function(x) x[1]) %>% unlist()
}


#a function which gets information on the electoral district from zip codes

getDistrictInfo = function(zip){
  #build url
  url = c("http://ziplook.house.gov/htbin/findrep?ZIP=", zip,"&Submit=FIND+YOUR+REP+BY+ZIP") %>% paste0(., collapse = "")
  page = html(url)
  page %>% html_nodes(xpath = "//div[@id = 'PossibleReps']")
              
              
              
              
              [[1]] %>% html_text() %>% str_split(., "is located in the"))[[1]][2] %>% str_trim()
  return(district)
}


