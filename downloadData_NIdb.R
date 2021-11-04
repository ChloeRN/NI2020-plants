### Set switch for re-download of old indicator data from NI database
retrieveOldData <- FALSE

### Provide user credentials for NI database and request token
myUserName_NIdb <- rstudioapi::askForPassword("NI database username") # = NINA email address
myPassword_NIdb <- rstudioapi::askForPassword("NI database password")

NIcalc::getToken(username = myUserName_NIdb,  
                 password = myPassword_NIdb,
                 url = "https://www8.nina.no/NaturindeksNiCalc")#source('species.r')

### Indicator species names
art <- c("alm","fjellvalmue","greplyng","issoleie","kusymre","myrtelg","olavsstake","prestekrage",
         "purpurlyng","sennegras","solblom","sveltstarr","engmarihand","hvitmyrak","brunmyrak","smalsoldogg","dikesoldogg")
species <- c("Ulmus glabra","Papaver radicatum","Kalmia procumbens","Ranunculus glacialis","Primula vulgaris",
             "Thelypteris palustris","Moneses uniflora","Leucanthemum vulgare","Erica cinerea","Carex vesicaria",
             "Arnica montana","Carex pauciflora","Dactylorhiza incarnata","Rhynchospora alba","Rhynchospora fusca",
             "Drosera anglica","Drosera intermedia")


### Retrieve and store old data from NI database (if necessary)
if(retrieveOldData){
  myIndicators <- NIcalc::getIndicators() 
  myIndicators <- myIndicators[myIndicators$id!=3,]  # removing one indicator which is estimated with other data and methods (3: Alge p? bj?rk)
  myIndicators <- cbind(myIndicators,species=species)
  oldIndicatorData <- list()
  for(i in 1:length(species)){
    oldIndicatorData[[i]] <- NIcalc::getIndicatorValues(indicatorID = myIndicators$id[i]) 
  }
  names(oldIndicatorData) <- myIndicators$name
  save(oldIndicatorData, file = "oldIndicatorData.RData")
}else{
  load('oldIndicatorData.RData')
}
