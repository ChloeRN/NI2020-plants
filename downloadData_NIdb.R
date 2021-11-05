downloadData_NIdb <- function(species, indicators, save = FALSE){

  ## Provide user credentials for NI database and request token
  UserName_NIdb <- rstudioapi::askForPassword("NI database username") # = NINA email address
  Password_NIdb <- rstudioapi::askForPassword("NI database password")

  ## Get token
  NIcalc::getToken(username = UserName_NIdb,  
                   password = Password_NIdb,
                   url = "https://www8.nina.no/NaturindeksNiCalc")
  
  ## Specify indicators for which to download data
  myIndicators <- NIcalc::getIndicators() %>%
    dplyr::right_join(data.frame(name = indicators, species = species), by = 'name')
    
  ## Retrieve old indicator data from NI database
  oldIndicatorData <- list()
  
  for(i in 1:length(species)){
    oldIndicatorData[[i]] <- NIcalc::getIndicatorValues(indicatorID = myIndicators$id[i]) 
  }
  names(oldIndicatorData) <- myIndicators$name
  
  ## Save indicator data (optional)
  if(save){
    save(oldIndicatorData, file = "oldIndicatorData.RData")
  }

  ## Return indicator data
  return(oldIndicatorData)
}
