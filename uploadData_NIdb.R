# Write updated indicator data to NI database
# (unitOfMeasurement needs to be updated manually. New units: "Funnsannsynlighet i 1x1km-ruter")
# Get token for writing to NI database if script is not run continuously (see above).
# mode argument: "overwrite" writes to database (irreversible), "compare" compares values in database with updated values
uploadData_NIdb <- function(species, speciesList, mode){
  
  if(!(mode%in%c("overwrite", "compare"))){
    stop("Incorrect mode specification. See documentation for supported modes.")
  }
  
  ### Provide user credentials for NI database and request token
  myUserName_NIdb <- rstudioapi::askForPassword("NI database username") # = NINA email address
  myPassword_NIdb <- rstudioapi::askForPassword("NI database password")
  
  NIcalc::getToken(username = myUserName_NIdb,  
                   password = myPassword_NIdb,
                   url = "https://www8.nina.no/NaturindeksNiCalc")#source('species.r')
  
  ## Write updated indicator data into NI database
  if(mode == "overwrite"){
    
    # Ask the user for confirmation to write to database
    command <- askYesNo("Do you want to write to the database?", default = FALSE)
    
    ## Write to database if confirmed (halt execution otherwise)
    if(!is.na(command) & command){
      
      message("Uploading new indicator data to NI database:")
      
      for(j in 1:length(species)){
        message(species[j])
        NIcalc::writeIndicatorValues(updatedIndicatorData[[j]])
      }  
    }else{
      message("Function halted.")
    }
  }
  
  ## Compare updated indicator data to data currently stored in NI database
  if(mode == "compare"){
    
    # Specify indicators for which to compare data
    myIndicators <- NIcalc::getIndicators() %>%
      dplyr::right_join(data.frame(name = indicators, species = speciesList), by = 'name')
    
    # Compare values for each species
    message("Comparing updated to previous indicator data.")
    
    for(j in 1:length(species)){
      message("")
      message(paste0("Change in indicator values for ", species[j], ":"))
      
      d1 <- updatedIndicatorData[[j]]$indicatorValues
      indicatorData <- NIcalc::getIndicatorValues(indicatorID = myIndicators$id[myIndicators$species==species[j]]) 
      d2 <- indicatorData$indicatorValues
      check_all <- data.frame(d1$verdi, d2$verdi)
      check_all$check <- check_all[,2]/check_all[,1]
      print(summary(check_all$check))
    }
  }
}
