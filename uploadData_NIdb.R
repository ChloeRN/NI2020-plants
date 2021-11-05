# Write updated indicator data to NI database
# (unitOfMeasurement needs to be updated manually. New units: "Funnsannsynlighet i 1x1km-ruter")
# Get token for writing to NI database if script is not run continuously (see above).
# mode argument: "overwrite" writes to database (irreversible), "compare" compares values in database with updated values
uploadData_NIdb <- function(species, mode){
  
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
   for(j in 1:length(species)){
     print(species[j])
     NIcalc::writeIndicatorValues(updatedIndicatorData[[j]])
   }
  }
  
  ## Compare updated indicator data to data currently stored in NI database
  for(j in 1:length(species)){
    print(species[j])
    d1 <- updatedIndicatorData[[j]]$indicatorValues
    indicatorData <- NIcalc::getIndicatorValues(indicatorID = myIndicators$id[myIndicators$species==species[j]]) 
    d2 <- indicatorData$indicatorValues
    print(head(d1))
    print(head(d2))
    check_all <- data.frame(d1$verdi,d2$verdi)
    check_all$check <- check_all[,1]/check_all[,2]
    print("Check all = 1")
    print(summary(check_all$check))
  }
}