# Create distribution objects and format data for import to NI base (based on code from NIcalc vignette)
create_DistObjects <- function(species, save){
  
  ## Load saved data if not present
  
  # Old indicator data
  if(!exists(oldIndicatorData)){
    load("oldIndicatorData")
    message('Old indicator data loaded from file.')
  }
  
  # New indicator data
  if(!exists(newIndicatorData)){
    load("newIndicatorData")
    message('New indicator data loaded from file.')
  }
  
  ## Set up structure for updated indicator data
  updatedIndicatorData <- oldIndicatorData
  
  ## Create distribution objects for each species
  for(j in 1:length(species)){
    
    print(species[j])
    
    d <- newIndicatorData[[j]]$indicatorValues
    myData <- data.frame(estimatedStates = d$verdi,
                         standardErrors = d$verdiSE)
    logNormalParams <- NIcalc::normal2Lognormal(mean = myData$estimatedStates, 
                                                sd = myData$standardErrors)
    myData$muLogNormal <- logNormalParams$mean 
    myData$sigmaLogNormal <- logNormalParams$sd
    
    ddd <- NULL
    for (i in 1:dim(myData)[[1]])
    {
      if(any(is.na(myData[i,c("muLogNormal","sigmaLogNormal")]))) {ddd[i] <- NA; next}
      ddd[i] <- list(NIcalc::makeDistribution(
        input = "logNormal",
        distParams = list(mean = myData$muLogNormal[i],
                          sd = myData$sigmaLogNormal[i]))) 
    }
    myData$distrObjects <- ddd
    myData$areaIDs <- d$areaId
    myData$years <- d$yearName
    myData$Datatype <- d$datatypeId
    
    rowsWithNAs <- which(is.na(myData$estimatedStates))
    rowsWithoutNAs <- which(!is.na(myData$estimatedStates))
    if(any(rowsWithNAs)){
      
      for(i in rowsWithNAs){
        updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]],
                                                                areaId = myData$areaIDs[i],
                                                                years = myData$years[i],
                                                                est = myData$estimatedStates[i],
                                                                lower = myData$estimatedStates[i]-myData$standardErrors[i],
                                                                upper = myData$estimatedStates[i]+myData$standardErrors[i])
      }
    }
    for(i in rowsWithoutNAs){

      updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]], 
                                                              areaId = myData$areaIDs[i], 
                                                              years = myData$years[i], 
                                                              distribution = myData$distrObjects[[i]],
                                                              datatype = myData$Datatype[i])
    } 
  }
  
  ## Save updated indicator data (optional)
  if(save){
    save(updatedIndicatorData, file = "updatedIndicatorData.RData")
  }
  
  ## Return updated indicator data
  return(updatedIndicatorData)
}
