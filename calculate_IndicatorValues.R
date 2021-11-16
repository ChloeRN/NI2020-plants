# Build data frame with new NI indicator values (GAM predictions per municipality)
calculate_IndicatorValues <- function(species, year, save = FALSE){
  
  ## Load saved data if not present
  
  # Old indicator data
  if(!exists("oldIndicatorData")){
    load("oldIndicatorData.RData")
    message('Old indicator data loaded from file.')
  }
  
  # GAM predictions
  if(!exists("NIGAM_All.list")){
    load("Results/NIGAM_All.list.RData")
    message('GAM prediction data loaded from file.')
  }
  
  ## Set up structure for new indicator data from old
  newIndicatorData <- oldIndicatorData
  
  ## Calculate indicator values for each species and year
  message("Calculating indicator values for:")
  
  for(j in 1:length(species))
  {
    message(species[j])
    
    # Make data objects of old and new indicator sets for further manipulation
    old <- oldIndicatorData[[j]]$indicatorValues
    new <- newIndicatorData[[j]]$indicatorValues
    
    # Set reference value
    selected.year <- old$yearName=="Referanseverdi"
    oldref <- old[selected.year,]
    old$ref <- oldref$verdi[match(old$areaName,oldref$areaName)]
    
    # Set up polygon of new GAM predictions and match names to previous
    newref.poly <- NIGAM_All.list[[j]][[1]]$p
    newref.se.poly <- NIGAM_All.list[[j]][[1]]$p.se
    r <- match(newref.poly$NAVN, oldref$areaName)
    
    # Set up reference values
    new$verdi <- NA
    new$verdi[selected.year][r[!is.na(r)]] <- newref.poly$Norway[!is.na(r)]
    new$verdiSE <- NA
    new$verdiSE[selected.year][r[!is.na(r)]] <- newref.se.poly$Norway[!is.na(r)]
    newref <- new[new$yearName=="Referanseverdi",]
    new$ref <- newref$verdi[match(new$areaName,newref$areaName)]
    
    # Calculate scaled indicator value for each year
    for(i in 2:(length(year)-1)){
      selected.year <- old$yearName == as.character(year[i])
      oldval <- old[selected.year,]
      newval <- NIGAM_All.list[[j]][[i]]$p
      newval.se <- NIGAM_All.list[[j]][[i]]$p.se
      o <- match(newval$NAVN, oldval$areaName)
      new$verdi[selected.year][o[!is.na(o)]] <- newval$Norway[!is.na(o)]
      new$verdiSE[selected.year][o[!is.na(o)]] <- newval.se$Norway[!is.na(o)]
    }
    
    # Remove new predictions outside definition area (indicator value for definition area (1) or not (NA) )
    def <- old$ref/old$ref  
    new$ref <- new$ref*def
    new$verdi <- new$verdi*def
    new$verdiSE <- new$verdiSE*def
    
    # Fill new value into indicator data list
    newIndicatorData[[j]]$indicatorValues <- new
  }
  
  ## Save new indicator data (optional)
  if(save){
    save(newIndicatorData, file = "newIndicatorData.RData")
  }
  
  ## Return new indicator data
  return(newIndicatorData)
}
