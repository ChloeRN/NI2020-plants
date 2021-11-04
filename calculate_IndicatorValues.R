# Build data frame with new NI indicator values (GAM predictions per municipality)
#load("oldIndicatorData") # Uncomment to load results from previous steps, if entire script is not run continuously
newIndicatorData <- oldIndicatorData
for(j in 1:length(species))
{
  print(species[j])
  
  # Make data objects of old and new indicator sets for further manipulation
  old <- oldIndicatorData[[j]]$indicatorValues
  new <- newIndicatorData[[j]]$indicatorValues
  
  selected.year <- old$yearName=="Referanseverdi"
  oldref <- old[selected.year,]
  old$ref <- oldref$verdi[match(old$areaName,oldref$areaName)]
  
  newref.poly <- NIGAM_All.list[[j]][[1]]$p
  newref.se.poly <- NIGAM_All.list[[j]][[1]]$p.se
  r <- match(newref.poly$NAVN,oldref$areaName)
  new$verdi <- NA
  new$verdi[selected.year][r[!is.na(r)]] <- newref.poly$layer[!is.na(r)]
  new$verdiSE <- NA
  new$verdiSE[selected.year][r[!is.na(r)]] <- newref.se.poly$layer[!is.na(r)]
  newref <- new[new$yearName=="Referanseverdi",]
  new$ref <- newref$verdi[match(new$areaName,newref$areaName)]
  for(i in 2:(length(year)-1))
  {
    selected.year <- old$yearName==as.character(year[i])
    oldval <- old[selected.year,]
    newval <- NIGAM_All.list[[j]][[i]]$p
    newval.se <- NIGAM_All.list[[j]][[i]]$p.se
    o <- match(newval$NAVN,oldval$areaName)
    new$verdi[selected.year][o[!is.na(o)]] <- newval$layer[!is.na(o)]
    new$verdiSE[selected.year][o[!is.na(o)]] <- newval.se$layer[!is.na(o)]
  }
  def <- old$ref/old$ref # indicator value for definition area (1) or not (NA) - used to remove new predictions outside definition area in next three lines
  new$ref <- new$ref*def
  new$verdi <- new$verdi*def
  new$verdiSE <- new$verdiSE*def
  newIndicatorData[[j]]$indicatorValues <- new
}
