
build_TrainingData <- function(GBIF_data, min.year, max.year, year.interval, save.unfiltered = FALSE){
  
  # SUBSETTING AND COORDINATE REFORMATTING #
  #----------------------------------------#
  
  ## Select relevant fields
  # Only species, geographical coordinates (with uncertainty/precision) and time (year, month, day) are
  # needed for the modelling, but other fields may be useful for error checking etc.
  selectedFields <- c( "institutionID", "collectionID", "catalogNumber",
                       "basisOfRecord", "contributor",
                       "species", "scientificName", "taxonID", "taxonKey",
                       "year", "month", "day",
                       "countryCode", "county", "municipality",
                       "decimalLongitude", "decimalLatitude", 
                       "coordinateUncertaintyInMeters", "coordinatePrecision") 
  
  GBIF_data <- GBIF_data[, selectedFields]
  
  ## Remove observations with missing dates and/or coordinates 
  # NOTE: shouldn't be necessary when "has coordinate" = TRUE, but quite a few long and lat are missing
  GBIF_data <- GBIF_data[complete.cases(GBIF_data[, c("year", "month", "day", "decimalLongitude", "decimalLatitude")]), ]
  
  ## Convert lat-long coordinates to coordinate system of Norway raster
  occ_points <- data.frame(x = GBIF_data$decimalLongitude, y = GBIF_data$decimalLatitude)
  occ_points <- SpatialPoints(occ_points, proj4string = CRS("+proj=longlat +datum=WGS84"))
  occ_UTM33 <- spTransform(occ_points, CRS("+proj=utm +zone=33 ellps=GRS80 +units=m"))
  GBIF_data$x <- occ_UTM33$x
  GBIF_data$y <- occ_UTM33$y
  
  ## Create a spatial points data frame, and add unique date 
  # NOTE: better than eventDate, which may be misleading if occurrences with month only are recorded on the first day
  occ_UTM33 <- SpatialPointsDataFrame(data.frame(x = GBIF_data$x, y = GBIF_data$y), data = data.frame(GBIF_data))
  occ_UTM33$year_month_day <- paste(occ_UTM33$year,occ_UTM33$month, occ_UTM33$day)
  
  ## Removing no longer required objects to lessen memory load
  rm(GBIF_data, occ_points)
  
  
  # RASTERISATION OF OCCURRENCE DATA #
  #----------------------------------#
  
  ## Define start years for all time intervals
  start.year <- seq(min.year, max.year-year.interval, by = year.interval) 
  
  ## Load background raster for Norway
  # NOTE: All values = 1
  norway <- raster("Data/Raster/Norway.tif")  
  
  ## Rasterize sampling effort in time intervals
  samp_ras <- stack(norway)
  
  for(i in 1:length(start.year)){
    print(start.year[i])
    records.in.interval <- (occ_UTM33$year >= start.year[i]) & (occ_UTM33$year < start.year[i]+5)
    print(table(records.in.interval))
    if(!any(records.in.interval)) samp_ras[[i]] <- norway*0
    else samp_ras[[i]] <- norway*rasterize(occ_UTM33[records.in.interval,], norway, field="year_month_day", fun=function(x,...){length(unique(x))}, background=0) # raster with counts of sampling effort in each cell of norway
  }
  names(samp_ras) <- paste("t", start.year, sep = ".")
  
  ## Rasterize species records in time intervals
  occ_ras_list <- list()

  for(j in 1:length(species)){
    key <- name_suggest(q = species[j], rank = 'species')$data$key
    occ_species <- occ_UTM33[occ_UTM33$taxonKey%in%key,]
    cat(species[j], nrow(occ_species),"\n")
    occ_ras_list[[j]] <- stack(norway)
    for(i in 1:length(start.year)){
      records.in.interval <- (occ_species$year >= start.year[i]) & (occ_species$year < start.year[i] + year.interval)
      cat(start.year[i], sum(records.in.interval), "\n")
      if(!any(records.in.interval)) occ_ras_list[[j]][[i]] <- norway*0
      else occ_ras_list[[j]][[i]] <- norway*rasterize(occ_species[records.in.interval,], norway, field = "year_month_day", fun=function(x,...){length(unique(x))}, background = 0) # raster with counts of occurrences in each cell of norway
    }
    names(occ_ras_list[[j]]) <- paste("t", start.year, sep=".")
  }
  names(occ_ras_list) <- species
  
  ## Save unfiltered data (optional)
  if(save.unfiltered){
    save(samp_ras, file = "Data/Raster/samp_ras_all.RData")  
    save(occ_ras_list, file = "Data/Raster/occ_ras_list_all.RData")
  }
  
  # Removing no longer required objects
  rm(occ_species, occ_UTM33)
  
  
  # ASSEMBLY OF TRAINING DATASETS FOR EACH SPECIES #
  #------------------------------------------------#
  
  ## Build training data sets for distribution modelling 
  ## using  all data - no filter on precision - for all times with continuous sampling (1820 onwards)

  year <- seq(min.year, max.year-year.interval, by = year.interval)
  yr <- paste("t.", year, sep = "")
  for(j in 1:length(species)){
    
    cat(species[j],"\n")
    
    for(k in 1:length(year)){
      
      cat(year[k],"\n")
      
      # Extract occurrence and sampling rasters
      o_ras <- occ_ras_list[[j]][[yr[k]]]
      s_ras <- samp_ras[[yr[k]]]
      
      # Take the occurrence cells as presences
      presences <- which(values(o_ras)>0)
      
      # Take cells with sampling events of some species
      # but without occurrence observations of this particular species
      absences <- which((values(s_ras)>0) & (values(o_ras)==0))
      
      # # Sample absences if too many?
      # absences_sample <- sample(absences,size=length(presences)) # sample of same number of absence cells as presence cells
      
      # Combine presences, absences and environmental data
      selected <- c(presences,absences)
      #selected <- c(presences,absences_sample)
      xy <- raster::coordinates(o_ras)[selected, ]
      data <- data.frame(xy, Y = values(o_ras)[selected], logS = log(values(s_ras)[selected]),
                         year = rep(year[k], nrow(xy)))
      # presence <- as.numeric(data$Y>0)
      # data <- cbind(presence,data)   # MIAmaxent wants presence as the first column
      
      if(k==1) training_data <- data
      else training_data <- rbind(training_data, data)
    }
    # # Convert discrete environmental predictors to factor variables
    # training_data$ar50artype <- factor(training_data$ar50artype)
    # training_data$geonorge123 <- factor(training_data$geonorge123)
    
    # Save training data
    save(training_data, file = paste("Data/Regression data/", species[j], "_training_data_all.RData", sep = ""))
  }
  
}
