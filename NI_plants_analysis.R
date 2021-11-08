# Appendix 1: R code for indicator estimation
# Script for estimation of indicator values and references states for 17 vascular plants
# for the Nature Index 2020

######################################
# Tools
######################################

# NIcalc package for reading/writing to NI database
#devtools::install_github("NINAnor/NIcalc", build_vignettes = T)
library(NIcalc)

# Other libraries needed:
# rgbif, rgdal, sp, raster, rio, mgcv
library(rgbif)
library(rgdal)
library(sp)
library(raster)
library(rio)
library(mgcv)

#################################################
# Data download and preparation
#################################################

### Set switch for re-download of old indicator data from NI database
retrieveOldData <- FALSE

### Provide user credentials for NI database and request token
myUserName_NIdb <- rstudioapi::askForPassword("NI database username") # = NINA email address
myPassword_NIdb <- rstudioapi::askForPassword("NI database password")

NIcalc::getToken(username = myUserName_NIdb,  
                 password = myPassword_NIdb,
                 url = "https://www8.nina.no/NaturindeksNiCalc")#source('species.r')

### Indicator species names
indicators <- c("Alm","Fjellvalmue","Greplyng","Issoleie","Kusymre","Myrtelg","Olavsstake","Prestekrage",
         "Purpurlyng","Sennegras","Solblom","Sveltstarr","Engmarihand","Hvitmyrak","Brunmyrak","Smalsoldogg","Dikesoldogg")
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


### Download occurrence data from GBIF
# This part is based on the extensively commented instructions for
# asynchronous downloading of GBIF data by Anders Finstad, available here:
# https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
# This allows downloading larger data sets, and citation of the download with a single doi. 

# Set switch for (re-)download of GBIF data
downloadGBIFData <- FALSE

# Set path for raw GBIF data
#path <- '/data/P-Prosjekter/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data' # Server
path <- 'P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data' # Local

# Download all vascular plants (if necessary)
if(downloadGBIFData){
  
  # Set up a user profile at GBIF (https://www.gbif.org), then
  # run this once to store your GBIF user credentials to your R session 
  options(gbif_user = rstudioapi::askForPassword("my gbif username"))
  options(gbif_email = rstudioapi::askForPassword("my registred gbif e-mail"))
  options(gbif_pwd = rstudioapi::askForPassword("my gbif password"))
  
  # Find a taxonkey - get list of gbif keys to filter download
  key <- name_suggest(q = 'Plantae', rank = 'kingdom')$data$key[1] 
  
  # Get download key for all occurrences of plants with coordinates in Norway
  download_key <- 
    occ_download(
      pred('taxonKey', key),
      pred('hasCoordinate', 'TRUE'),
      pred('country', 'NO'),
      type = 'and'
    ) %>% 
    occ_download_meta
  # NOTE: This takes a while to run, and a notification email will be sent once
  # the data is ready for downloading (progress can be checked on GBIF profile site)
  
  # Download data
  occ_download_get(key = download_key$key, path = path)
  
  # Citation - copy into documentation
  paste("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())
  
  # Open data and extract into data frame
  # Get a list of the files within the archive by using "list=TRUE" in the unzip function
  download_path <- paste(path,"/",download_key$key,".zip",sep = "")
  archive_files <- unzip(download_path, files = "NULL", list = T) 
  archive_files
  
  # Get the occurrence.txt file in as a dataframe (using import from rio)
  #sp <- import(unzip(download_path, files = "occurrence.txt"), header = T, sep = "\t")
  sp <- import(unzip(download_path, files = "occurrence.txt"), header = T, sep = "\t", exdir = path)
  
}else{
  # Data import from downloaded occurrence file
  sp <- import(paste0(path, "/occurrence.txt"), header = T, sep = "\t")
}

dim(sp)
head(sp)

# Select fields
# Only species, geographical coordinates (with uncertainty/precision) and time (year, month, day) are
# needed for the modelling, but other fields may be useful for error checking etc.
selectedFields <- c( "institutionID", "collectionID", "catalogNumber",
                     "basisOfRecord", "contributor",
                     "species", "scientificName", "taxonID", "taxonKey",
                     "year", "month", "day",
                     "countryCode", "county", "municipality",
                     "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", "coordinatePrecision") 
sp <- sp[, selectedFields]
dim(sp)

# Remove obs with missing dates and/or coordinates (shouldn't be necessary when "has coordinate" = TRUE, but quite a few long and lat are missing...)
sp <- sp[complete.cases(sp[, c("year", "month", "day", "decimalLongitude", "decimalLatitude")]), ]
dim(sp)

# Convert lat-long coordinates to coordinate system of Norway raster
occ_points <- data.frame(x = sp$decimalLongitude, y = sp$decimalLatitude)
occ_points <- SpatialPoints(occ_points, proj4string = CRS("+proj=longlat +datum=WGS84"))
occ_UTM33 <- spTransform(occ_points, CRS("+proj=utm +zone=33 ellps=GRS80 +units=m"))
sp$x <- occ_UTM33$x
sp$y <- occ_UTM33$y

# Creating a spatial points data frame, and adding unique date (better than eventDate, which may be misleading if occurrences with month only are recorded on the first day)
#load("occurrences cleaned") # for high-precision data
occ_UTM33 <- SpatialPointsDataFrame(data.frame(x = sp$x, y = sp$y), data = data.frame(sp))
occ_UTM33$year_month_day <- paste(occ_UTM33$year,occ_UTM33$month, occ_UTM33$day)

# Removing no longer required objects
rm(sp, occ_points)

### Rasterize occurrence data

# Set time intervals
min.year <- 1820
max.year <- 2020
year.interval <- 5

start.year <- seq(min.year, max.year-year.interval, by = year.interval) # 5-year intervals from 1820 to 2020

# Rasterize sampling effort in time intervals
norway <- raster("Data/Raster/Norway.tif")  # Background raster of Norway (all values = 1)
samp_ras <- stack(norway)
for(i in 1:length(start.year)){
  print(start.year[i])
  records.in.interval <- (occ_UTM33$year >= start.year[i]) & (occ_UTM33$year < start.year[i]+5)
  print(table(records.in.interval))
  if(!any(records.in.interval)) samp_ras[[i]] <- norway*0
  else samp_ras[[i]] <- norway*rasterize(occ_UTM33[records.in.interval,], norway, field="year_month_day", fun=function(x,...){length(unique(x))}, background=0) # raster with counts of sampling effort in each cell of norway
}
names(samp_ras) <- paste("t", start.year, sep = ".")
save(samp_ras, file = "Data/Raster/samp_ras_all.RData")  # unfiltered data

# Rasterize species records in time intervals
#source("species.r") - NOTE: not available
occ_ras_list <- list()
#occ_species_list <- list()
for(j in 1:length(species)){
  key <- name_suggest(q = species[j], rank = 'species')$data$key
  occ_species <- occ_UTM33[occ_UTM33$taxonKey%in%key,]
  cat(species[j], nrow(occ_species),"\n")
  #  occ_species_list[[j]] <- occ_species
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
save(occ_ras_list, file = "Data/Raster/occ_ras_list_all.RData") # unfiltered data

# Removing no longer required objects
rm(occ_species, occ_UTM33)

# Build training data sets for distribution modelling with
# all data (no filter on precision), for all times with continuous sampling (1820 onwards)
load("Data/Raster/occ_ras_list_all.RData")  # unfiltered data
load("Data/Raster/samp_ras_all.RData")      # unfiltered data
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

# Clear workspace
rm(list = setdiff(ls(), 'species'))

################################################
# Modelling
################################################
# GAM spacetime model with geographic coordinates (x and y in meters), year (5-year intervals) and interactions (x:year and y:year).
# Sampling effort as offset. No environmental covariates.

# GAM: all data (no spatial precision filtering) and smoothing parameter gamma=3
gam.results <- list()
for(j in 1:length(species)){
  
  cat(species[j],"\n")
  
  # Training data for species from GBIF, see NI 2020 plants dataprep.r
  load(paste("Data/Regression data/", species[j], "_training_data_all.RData", sep=""))
  d <- training_data[, c("Y", "x", "y", "year", "logS")]
  m <- gam(Y ~ ti(x) + ti(y) + ti(year) + ti(x, year) + ti(y, year),
           data = d, gamma = 3, family = poisson, offset = logS, select = TRUE)
  gam.results[[j]] <- m
}

save(gam.results, file = "Results/gam.results.all.gamma3.RData")

# Clear workspace
rm(list = setdiff(ls(), 'species'))

################################################
# Prediction: NI indicator value estimation
################################################

# GAM predictions: municipality maps for specific NI years
kommune.poly <- readOGR(dsn = "Data/Shapefiles", layer = "Norway municipalities", encoding = "")
norway <- raster("Data/Raster/Norway.tif")  # Background raster of Norway (all values = 1)
year <- c(1900, 1950, 1990, 2000, 2010, 2014, 2019)
xy <- coordinates(norway)
x <- y <- norway
values(x) <- xy[,1]
values(y) <- xy[,2]
NIGAM_All.list <- list()
pdf("Results/GAMplotsMunicAll.pdf")
# load("Results/gam.results.all.gamma3.RData")  # Uncomment to load results from previous steps, if entire script is not run continuously
for(j in 1:length(species)){
  print(species[j])
  NIGAM_All.list[[j]] <- list()
  for(i in 1:length(year)){
    print(year[i])
    pred.ras <- stack(year[i]*norway, x, y, log(norway))
    names(pred.ras)[1:4] <- c("year", "x", "y", "logS")
    pred.dat <- as.data.frame(values(pred.ras))
    p <- norway
    pred <- predict(gam.results[[j]], pred.dat, se.fit = TRUE, type = "response") # The ordinary raster prediction does not always work: p <- predict(pred.ras, gam.results[[j]], type = "response")
    values(p) <- as.vector(pred$fit)
    p.poly <- extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
    b <- seq(0, 0.1, by = 0.001)
    n <- length(b)
    plot(p.poly, col = rev(terrain.colors(n-1))[raster::cut(p.poly$Norway, breaks = b)], main = paste(species[j], year[i]))
    values(p) <- as.vector(pred$se.fit)
    p.poly.se <- extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
    NIGAM_All.list[[j]][[i]] <- list(p = p.poly, p.se = p.poly.se)
  }
}
dev.off()
names(NIGAM_All.list) <- species

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

# Create distribution objects and format data for import to NI base (based on code from NIcalc vignette)
updatedIndicatorData <- oldIndicatorData
for(j in 1:length(species))
{
  print(species[j])
  
  # Create distributions
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
  if(any(rowsWithNAs))
  {
    for(i in rowsWithNAs)
    {
      updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]],
                                                              areaId = myData$areaIDs[i],
                                                              years = myData$years[i],
                                                              est = myData$estimatedStates[i],
                                                              lower = myData$estimatedStates[i]-myData$standardErrors[i],
                                                              upper = myData$estimatedStates[i]+myData$standardErrors[i])
    }
  }
  for(i in rowsWithoutNAs)
  {
    updatedIndicatorData[[j]] <- NIcalc::setIndicatorValues(updatedIndicatorData[[j]], 
                                                            areaId = myData$areaIDs[i], 
                                                            years = myData$years[i], 
                                                            distribution = myData$distrObjects[[i]],
                                                            datatype = myData$Datatype[i])
  } 
}

# Write updated indicator data to NI database
# (unitOfMeasurement needs to be updated manually. New units: "Funnsannsynlighet i 1x1km-ruter")
# Get token for writing to NI database if script is not run continuously (see above).
# Code below commented to avoid accidential overwriting of data in the NI database.
# Uncomment to write to database:
# for(j in 1:length(species))
# {
#   print(species[j])
#   NIcalc::writeIndicatorValues(updatedIndicatorData[[j]])
# }

# Check uploaded data by downloading the same data sets
for(j in 1:length(species))
{
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


# Appendix 2: Summaries of GAMs
names(gam.results) <- species
lapply(gam.results, summary)
