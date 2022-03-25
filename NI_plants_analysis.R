#-------#
# Setup #
#-------#

## Load required packages

# NIcalc package for reading/writing to NI database
#devtools::install_github("NINAnor/NIcalc", build_vignettes = T)
library(NIcalc)

# Other libraries needed:
# magrittr, rgbif, rgdal, sp, raster, rio, mgcv, tidyverse
library(magrittr)
library(rgbif)
library(rgdal)
library(sp)
library(raster)
library(rio)
library(mgcv)


## Source function scripts
source("downloadData_NIdb.R")
source("getKey_GBIF.R")
source("downloadData_GBIF.R")
source("build_TrainingData.R")
source("run_GAM.R")
source("predict_GAM.R")
source("calculate_IndicatorValues.R")
source("create_DistObjects.R")
source("uploadData_NIdb.R")

## Set species and indicator names & check that they correspond
indicators <- c("Alm","Fjellvalmue","Greplyng","Issoleie","Kusymre","Myrtelg","Olavsstake","Prestekrage",
                "Purpurlyng","Sennegras","Solblom","Sveltstarr","Engmarihand","Hvitmyrak","Brunmyrak","Smalsoldogg","Dikesoldogg")
species <- c("Ulmus glabra","Papaver radicatum","Kalmia procumbens","Ranunculus glacialis","Primula vulgaris",
             "Thelypteris palustris","Moneses uniflora","Leucanthemum vulgare","Erica cinerea","Carex vesicaria",
             "Arnica montana","Carex pauciflora","Dactylorhiza incarnata","Rhynchospora alba","Rhynchospora fusca",
             "Drosera anglica","Drosera intermedia")

length(indicators) == length(species)

## Set "switches" for executing different steps of the workflow
downloadOldNIData <- FALSE
downloadGBIFData <- FALSE

#------------------------------------------------------------#
# Retrieve old indicator data from NI database or local save #
#------------------------------------------------------------#

if(downloadOldNIData){
  
  ## Execute function for downloading old indicator data
  oldIndicatorData <- downloadData_NIdb(species = species, indicators = indicators, save = TRUE)

}else{
  
  ## Load old indicator data from local save
  load('oldIndicatorData.RData')
}

#------------------------------------------------------#
# Retrieve raw occurrence data from GBIF or local save #
#------------------------------------------------------#

## Execute function for setting up download key and requesting download
if(downloadGBIFData){
 download_key <- getKey_GBIF()
 message("Download key retrieved.")
 message("Wait until your GBIF download is ready (typically within 15 minutes, but can take up to 3 hours).")
 message("You should receive an email confirmation once the download is ready.")
}

## Set path for storing raw GBIF data
path <- "/data/P-Prosjekter/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data" # Server
#path <- "P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Karplanter_Dataflyt/GBIF_Data" # Local

if(downloadGBIFData){
  
  ## Execute function to download, unzip, and import raw data (using generated key)
  sp <- downloadData_GBIF(key = download_key$key, path = path)
  
  ## Move file from working directory into GBIF data folder
  fs::file_move("occurrence.txt", paste0(path, "/occurrence.txt"))
  
}
  
  
## Import raw data from previously downloaded occurrence file
GBIF_data <- import(paste0(path, "/occurrence.txt"), header = T, sep = "\t")


#--------------------------------------------#
# Build training data for each focal species #
#--------------------------------------------#

## Set start, end, and year interval for time period of interest
min.year <- 1820
max.year <- 2020
year.interval <- 5

## Execute function for building training data
build_TrainingData(GBIF_data, species, min.year, max.year, year.interval, save.unfiltered = TRUE)

## Declutter workspace
rm(list = setdiff(ls(), 'species'))


#------------------------#
# Analyse data with GAMs #
#------------------------#

## Execute function to run GAM analyses
gam.results <- run_GAM(species = species, save = TRUE)


#------------------------------------#
# Make spatial predictions from GAMs #
#------------------------------------#

## Make a list of years for which to make predictions
yearList <- c(1900, 1950, 1990, 2000, 2010, 2014, 2019)

## Execute function to predict from GAMs and plot predictions (optional)
NIGAM_All.list <- predict_GAM(species = species, year = yearList, plot.pdf = TRUE, save = TRUE)


#---------------------------------------------------#
# Calculate indicator values from model predictions #
#---------------------------------------------------#

## Execute function to calculate indicator values
newIndicatorData <- calculate_IndicatorValues(species = species, year = yearList, save = TRUE)


#-----------------------------------------------------------------------#
# Create distribution objects and format data for import to NI database #
#-----------------------------------------------------------------------#

## Execute function to create and format distribution object data
updatedIndicatorData <- create_DistObjects(species = species, save = TRUE)


#----------------------------------------------------------#
# Compare updated indicator data and upload to NI database #
#----------------------------------------------------------#

## Exectue function to compare updated indicator data to previous
uploadData_NIdb(species = species, speciesList = species, mode = "compare")

## Exectue function to write updated indicator data to NI database
#uploadData_NIdb(species = species, speciesList = species, mode = "overwrite")


