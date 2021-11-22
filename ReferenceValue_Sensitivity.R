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

#---------------------------------------------------------#
# Make spatial predictions from GAMs for additional years #
#---------------------------------------------------------#

## Make a list of years for which to make predictions
#yearList <- c(1900, 1950, 1990, 2000, 2010, 2014, 2019)
yearList <- c(1960, 1970, 1980)

## Execute function to predict from GAMs and plot predictions (optional)
NIGAM_All.list <- predict_GAM(species = species, year = yearList, plot.pdf = FALSE, save = TRUE)


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
