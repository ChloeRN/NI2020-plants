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
