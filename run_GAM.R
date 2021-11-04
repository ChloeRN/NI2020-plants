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
