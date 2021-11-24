#' Run a generalized linear model (GAM) on spati-temporal occurrence data
#'
#' This function loads training data for a (list of) species and fits a 
#' space-time generalized additive model to it.
#'  
#' The response variable in `training_data` is the quantity `Y`, representing
#' the count of observations of a species within 1km by 1km cells over a 
#' five-year interval. The fitted effects are geographic coordinates (x and y
#' in meters), year (5-year intervals), and their two-way interactions, with a
#' smoothing parameter of 3.  
#' Sampling effort is included as an offset, and all data is analysed (no 
#' spatial precision filtering).
#' For more details on the modeling approach, consult earlier documentation
#' by O. Skarpaas (NI-2020-plants-report.docx).
#' 
#' @param species A character string or vector of character strings containing
#' the latin name of the species for which to run the GAM
#' @param save Logical, default = `TRUE`. When `TRUE`, model fits for all 
#' species are saved in the subfolder Results as `gam.results.all.gamma3.RData`.
#' Set to `FALSE` to skip saving of new GAM fits.
#'
#' @return A list of GAM fits, one fit per species. 
#' @export
#'
#' @examples
#' 
run_GAM <- function(species, save = TRUE){
  gam.results <- list()

  for(j in 1:length(species)){
      
    #cat(species[j],"\n")
    message(paste0("Fitting GAM for ", species[j], " ..."))
     
    # Training data for species from GBIF, see NI 2020 plants dataprep.r
    load(paste("Data/Regression data/", species[j], "_training_data_all.RData", sep=""))
    #load(paste("Data/Regression data/", species[j], "_training_data_all", sep=""))
    
    d <- training_data[, c("Y", "x", "y", "year", "logS")]
    m <- gam(Y ~ ti(x) + ti(y) + ti(year) + ti(x, year) + ti(y, year),
               data = d, gamma = 3, family = poisson, offset = logS, select = TRUE)
    gam.results[[j]] <- m
  }
  
  if(save){
    save(gam.results, file = "Results/gam.results.all.gamma3.RData")
  }
  
  return(gam.results)
}
