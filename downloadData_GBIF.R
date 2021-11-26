#' Download requested plant occurrence data for Norway from GBIF
#' 
#' This function downloads and unzips GBIF data that has been previously 
#' requested under a specified download key into a user-specified directory.
#' 
#' The procedure is based on the extensively commented instructions for
#' asynchronous downloading of GBIF data by Anders Finstad, available here:
#' https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
#' This allows downloading larger data sets, and citation of the download with 
#' a single doi. 
#' 
#' Note that after running `getKey_GBIF` to obtain the download key, you need 
#' to wait until GBIF has readied your dataset before you run `downloadData_GBIF`. 
#' 
#' @param key A character string representing a download key. Obtained via the
#' `getKey_GBIF` function, but can also be set manually if required.
#' 
#' @param path Directory into which GBIF data should be downloaded. 
#'
#' @return A dataframe containing downloaded GBIF occurrence records.
#' @export 
#'
#' @examples

downloadData_GBIF <- function(key, path){
  
  ## Download data
  occ_download_get(key = download_key$key, path = path)
  
  ## Print and save data citation
  data_citation <- paste0("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())
  print(data_citation)
  writeLines(data_citation, paste0(path, "/GBIF_Data_Citation.txt"))
  
  ## Open data and extract into data frame
  # Get a list of the files within the archive by using "list=TRUE" in the unzip function
  download_path <- paste0(path,"/",download_key$key,".zip")
  archive_files <- unzip(download_path, files = "NULL", list = TRUE) 
  archive_files
  
  ## Unzip the occurrence.txt data file into the working directory and import into R
  GBIF_data <- import(unzip(download_path, files = "occurrence.txt"), header = T, sep = "\t")
  return(GBIF_data)
}
