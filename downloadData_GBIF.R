### Download occurrence data from GBIF
# This part is based on the extensively commented instructions for
# asynchronous downloading of GBIF data by Anders Finstad, available here:
# https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
# This allows downloading larger data sets, and citation of the download with a single doi. 

# Set up a user profile at GBIF (https://www.gbif.org), then
# run this once to store your GBIF user credentials to your R session 
 
# NOTE: This takes a while to run, and a notification email will be sent once
# the data is ready for downloading (progress can be checked on GBIF profile site)

downloadData_GBIF <- function(path){
  
  ## Provide user credentials for GBIF
  options(gbif_user = rstudioapi::askForPassword("GBIF username"))
  options(gbif_email = rstudioapi::askForPassword("Registered GBIF e-mail"))
  options(gbif_pwd = rstudioapi::askForPassword("GBIF password"))
  
  ## Find plant taxonkey - get list of gbif keys to filter download
  key <- name_suggest(q = 'Plantae', rank = 'kingdom')$data$key[1] 
  
  ## Get download key for all occurrences of plants with coordinates in Norway
  download_key <- 
    occ_download(
      pred('taxonKey', key),
      pred('hasCoordinate', 'TRUE'),
      pred('country', 'NO'),
      type = 'and'
    ) %>% 
  occ_download_meta
  
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
}