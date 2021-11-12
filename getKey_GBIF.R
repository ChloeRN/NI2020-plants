### Download occurrence data from GBIF
# This part is based on the extensively commented instructions for
# asynchronous downloading of GBIF data by Anders Finstad, available here:
# https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
# This allows downloading larger data sets, and citation of the download with a single doi. 

# Set up a user profile at GBIF (https://www.gbif.org), then
# run this once to store your GBIF user credentials to your R session 
 
# NOTE: This takes a while to run, and a notification email will be sent once
# the data is ready for downloading (progress can be checked on GBIF profile site)

getKey_GBIF <- function(){
  
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
  
  ## Return download key
  return(download_key)
}