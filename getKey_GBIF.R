#' Request a key for download of plant occurrence data for Norway from GBIF
#'
#' This function requests a download key for all GBIF occurrence data (with
#' complete coordinate information) of plants in Norway.
#' 
#' The procedure is based on the extensively commented instructions for
#' asynchronous downloading of GBIF data by Anders Finstad, available here:
#' https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
#' This allows downloading larger data sets, and citation of the download with 
#' a single doi. 
#'
#' Requesting a download key requires  a user profile at GBIF 
#' (https://www.gbif.org) and the running the function will prompt you to enter 
#' your GBIF login credentials (user name, registered email, password).
#'
#' Once you have requested the key, GBIF will prepare your download.
#' This takes a while (typically around 15 minutes, but can take up to 3 hours), 
#' and a notification email should be sent once the data is ready for downloading.
#' You can also check the progress on your GBIF profile site.
#' 
#' @return A character string representing the requested download key.
#' @export
#'
#' @examples
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