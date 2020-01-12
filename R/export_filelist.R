library(dataverse)
library(dplyr)


#' Writes a CSV file of all files pertaining to the current DOI to the specified folder
#'
#' @param doi A string. The DOI of a dataset in the form doi:10.1234/5678
#'
#' @return
#' @export
#' @import dataverse
#' @examples
#' \dontrun{
#' export_files("doi:10.5064/F6NUVQRR")
#' }
export_filelist <- function (data, folder = getwd()) {
  # filename will be author_files_date using todays date
  # save to specified folder (should be project folder in Dropbox)
  filename <- paste(get_author(data), "_files_", Sys.Date(), ".csv", sep = "")
  print(paste("Saving file", filename, "to", folder))
  filepath <- paste(folder, "/", filename, sep="")
  write.csv(get_filelist(data), filepath)
}


#' Extracts a list of filenames with selected characteristics from a dataverse dataset object
#'
#' @param data A list. The response of the dataverse API to a request for dataset metadata
#'
#' @return A data frame containing all filenames with selected characteristics
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' get_filelist(data)
#' }
get_filelist <- function (data) {
  # Take only the files and remove the first column to prevent duplicates
  data_files <- data[['files']]

  # collapse checksum
  data_files$checksum <- data_files$checksum$value
  # select relevant columns; you can edit this list.
  # Use colnames(data_files for a complete list)
  data_files <- data_files %>% select_if(names(.) %in%
                       c("label", "description", "restricted", "contentType", "checksum", "directoryLabel"))
  return (data_files)
}




