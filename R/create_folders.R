#' Creates a download directory named QDR Project - Author with two subfolders in the specified folder
#'
#' @param data A dataverse dataset object
#' @param folder The parent folder in which the download directory will be created. Typically the Dropbox folder.
#'
#' @return The filepath of the created folder
#' @export
#'
#' @examples \dontrun{
#' data <- get_dataset("doi:10.5064/F6NUVQRR")
#' folderpath <- create_folder(data, folder="C:/Users/Sebastian/Dropbox/QDR Project - Smith")
#' }
create_folder <- function(data, folder = getwd()) {
  folderpath <-
    paste(folder, "/QDR Project - ", get_author(data), sep = "")

  # If the folder already exists, create the folder with short title
  if (dir.exists(folderpath)) {
    folderpath <-
      paste(folderpath, " - ", get_shortTitle(get_title(data)))
  }
  if (dir.exists(folderpath)) {
    stop("Someone seems to already have created that folder. Check on that before proceeding")
  }
  dir.create(folderpath)
  dir.create(paste(folderpath, "/QDR Prepared", sep = ""))
  dir.create(paste(folderpath, "/Original Deposit", sep = ""))

  print(paste("Created Dropbox folder", folderpath,  "and subfolders"))
  return (folderpath)
}

#' Returns the zip file of all files in a dataset as a raw vector
#'
#' @param data A dataverse dataset object
#'
#' @return The .zip file of all datafiles as a raw vector
#' @export
#'
#' @examples \dontrun{
#' data <- get_dataset("doi:10.5064/F6NUVQRR")
#' zip <- download_files(data)
#' }
download_files <- function (data) {
  fileids <- data[['files']]$id
  # Implement Zip download from DV while not available through client
  fileids <- paste0(fileids, collapse = ",")
  u <- paste0(api_url(), "access/datafiles/", fileids, "?format=original")
  r <-
    httr::GET(u, httr::add_headers("X-Dataverse-key" = Sys.getenv("DATAVERSE_KEY")))
  httr::stop_for_status(r)
  return (httr::content(r, as = "raw"))
}

#' Save a downloaded zip file to the appropriate folder
#'
#' @param zip A downloaded zip file as a raw vector
#' @param data A dataverse dataset object
#' @param folder A folder, typically the project's folder on Dropbox
#'
#' @return
#' @export
#' @description Saves the zip file in the "/Original Deposit" folder in the project folder, and extracts the zip file, minus the Manifest, into the "/QDR Prepared" subfolder
#' @examples \dontrun{
#' data <- get_dataset("doi:10.5064/F6NUVQRR")
#' zip <- download_files(data)
#' save_files(zip, data)
#' }
save_files <- function(zip, data, folder = getwd()) {
  zip_filename <-
    paste(folder,
          "/Original Deposit/",
          get_author(data),
          "_deposit.zip",
          sep = "")
  writeBin(zip, zip_filename)
  print(paste("Saved Zip file ", get_author(data), "deposit.zip to Original Deposit folder", sep=""))
  to_extract <- utils::unzip(zip_filename, list = TRUE)
  lapply(to_extract$Name[to_extract$Name != "MANIFEST.TXT"], function(zipf) {
    utils::unzip(
      zipfile = zip_filename,
      files = zipf,
      exdir = paste(folder, "/QDR Prepared", sep = "")
    )
  })
  print("Extracted all datafiles to QDR Prepared folder")
}

# Copied from dataverse package while needed
api_url <-
  function(server = Sys.getenv("DATAVERSE_SERVER"),
           prefix = "api/") {
    if (is.null(server) || server == "") {
      stop("'server' is missing with no default set in DATAVERSE_SERVER environment variable.")
    }
    server_parsed <- httr::parse_url(server)
    if (is.null(server_parsed[["hostname"]]) ||
        server_parsed[["hostname"]] == "") {
      server_parsed[["hostname"]] <- server
    }
    if (is.null(server_parsed[["port"]]) ||
        server_parsed[["port"]] == "") {
      domain <- server_parsed[["hostname"]]
    } else {
      domain <-
        paste0(server_parsed[["hostname"]], ":", server_parsed[["port"]])
    }
    return(paste0("https://", domain, "/", prefix))
  }
