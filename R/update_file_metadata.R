#' Retrieves file metadata information for a file given its ID
#'
#' @param file The numeric ID for the file
#' @param key  The API key
#' @param server The dataverse installation to be used
#' @importFrom jsonlite fromJSON
#' @import httr
#' @return The JSON return from the API as a List
#' @export
#'
#' @examples fileJSON <- get_file_json(396501, key="", server = "demo.dataverse.org")
get_file_json <-
  function(file,
           key = Sys.getenv("DATAVERSE_KEY"),
           server = "data.qdr.syr.edu") {
    u <- paste0(api_url(server), "files/", file, "/metadata/draft")
    r <-
      httr::GET(u, httr::add_headers("X-Dataverse-key" = key))
    out <- httr::content(r, as = "text")
    return(jsonlite::fromJSON(out))
  }



#' Updates the file metadata of a given file
#'
#' It will preserve all metadata that's not specifically addressed
#' @param file The numeric ID of a file
#' @param label A string. filename/label of the file.
#' @param directoryLabel A string. The directory for the file _not_ starting with a slash
#' @param description A string. The description of the file
#' @param restricted Boolean. TRUE (for restricted), FALSE for not.
#' @param categories A string. Tags for the file, comma or semicolon separated.
#' @param key The API key for the dataverse installation
#' @param server The dataverse installation to be used
#' @importFrom jsonlite toJSON
#' @import httr
#' @return The API response (as text)
#' @export
#'
#' @examples \dontrun{
#' # Updates the file description
#' update_message <- update_file_metadata(396501, description = "New file description")
#' }
update_file_metadata <-
  function(file,
           label = NULL,
           directoryLabel = NULL,
           description = NULL,
           restricted = NULL,
           categories = NULL,
           key = Sys.getenv("DATAVERSE_KEY"),
           server = "data.qdr.syr.edu") {
    metadata_fields <-
      list(
        "label" = label,
        "directoryLabel" = directoryLabel,
        "description" = description,
        "restricted" = restricted,
        "categories" = categories
      )
    existing_data <- get_file_json(file, key, server)

    for (field in names(metadata_fields)) {
      value <- metadata_fields[[field]]
      if (!is.null (value)) {
        if (field == "categories") {
          # split tags by comma or semicolon
          value <- unlist(strsplit(value, "[,;]\\s*"))
        }
        existing_data[[field]] <- value
      }
    }
    new_json <-
      as.character(jsonlite::toJSON(existing_data, auto_unbox = FALSE))
    u <- paste0(api_url(server), "files/", file, "/metadata")
    r <-
      httr::POST(
        u,
        httr::add_headers("X-Dataverse-key" = key),
        body = list(jsonData = new_json),
        encode = "multipart"
      )
    httr::stop_for_status(r)
    return (httr::content(r, as = "text", encoding = "UTF-8"))
  }


#' Updates a list of files based on their id and corresponding vectors
#'
#' The assumption here is that this is based on a dataframe/spreadsheet so that file IDs and metadata vectors are guaranteed to be aligned
#'
#' @param files A numerical vector. Dataverse IDs
#' @param label A character vector. Filenames for the files
#' @param directoryLabel A character vector. Folders for the files
#' @param description A character vector. Descriptions for files
#' @param restricted A boolean vector. Whether files are restricted
#' @param categories A character vector. File tags. Mulitple tags are comma or semicomma separated
#' @param key The API key for the dataverse installation
#' @param server The dataverse installation
#' @importFrom jsonlite fromJSON
#' @seealso update_file_metadata
#'
#' @export
#'
#' @examples \dontrun{
#' bulk_update_file_metadata(
#'   c(402881, 395305),
#'   description = c("new description", "other new description"),
#'   categories = c("Data", "Documentation"),
#'   server = "demo.dataverse.org"
#' )
#' }
#'


bulk_update_file_metadata <- function(files,
                                      label = NULL,
                                      directoryLabel = NULL,
                                      description = NULL,
                                      restricted = NULL,
                                      categories = NULL,
                                      key = Sys.getenv("DATAVERSE_KEY"),
                                      server = "data.qdr.syr.edu") {
  metadata_fields <-
    list(
      "label" = label,
      "directoryLabel" = directoryLabel,
      "description" = description,
      "restricted" = restricted,
      "categories" = categories
    )

  # remove NULL vectors
  for (i in length(metadata_fields):1) {
    if (is.null(metadata_fields[[i]])) {
      metadata_fields <- metadata_fields[-i]
    }
  }

  # Make sure all our vectors are the same length
  for (field in names(metadata_fields)) {
    if (length(metadata_fields[[field]]) != length(files)) {
      stop(paste(
        "The vetor for",
        field,
        "needs to be the same length as number of file ids"
      ))
    }
  }
  # Create empty data frame
  results <- data.frame()

  for (i in 1:length(files)) {
    # Skipping lines with non-numeric file ids.
    if (!is.numeric(files[i])) {
      warning(paste("File id", files[i], "is not numeric; skipping files"))
      next
    }
    else {
      return <- update_file_metadata(
        files[i],
        label = label[[i]],
        directoryLabel = directoryLabel[[i]],
        description = description[[i]],
        restricted = restricted[[i]],
        categories = categories[[i]],
        key,
        server
      )
      # Trun response into a list and flatten
      return <-
        jsonlite::fromJSON(sub("File Metadata update has been completed: ", "", return))
      return <- sapply(return, paste, collapse = ", ")
      # Add to data frame
      results <-
        rbind(results, as.list(return), stringsAsFactors = FALSE)
    }
  }
  return (results)
}
