
#' Extracts the first author last name
#'
#' @param data a dataverse API object for a dataset
#'
#' @return author the last name of the author
#' @export
#'
#' @examples
#' \dontrun{
#' get_author(data)
#' }
get_author <- function(data) {
  author <- gsub("\\s+", "", gsub(",.*", "", data$metadataBlocks$citation$fields$value[[2]]$authorName$value[1]))
  return (author)
}

#' Returns the email of the first listed contact of the
#'
#' @param data a dataverse API object for a dataset
#'
#' @return email address of the first listed contact
#' @export
#'
#' @examples
#' \dontrun{
#' get_email(data)
#' }
get_email <- function(data) {
  return (data$metadataBlocks$citation$fields$value[[3]]$datasetContactEmail$value)
}

#' Extracts the title of the datasets from its object
#'
#' @param data
#'
#' @return The title of the dataset. A string
#' @export
#'
#' @examples
#' \dontrun{
#' get_title(data)
#' }
get_title <- function(data) {
  return (data$metadataBlocks$citation[['fields']][1,"value"])
}

#' Creates the QDR link for a dataset
#'
#' @param doi
#'
#' @return the QDR url for a dataset. A string.
#' @export
#'
#' @examples
#' url <- get_url("doi:10.5064/F6NUVQRR")

get_url <- function(doi) {
  return(paste("https://data.qdr.syr.edu/dataset.xhtml?persistentId=", doi, sep=""))
}

#' Returns a shortened title
#'
#' @param title A string, typically a dataset title.
#' @description Removes "Data for:" and "Replication Data for:" and uses the first five words for the title or the title up to the first colon, whichvever comes first.
#' @return short_title, a string.
#' @export
#' @import stringr
#' @examples
#' short_title <- get_shortTitle("Data for: From Pews to Politics: Religious Sermons and Political Participation in Africa")
get_shortTitle <- function(title) {
  short_title <- str_remove(title, "(Replication )?[Dd]ata for: ") %>% str_extract("^(.+?\\s){1,5}") %>% str_trim() %>% str_remove(":.+")
  if (is.na(short_title)){
    short_title <- title
  }
  return(short_title)
}
