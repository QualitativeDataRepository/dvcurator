#' Initiaties a curation project: Creates gh projects, DB folder, downloads files & filelist
#'
#' @param doi A string. the dataset DOI in the form "doi:10.1234/5667"
#' @param folder A path to a folder, typically the local Dropobox folder
#' @param owner A string. The owner of the github repository to be used
#' @param repo A string. The name of the github repository to be used
#'
#' @export
#'
#' @examples
#' \dontrun{
#' start_curation("doi:10.5064/F6NUVQRR", folder = "C:/Users/Sebastian/Dropbox (QDR)")
#' }
start_curation <- function(doi, folder = getwd(), owner = "QualitativeDataRepository", repo = "Project-Curation",
                           issues=c("metadata", "initial_checks", "file_processing", "publication", "time")) {
  # Make sure keys are set
  if (Sys.getenv("DATAVERSE_KEY") == "") {
    stop("Please set a QDR API key using sys.sentenv('DATAVERSE_KEY' = 'key')")
  }
  if (Sys.getenv("GITHUB_PAT") == "") {
    stop("Please set a github API token using sys.sentenv('GITHUB_PAT' = 'key')")
  }
  if (Sys.getenv("DATAVERSE_SERVER") == "") {
    Sys.setenv("DATAVERSE_SERVER" = "data.qdr.syr.edu")
    print("Setting Dataverse to QDR")
  }
  data <- dataverse::get_dataset(doi)

  # Create github projects
  gh_curate(doi, owner, repo, issues)

  # create DB folders
  folderpath <- create_folder(data, folder)

  # save Zip of files to DB folder

  zip <- download_files(data)
  save_files(zip, data, folderpath)

  # Save list of original files and description to project folder
  export_filelist(data, folderpath)
}
