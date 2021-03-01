
library(dataverse)
library(gh)
library(stringr)


#' Creates issues and projects in the QDR Github repository
#'
#' @param doi A string. The doi of a dataverse dataset in the form doi:10.1234/5678
#' @param owner A string. The owner of the github repository
#' @param repo A string. The name of the github repo
#' @export
#' @import gh
#' @import stringr
#' @import dataverse
#' @examples
#' \dontrun{
#' gh_curate("doi:10.5064/F6NUVQRR")
#' }
gh_curate <- function(doi, owner = "QualitativeDataRepository", repo="Project-Curation") {

  # Get the dataset data
  data <- get_dataset(doi)

  short_title <- get_shortTitle(get_title(data))
  metadata_text <- 'Not all of these need to be completed, but check if available/possible

  - [ ] Funder
  - [ ] depositor ORCID
  - [ ] Check depositor affiliation
  - [ ] Other contributors
  - [ ] Associated publications
  - [ ] Collection dates
  - [ ] Temporal coverage of data itself
  - [ ] Geographic coverage
  - [ ] Project Summary: Abstract
  - [ ] Data Abstract:
     - [ ] What are the data (interviews, focus groups, PDFs of XYZ)?
     - [ ] How where data selected (e.g., picking interviewees, selection among archival documents, etc.)?
     - [ ] Data collections strategy: both technical (scans/camera) and more substantial (interview/focus groups) -- though full guide should be separate document
     - [ ] How are they organized / will be organized for publication.
  - [ ] Access restrictions: on which files, under what conditions.
  '

  metadata_title <- paste(get_author(data), " - ", short_title," - ", "Metadata")

  initial_text <- paste("
  ## Information

  Contact
  Name:", get_author(data),"
  e-mail:", get_email(data),"
  DV link:", get_url(doi),"

  ## General Considerations
  - [ ] Project is in Scope
  - [ ] Project is journal supplement
  - If yes: deadline?
  - [ ] Checked human participant issues
  - [ ] Checked copyright issues",
                        sep = " "
  )

  initial_title <- paste(get_author(data), " - ", short_title," - ", "Initial Deposit & Checks")

  fileprocessing_text <- "Not all of these need to be completed, but check if available/possible

  - [ ] Create Dropbox Folder
  - [ ] Redactions
  - [ ] Rename Files
  - [ ] Enter File Metadata
  - [ ] OCR
  - [ ] Convert to PDF/A
  - [ ] Create Permanent Record of Web Sources
  - [ ] Create Readme File
  - [ ] Archive Files"

  fileprocessing_title <- paste(get_author(data), " - ", short_title," - ", "File Processing")

  publication_text <- "Not all of these need to be completed, but check if available/possible

  - [ ] Assign depositor to project
  - [ ] Review metadata entry
  - [ ] Enter QDR vocabulary (ICPSR Thesaurus)
  - [ ] Upload files
  - [ ] Tag Data/Documentation
  - [ ] Restrict files for registered users as required
  - [ ] Authorize file access to registered users
  - [ ] Add terms in both terms of use and terms of access
  - [ ] Enable Access Request
  - [ ] Select thumbnail image
  - [ ] Deposit agreement received from depositor
  - [ ] Depositor Final Review
  - [ ] Curators' Final Review (two sets of eyes, special attention to README and terms fo use/access conditions)
  - [ ] Add Distribution Date
  - [ ] Publish
  - [ ] Super User (SK) verified that archiving was sucessful
  - [ ] Tweet about data project
  - [ ] Set Google Scholar Alert (to qualitativedatarepository@gmail.com)"

publication_title <- paste(get_author(data), " - ", short_title," - ", "Publication")

  # Create Issues

  metadata_issue <- gh("POST /repos/:owner/:repo/issues", owner = owner, repo= repo, title = metadata_title, body = metadata_text)
print(paste("Created issue", metadata_title))
  initial_issue <- gh("POST /repos/:owner/:repo/issues", owner = owner, repo= repo, title = initial_title, body = initial_text)
  print(paste("Created issue", initial_title))

  fileprocessing_issue <- gh("POST /repos/:owner/:repo/issues", owner = owner, repo= repo, title = fileprocessing_title, body = fileprocessing_text)
  print(paste("Created issue", fileprocessing_title))

  publication_issue <- gh("POST /repos/:owner/:repo/issues", owner = owner, repo= repo, title = publication_title, body = publication_text)
  print(paste("Created issue", publication_title))

  issues <- list(metadata_issue, initial_issue, fileprocessing_issue, publication_issue)

  project <- gh("POST /repos/:owner/:repo/projects", owner = owner, repo= repo, name = paste(get_author(data), " - ", short_title, sep=""), .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

  projectID <- project$id

  toDo <- gh("POST /projects/:project_id/columns", project_id = projectID, name = "To do", .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

  inProgress <- gh("POST /projects/:project_id/columns", project_id = projectID, name = "In progress", .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

  done <- gh("POST /projects/:project_id/columns", project_id = projectID, name = "Done", .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

  toDoID <- toDo$id

  for (issue in issues) {
    issueCard <- gh("POST /projects/columns/:column_id/cards", column_id = toDoID, content_id = issue$id, content_type = "Issue", .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))
  }
}
