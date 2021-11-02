
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
gh_curate <- function(doi, owner = "QualitativeDataRepository", repo="Project-Curation",
                      tickets=c("metadata", "initial_checks", "file_processing", "publication", "time")) {


  # Get the dataset data
  data <- dataverse::get_dataset(doi)
  short_title <- get_shortTitle(get_title(data))
  generic_title <- paste(get_author(data), " - ", short_title)

  # Check if there's an existing project and ingest ID numbers.
  # Create a new one if not.
  existing_projects <- gh("GET /repos/:owner/:repo/projects", owner=owner, repo=repo)
  existing_projects <- lapply(existing_projects, function(x) {x[c("name", "id")]})
  existing_projects <- do.call(rbind.data.frame, existing_projects)
  if (generic_title %in% existing_projects$name) {
    projectID <- existing_projects$id[existing_projects$name==generic_title]
    toDo <- gh("GET /projects/:project/columns", project=projectID)
    toDo <- do.call(rbind.data.frame, toDo)
    toDoID <- toDo$id[toDo$name=="To do"]
  } else {
    contact_info <- paste0("Contact Name: ", get_author(data), "\n",
    "e-mail: ", get_email(data), "\n",
    "DV link: ", get_url(doi))

    project <- gh("POST /repos/:owner/:repo/projects", owner = owner, repo= repo,
                  name = generic_title, body=contact_info,
                  .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

    projectID <- project$id

    toDo <- gh("POST /projects/:project_id/columns", project_id = projectID,
               name = "To do",
               .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

    inProgress <- gh("POST /projects/:project_id/columns", project_id = projectID,
                     name = "In progress",
                     .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

    done <- gh("POST /projects/:project_id/columns", project_id = projectID,
               name = "Done",
               .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))

    toDoID <- toDo$id
  }


  # This lapply() creates issues for each specificed ticket and returns a list of their gh objects
  issues <- lapply(tickets, FUN=function(ticket, owner, repo, generic_title) {
    # Actual title of the ticket is rendered here
    title <- paste(generic_title, "-",
                   stringr::str_to_title(gsub("_", " ", ticket)))

    file <- paste0("tickets/", ticket, ".md")
    if (!file.exists(file)) {
      stop(paste(file, "is missing"))
    }

    body <- readLines(file)
    body <- paste(body, collapse="\n") # Read the file into a single vector
    message(paste("Created issue", title))

    gh("POST /repos/:owner/:repo/issues",
       owner = owner, repo=repo,
       title = title, body = body)

  }, owner, repo, generic_title)

  for (issue in issues) {
    issueCard <- gh("POST /projects/columns/:column_id/cards", column_id = toDoID, content_id = issue$id,
                    content_type = "Issue",
                    .send_headers = c(Accept = "application/vnd.github.inertia-preview+json"))
  }
}
