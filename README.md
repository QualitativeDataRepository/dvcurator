# dvcurator -- A set of common Dataverse Curation tasks

The tool is based on QDR's curation practices and will likely require modifications for other repositories.

## Functionality

The tools main function `start_curation()` initiates the process of curation by
1. Creating github issues for standard curation tasks and associating them with a github Project for the curation of the data project.
2. Creating a local curation folder with subfolders for Original Deposit and QDR Prepared (this should typically be in Dropbox)
3. Downloading the .zip file for the full data project to the Original Deposit folder and an unzipped version to QDR Prepared for further Curation
4. Downloading a list of a files with associated metadata as a .csv file to the same folder.

The tool also implements an entirely separate function, `datasets_byDate()` that creates an .html file with citations of all datasets published within a specified date range.

## Installation

This package won't go to CRAN given its customized nature. install using `devtools`:
```
library(devtools)
install_github("QualitativeDataRepository/dvcurator")
```

## Getting started

The tool requires the following environmental variables to be set:
* A github token using `Sys.setenv("GITHUB_PAT" = "key")`
* A dataverse API key using `Sys.setenv("DATAVERSE_KEY" = "key")` - this must be for the dataverse installation you will work with.
* Typically a dataverse server instance, although `start_curation()` will default to QDR in the absence of such a variable. Set using  `Sys.setenv("DATAVERSE_SERVER" = "demo.dataverse.org")` (e.g., for the demo installation)

Failure to specify either of the first two environmental variables will trigger and error and prevent the script from running.
