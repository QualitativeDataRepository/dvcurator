library(dataverse)
library(hwriter)
library(dplyr)


#' Produces an html file with all datasets published in a given period
#'
#' @param startDate The (inclusive) start date in YYYY-MM-DD format. default 2016-01-01
#' @param endDate The (exclusive) end date in YYYY-MM-DD format. default: today's date using `Sys.Date()`
#' @param citationsDir directory for the new file to be created. Defaults to `tempdir()`
#' @param pageName name of the html file. defaults to citations.html
#'
#' @return None. Writes an html file to local disk
#' @export
#' @import dataverse
#' @import dplyr
#' @import hwriter
#' @examples
#' \dontrun{
#' datasets_byDate("2017-01-01", "2017-06-01")
#' }
datasets_byDate <-
  function(startDate = "2016-01-01",
           endDate = Sys.Date(),
           citationsDir = tempdir(),
           pageName = "citations.html") {
    fqstring <-
      paste("dateSort:[",
            startDate,
            "T00:00:00Z+TO+",
            endDate,
            "T00:00:00Z]",
            sep = "")


    datasets <-
      dataverse_search("*",
                       fq = fqstring,
                       type = "dataset",
                       per_page = 100)

    citationPage <- openPage(pageName, dirname = citationsDir)
    for (citation in datasets$citationHtml) {
      citation <- gsub("\\sQDR Main Collection.+", "", citation)
      hwrite(citation, citationPage, br = TRUE)
    }

    closePage(citationPage)
    print(paste("Saved HTML file with citations for data created between", startDate, "and", endDate, "to", citationsDir))
  }
