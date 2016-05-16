##' Downloads datasets from NHANES.
##'
##' \code{getNHANES} takes in a set of years, files, and variables along with a
##'   destination directory and returns a dataframe with data from all years and
##'   files put together.
##' @param years Vector of years.  Must be in \code{c(1999, 2001, 2003, 2005,
##'   2007, 2009, 2011, 2013)}.
##' @param files Vector of filenames as specified by NHANES.
##' @param variables A list of variables where each element of the list is a
##'   vector of variables corresponding to each file.  Every element in the
##'   \code{files} parameter must have an element in the \code{variables}
##'   parameter.
##' @param dir Destination directory for downloaded files.
##' @return Dataframe containing all data from specified years and files.
getNHANES <- function(years, files, variables = NULL, dir) {
  require(dplyr)
  yearLetters <- c("1999" = "A", "2001" = "B", "2003" = "C", "2005" = "D",
                   "2007" = "E", "2009" = "F", "2011" = "G", "2013" = "H")
  dataList <- list()
  i <- 1
  for (y in years) {
    dataListYear <- list()
    j <- 1
    for (f in files) {
      url <- paste("http://wwwn.cdc.gov/Nchs/Nhanes/", y, "-", y+1, "/", f,
                   "_", yearLetters[as.character(y)], ".XPT", sep = "")
      data <- retrieveFile(url, dir, format = "xpt")
      if (!is.null(variables)) {
        dataListYear[[j]] <- data[,variables[[j]]]
      } else {
        dataListYear[[j]] <- data
      }
      j <- j + 1
    }
    dataList[[i]] <- Reduce(dplyr::full_join, dataListYear)
    i <- i + 1
  }
  finalData <- do.call(rbind, dataList)
  return(finalData)
}


##' Retrieves a file from the Internet
##'
##' \code{retrieveFile} takes in a url and a directory and checks if the
##' specified file already exists. If it does not exist, the file is downloaded
##' into the directory. The file is loaded into R using \code{rio::import} and
##' returned as a dataframe.
##' @param url URL of the file.
##' @param dir Destination directory.
##' @param ... Additional parameters passed to \code{rio::import}.
##' @return Dataframe with specified data.
retrieveFile <- function(url, dir, ...) {
  split <- unlist(strsplit(url, "/"))
  fname <- paste(dir, split[length(split)], sep = "/")
  if (!file.exists(fname)) {
    message(paste(fname, "does not exist.  Downloading file..."))
    dir.create(dir, showWarnings = FALSE)
    download.file(url, fname)
  }
  message(paste("Importing", fname))
  return(rio::import(fname, ...))
}
