
#' Process the data ingested from web pages into a data frame
#'
#' Process the data ingested from web pages into a data frame
#'
#' @param dAll a list, where each element is such that 
#' @export
processIngestedData <- function ( dAll ) {
  infoCat <- getAllInfoTypes(dAll)
  info <- collateInfo(dAll, infoCat)
  d <- mergeDataFrames(dAll)
  cbind(d, info)
}


#' @export
 getUniqueInfoCategory <- function(dInfo) { # d a list of list of element characters
  x <- llply(dInfo, names)
  unique(unlist(x))
}

#' @export
collate <- function(d, cats) { # d is a list of element characters
  # List of 4
 # $ engine icon-car-engine        : chr "4 cyl, 1.6 L"
 # $ transmission icon-transmission: chr "Automatic"
 # $ body-type icon-car            : chr "Hatchback"
 # $ km icon-kilometers            : chr "9,421 Kms"
  res <- as.list(as.character(rep(NA, length(cats))))
  names(res) <- cats
  for(x in names(d)) {
    res[[x]] <- d[[x]]
  }
  as.data.frame(res)
}

#' @export
collateBatch <- function(d, cats) { # d a list of list of element characters
# a List of:
  # List of 4
 # $ engine icon-car-engine        : chr "4 cyl, 1.6 L"
 # $ transmission icon-transmission: chr "Automatic"
 # $ body-type icon-car            : chr "Hatchback"
 # $ km icon-kilometers            : chr "9,421 Kms"
  d <- ldply(d, collate, cats)
}

#' @export
mergeDataFrames <- function(dAll) {
  d <- llply(dAll, `[[`, 1 )# cars[[X]][[1]]
  ldply(d)
}

#' @export
getAllInfoTypes <- function(dAll) {
  d <- llply(dAll, `[[`, 2 )# cars[[X]][[2]]
  unique(unlist(llply(d, getUniqueInfoCategory)))
}

#' @export
collateInfo <- function(dAll, infoCat) {
  d <- llply(dAll, `[[`, 2 ) # cars[[X]][[2]]
  d <- llply(d, collateBatch, infoCat)
  ldply(d)
}
