#' Extract useful car sales information from a web page
#'
#' Extract useful car sales information from a web page. This function takes care to dispose properly of the transient XML document created.
#'
#' @param uri the URI (usually URL, i.e. the address of the web page of interest)
#' @param ... additional arguments passed to processDocument
#' @return a list with two elements, the first is a data frame; one line per car sold. 
#' The second item is a list of data points that needs further post processing before inclusion.
#' @export
processPage <- function(uri, ...) {
  cars <- htmlTreeParse(getURL(uri), useInternalNodes=TRUE)
  tryCatch(processDocument(cars, ...), finally= free(cars))
}


#' Extract car sales information from a list of XMLNode
#'
#' Extract car sales information from a list of XMLNode
#'
#' @param xmlDoc an object of class XMLInternalDocument
#' @param path the XPath path identifying the items of interest in the document.
#' @param attribName the name of the attribute of interest in the nodes identified by the path.
#' @return a character vector, all the values of the attributes found.
#' @export
getNodeSetAttribute <- function(xmlDoc, path, attribName='content') {
  nodeSet <- getNodeSet(xmlDoc, path)
  sapply(nodeSet, xmlGetAttr, attribName)
}

#' Extract useful car sales information from a web page
#'
#' Extract useful car sales information from a web page. Prefer using processPage.
#'
#' @param xmlDoc an object of class XMLInternalDocument
#' @param prodPath the XPath path identifying the car sale entry.
#' @return a list with two elements, the first is a data frame; one line per car sold. 
#' The second item is a list of data points that needs further post processing before inclusion.
#' @export
processDocument <- function(xmlDoc, prodPath = "//div[@itemtype='http://schema.org/Product']") {
  a <- getNodeSet(xmlDoc, paste0(prodPath, "//a"))
  metalist <- sapply(a, xmlAttrs)
  carsDf <- as.data.frame(t(metalist), stringsAsFactors=FALSE)

  getContentChecked <- function(xdf, xmlDoc, pathAppend) {
    res <- getNodeSetAttribute(xmlDoc, paste0(prodPath, pathAppend), attribName='content')
    stopifnot(is.vector(res))
    stopifnot(nrow(xdf) == length(res))
    res
  }
  carsDf$brands <- getNodeSetAttribute(carsDf, xmlDoc, "//a//div[@itemprop='brand']//meta")
  carsDf$models <- getNodeSetAttribute(carsDf, xmlDoc, "//a//meta[@itemprop='model']")

  processedInfo <- getInfoEntries(xmlDoc, paste0(prodPath, "//a//ul[@class='info']"))
  stopifnot(length(processedInfo) == nrow(carsDf))

  list(carsDf=carsDf, info=processedInfo)
}

#' Extract car sales information of variable length
#'
#' Extract car sales information of variable length
#'
#' @param xmlDoc an object of class XMLInternalDocument
#' @param infoPath the XPath path identifying the car sale information
#' @return a list; one item per car. Each item is a named character vector 
#' @export
getInfoEntries <- function(xmlDoc, infoPath) {
  infos <- getNodeSet(xmlDoc, infoPath)
  infodetails <- lapply( infos, xmlChildren)
  processedInfo <- lapply( infodetails, getInfo )
  processedInfo
}

#' Extract car sales information from a list of XMLNode
#'
#' Extract car sales information from a list of XMLNode
#'
#' @param infoHeadNodes a list of objects of class XMLNode
#' @return a named character vector
#' @export
getInfo <- function(infoHeadNodes) {
  x <- infoHeadNodes
  infoNames <- lapply( x, xmlAttrs)
  infoValues <- lapply( x, xmlValue)
  withNames <- sapply(infoNames, function(item) { !is.null(item) } )
  infoValues <- infoValues[withNames]
  names(infoValues) <- as.character(infoNames[withNames])
  infoValues
}
