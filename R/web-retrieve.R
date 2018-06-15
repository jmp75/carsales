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

  carsDf$brands <- getContentChecked(carsDf, xmlDoc, "//a//div[@itemprop='brand']//meta")
  carsDf$models <- getContentChecked(carsDf, xmlDoc, "//a//meta[@itemprop='model']")

  processedInfo <- getInfoEntries(xmlDoc, paste0(prodPath, "//a//ul[@class='info']"))
  stopifnot(length(processedInfo) == nrow(carsDf))

  list(carsDf=carsDf, info=processedInfo)
}

#' Apply a function to an XML document 
#'
#' Apply a function to an XML document. This takes care of proper disposal of XMLDocument if given a URI as an input
#'
#' @param uriOrXmlDoc a URI or an object of class XMLInternalDocument
#' @param xmlProcessor a function that takes an XMLInternalDocument as argument
#' @return An integer, the number of pages to browse through
#' @export
processPageTry <- function(uriOrXmlDoc, xmlProcessor) {
  if(is(uriOrXmlDoc, 'XMLInternalDocument')) {
    xmlProcessor(uriOrXmlDoc)
  } else if (is.character(uriOrXmlDoc)) {
    cars <- htmlTreeParse(getURL(uriOrXmlDoc), useInternalNodes=TRUE)
    tryCatch(xmlProcessor(cars), finally= free(cars))
  }
  else
    stop('argument type not supported')
}

#' Gets the number of pages for a search criteria
#'
#' Gets the number of pages for a search criteria
#'
#' @export
getNumPages <- function(xmlDoc) {
    # <div class="sub-utility listings-sort">
      # <strong data-page="1">Page 1 of 6</strong>
    # pagesNode <- getNodeSet(xmlDoc, "//div[@class='sub-utility listings-sort']//strong")
    
    # 
    # things have changed in 2014; google chrome tells me the new XPath to use is something like:
    # //*[@id="content"]/div/div[1]/div[2]/div[2]/div[2]/div[16]/a[5]
    # but I find I need:
    pagesNode <- getNodeSet(xmlDoc, "//div[@class='pagination']//a[5]")
    stopifnot(length(pagesNode)==1)
    # s <- str_split(xmlValue(pagesNode[[1]]), ' ')[[1]]
    # as.integer(s[length(s)])
    # Changes in 2014:
    as.integer(xmlValue(pagesNode[[1]]))
}

#' Retrieve details from the individual page of the car.
#' 
#' Retrieve details from the individual page of the car.
#' 
#' @export
getCarDetails <- function( xmlDoc ) {
  carDetails <- xmlDoc
  # <dd itemprop="transmission">4 SP AUTOMATIC</dd>
  transmissionNode <- getNodeSet(carDetails, "//dd[@itemprop='transmission']")
  transmission <- xmlValue(transmissionNode[[1]])
  # <dd itemprop="fuelEfficiency">9.6L / 100Km</dd>
  fuelNode <- getNodeSet(carDetails, "//dd[@itemprop='fuelEfficiency']")
  fuel <- xmlValue(fuelNode[[1]])
  res <- c(transmission, fuel)
  names(res) <- c('transmission', 'fuel')
  res
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
  if(length(infoHeadNodes) == 0){
    list()
  } else {
    x <- infoHeadNodes
    infoNames <- lapply( x, xmlAttrs)
    infoValues <- lapply( x, xmlValue)
    withNames <- sapply(infoNames, function(item) { !is.null(item) } )
    if((length(withNames))>0) {
      infoValues <- infoValues[withNames]
      names(infoValues) <- as.character(infoNames[withNames])
    } else {
      infoValues <- list()
    }
    infoValues
  }
}


# .carListing--textHeading
# .carListing--textPrice
# .icon-gauge
# .icon-gears
# .dealer

#.listing-cars
  
#' @export
sapply_fun <- function(my_list, my_fun=rvest::html_text) {
  sapply(my_list, my_fun)
}

#' @export
lapply_fun <- function(my_list, my_fun=rvest::html_text) {
  lapply(my_list, my_fun)
}

#' @export
get_text_data <- function(node) {
  a <- xml2::xml_children(node)
  if(is.list(a)) {
    a <- a[[1]]
  }
  if(!is(a, "xml_node")) {
    return('')
  }
  attbuts <- xml_attrs(a)
  if(!is.character(attbuts)) {
    return('')
  }
  if(!("ng-init" %in% names(attbuts))) {
    return('')
  }
  return(attbuts["ng-init"])
}

#' @export
get_tabular_data <- function(strdata) {
  strdata <- str_replace(strdata, pattern='setCurrentListingId.*setGAEventData\\(', '')
  strdata <- str_replace(strdata, pattern='\\);$', '')
  a_list <- jsonlite::fromJSON(strdata)
  return(as.data.frame(a_list, stringsAsFactors=FALSE))
}

#' @export
get_html_page <- function(url, encoding='')
{
  url %>%
    read_html(encoding=encoding)
} 

#' @export
get_listings_nodes <- function(html_page)
{
  if(is.character(html_page)) {
    html_page = get_html_page(html_page)
  }
  html_page %>%
    html_nodes(".listing-cars")
} 

#' @export
get_listings_pagination <- function(html_page, css=".listing-pagination")
{
  if(is.character(html_page)) {
    html_page = get_html_page(html_page)
  }
  paginations <- html_page %>%
    html_nodes(css)
  if(is.list(paginations)) {
    if(length(paginations)>0) {
      return(paginations[[1]])
    }
  }
  return(paginations)
} 

#' @export
get_num_pages <- function(html_page, css=".listing-pagination", pattern_to_remove='^Page.* of |^of *') {
  listings_pagination_node <- get_listings_pagination(html_page, css=css)
  if(length(listings_pagination_node)==0) stop(paste0("No node found for css named ", css))
  listings_pagination_node_children <- xml_children(listings_pagination_node)
  x <- xml_children(listings_pagination_node)[1]
  # if(is(x, "xml_nodeset")) x <- xml_children(x)
  pages_txt <- html_text(x)
  if(length(pages_txt)!=1) stop("Unexpected length of the vector of characters for the pagination. Should have been length one.") 
  # [1] "Page 2 of 8"
  # or 
  # [1] "of 92"
  return(as.integer(str_replace(pages_txt, pattern=pattern_to_remove, '')))
}

#' @export
get_listings_data <- function(htmlpage) {
  data_list <- htmlpage %>%
    html_nodes(".carListing--textCol2") %>%
    html_nodes(".btn-group") %>%
    lapply_fun(my_fun=get_text_data) %>%
    lapply_fun(my_fun=get_tabular_data) %>%
    ldply
    # Note: seems one cannot use  plyr::ldply with %>%

  # Some of the information, including the, er, critical, price and year, is not in the previous data_list    
    # .carListing--textHeading
  heading <- htmlpage %>%
    html_nodes(".carListing--textHeading") %>%
    sapply_fun()

  # .carListing--textPrice
  prices <- htmlpage %>%
    html_nodes(".carListing--textPrice") %>%
    sapply_fun()
    
  hrefs <- htmlpage %>% html_nodes(".carListing.carListing-slideBtn") %>% html_attr(name='href')

  data_list$href <- hrefs
  data_list$price <- prices
  data_list$heading <- heading

  return(data_list)
}

#' @export
process_raw_info <- function ( d ) {
  tmpDist <- as.numeric(rep(NA, nrow(d)))
  hasDist <- 'distance' %in% colnames(d)
  if (hasDist) {
    tmpDist <- d$distance
  } else { 
    tmpDist <- tolower(d$Kms)
    tmpDist <- str_replace_all(distance, ' km.*', '')
    tmpDist <- str_replace_all(distance, ',', '')
    tmpDist <- str_replace_all(distance, ' ', '')
    tmpDist <- as.integer(distance)
  }
  distance <- tmpDist
  if(!is.numeric(d$price)) {
    price <- tolower(d$price)
    price <- str_replace_all(price, '\\$', '')
    price <- str_replace_all(price, ',', '')
    price <- as.integer(price)
  } else {price <- d$price}

  seller_type <- tolower(d$seller_type)
  used <- str_detect(seller_type, 'used')
  seller_status <- seller_type # as.character(rep(NA, length(used)))
  seller_status <- ifelse(str_detect(seller_type, 'dealer'),  'dealer',  seller_status)
  seller_status <- ifelse(str_detect(seller_type, 'private'), 'private', seller_status)

  # carsales: may have NA for prices for new. How to???
  # price <- ifelse(str_detect(seller_type, 'showroom'), '???', price)

  # To get the nuumber of cylinders and volume displacement is a bit more involved. 
  # Note that of course some things will be missing sometimes, with partial data cyl or L, or none.
  # [1] 4 cyl, 1.6 L 4 cyl, 1.6 L 4 cyl, 2.0 L 4 cyl, 1.8 L
  has_str_marker <- function(x, marker) {
    has_marker <- str_detect(x, marker)
    has_marker <- ifelse( is.na(has_marker), FALSE, has_marker)
    return(has_marker)
  }
  
  engine <- d$engine
  engine <- str_replace_all(engine, 'cyl ', 'cyl, ' ) # carsales has no comma separation
  has_cyl <- has_str_marker(engine, 'cyl')
  has_litres <- has_str_marker(engine, 'L')
  engine <- tolower(engine) %>% trimws

  a <- str_split(engine, ',')
  cyl <- ifelse( has_cyl, as.integer(str_replace_all(laply(a, `[`, 1), 'cyl', '')), NA )
  cylvol <- ifelse( has_litres, as.numeric(str_replace_all(laply(a, `[`, 2), 'l', '')), NA)
  
  heading_length <- str_length(d$heading)
  ma <- pmin(heading_length, 4)
  year <- ifelse(heading_length < 4, NA, d$heading)
  year <- as.integer(substr(d$heading, 1, 4))
  # it looks like quite a few new vehicles dont provide explicit Km. 
  # Since some second hand vehicles also skip this info, use year as a criteria.
  # distance <- ifelse(is.na(distance) & year > 2011, 0, distance)  

  # other columns seems to be reliable enough to be processed simply. Watch out however.
  result <- data.frame(
    brand          =  as.factor(toupper(d$make))
    ,model          = as.factor(toupper(d$model))           
    ,body_type      = as.factor(d$body_type)
    ,transmission   = as.factor(as.character(d$transmission)) 
    ,seller_type    = as.factor(seller_status)
    ,used           = as.factor(used)
    ,cyl            = as.factor(cyl)          
    ,year           = as.factor(year)          
    ,featured       = as.factor(d$featured)
    ,listingid      = d$listingid       
    ,cylvol         = cylvol
    ,location       = d$location        
    ,distance       = distance   
    ,price          = price           
    ,heading        = d$heading         
    ,href           = d$href
    ,model_year     = d$model_year
    ,stringsAsFactors = FALSE
  )
  return(result)

}

#' Apply a function to an XML document 
#'
#' Apply a function to an XML document. This takes care of proper disposal of XMLDocument if given a URI as an input
#'
#' @param html_page a URI or an object of class xml_document
#' @param xmlProcessor a function that takes an xml_document as argument
#' @return An integer, the number of pages to browse through
#' @export
process_page_try <- function(html_page, xmlProcessor) {
  if(is.character(html_page)) {
    html_page = get_html_page(html_page)
  }
  if(is(html_page, 'xml_document')) {
    return(tryCatch(xmlProcessor(cars)))
  }
  else
    stop('argument html_page is not an xml_document')
}
