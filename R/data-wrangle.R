
#' @export
potentialBrands <- c("citroen", "ford", "holden", "honda", "hyundai", "jeep", "kia", "land rover", "mazda", "mitsubishi", "nissan", "peugeot", "range rover", "renault", "subaru", "suzuki", "toyota", "volkswagen") 

#' @export
modelsOfInterest <- function() {
  models <- list()
  # sort(as.character(unique(d_toy$model)))
  #  can replace with regexp on  \" *\"  in notepad plus plus
  models[["toyota"]] <- c( "aurion", "camry", "corolla", "fj cruiser", "hilux", "kluger", "landcruiser", "rav4", "tarago")
  models
}

#' @export
keep <- function( d, colname, kept) {
  d_2 <- subset(d, d[[colname]] %in% kept)
  droplevels(d_2)
}

#' @export
keepBrands <- function( d, brandsKept=c(potentialBrands)) {
  keep(d, 'brand', brandsKept )
}

#' @export
keepModels <- function( d, kept ) {
  keep(d, 'model', kept )
}

#' Process a data frame to get data useful for plotting
#'
#' Process a data frame to get data useful for plotting
#'
#' @param d a single data frame with columns containing all the raw outputs from the processing, as returned by processIngestedData
#'  Expected columns available are at least: "data-price", "data-seotitle", "brand", "model", "engine.icon.car.engine", "km.icon.kilometers"
#' @return a data frame with columns [1] "brand"        "model"        "distance"     "cyl"          "cylvol"       "price"        "year_integer" "year".  
#' Brand, models, cyl and years are already factors.
#' @export
makePlotable <- function ( d ) {
  distance <- tolower(d$km.icon.kilometers)
  distance <- str_replace_all(distance, ' kms', '')
  distance <- str_replace_all(distance, ',', '')
  distance <- as.integer(distance)
  # To get the nuumber of cylinders and volume displacement is a bit more involved. 
  # Note that of course some things will be missing sometimes, with partial data cyl or L, or none.
  # [1] 4 cyl, 1.6 L 4 cyl, 1.6 L 4 cyl, 2.0 L 4 cyl, 1.8 L

  has_cyl <- str_detect(d$engine.icon.car.engine, 'cyl')
  has_cyl <- ifelse( is.na(has_cyl), FALSE, has_cyl)

  has_litres <- str_detect(d$engine.icon.car.engine, ' L')
  has_litres <- ifelse( is.na(has_litres), FALSE, has_litres)

  a <- str_split(tolower(d$engine.icon.car.engine), ',')
  cyl <- ifelse( has_cyl, as.integer(str_replace_all(laply(a, `[`, 1), ' cyl', '')), NA )
  cylvol <- ifelse( has_litres, as.numeric(str_replace_all(laply(a, `[`, 2), ' l', '')), NA)
  
  year <- as.integer(substr(d$`data-seotitle`, 1, 4))
  # it looks like quite a few new vehicles dont provide explicit Km. 
  # Since some second hand vehicles also skip this info, use year as a criteria.
  distance <- ifelse(is.na(distance) & year > 2011, 0, distance)  

  # other columns seems to be reliable enough to be processed simply. Watch out however.
  d_1 <- data.frame(
    brand = as.factor(tolower(d$brand)),
    model = as.factor(tolower(d$model)),
    distance = as.integer(distance),
    cyl = as.factor(cyl),
    cylvol = cylvol,
    price = as.numeric(d$`data-price`),
    title = as.character(d$`data-title`),
    year_integer = year, # need this for subsetting, I think. Factors confusing
    year = as.factor(year), 
    vehicletype = as.factor(as.character(d$`data-vehicletype`)),
    bodytype = as.factor(as.character(d$`body.type.icon.car`)),
    href = d$href,
    transmission = as.factor(as.character(d$transmission.icon.transmission)),
    stringsAsFactors = FALSE
  )
  d_1
}


#' Process the data ingested from web pages into a data frame
#'
#' Process the data ingested from web pages into a data frame
#'
#' @param dAll a list, where each element is the raw result of a page retrieval via the function processPage
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
