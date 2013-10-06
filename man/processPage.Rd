\name{processPage}
\alias{processPage}
\title{Extract useful car sales information from a web page}
\usage{
  processPage(uri, ...)
}
\arguments{
  \item{uri}{the URI (usually URL, i.e. the address of the
  web page of interest)}

  \item{...}{additional arguments passed to
  processDocument}
}
\value{
  a list with two elements, the first is a data frame; one
  line per car sold. The second item is a list of data
  points that needs further post processing before
  inclusion.
}
\description{
  Extract useful car sales information from a web page.
  This function takes care to dispose properly of the
  transient XML document created.
}

