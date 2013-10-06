\name{processDocument}
\alias{processDocument}
\title{Extract useful car sales information from a web page}
\usage{
  processDocument(xmlDoc,
    prodPath = "//div[@itemtype='http://schema.org/Product']")
}
\arguments{
  \item{xmlDoc}{an object of class XMLInternalDocument}

  \item{prodPath}{the XPath path identifying the car sale
  entry.}
}
\value{
  a list with two elements, the first is a data frame; one
  line per car sold. The second item is a list of data
  points that needs further post processing before
  inclusion.
}
\description{
  Extract useful car sales information from a web page.
  Prefer using processPage.
}

