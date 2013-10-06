\name{getInfoEntries}
\alias{getInfoEntries}
\title{Extract car sales information of variable length}
\usage{
  getInfoEntries(xmlDoc, infoPath)
}
\arguments{
  \item{xmlDoc}{an object of class XMLInternalDocument}

  \item{infoPath}{the XPath path identifying the car sale
  information}
}
\value{
  a list; one item per car. Each item is a named character
  vector
}
\description{
  Extract car sales information of variable length
}

