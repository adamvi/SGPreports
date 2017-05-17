\name{tblNumIncrement}
\alias{tblNumIncrement}
\title{Increase the table number counter}
\usage{
tblNumIncrement(advance.counter=1)
}
\arguments{
  \item{advance.counter}{Numeric.  Number to be added to the present table number.  Default is 1, advancing table count by 1.}
}
\description{
Bump the table number by one.  The function relies
on \code{options("table_number")} in order to keep track of the last number.
}
\examples{
options(table_number=1)
getOption("table_number")
tblNumIncrement()
getOption("table_number")
}
