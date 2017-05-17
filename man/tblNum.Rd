\name{tblNum}
\alias{tblNum}
\title{Gets the table number}
\usage{
tblNum(advance.counter=0)
}
\arguments{
  \item{advance.counter}{Numeric.  Number to be added to the present table number.  Default is 0, giving the present table.}
}
\description{
The function relies on \code{options("table_number")} in
order to keep track of the current number.
}
\examples{
options(table_number=1)
tblNum()
}
