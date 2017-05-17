\name{eqnNum}
\alias{eqnNum}
\title{Gets the equation number}
\usage{
eqnNum(
  advance.counter=0,
  eqn.name="t1",
  em.space=150
  )
}
\arguments{
  \item{advance.counter}{Numeric.  Number to be added to the present table number.  Default is 0, giving the present equation}
  \item{eqn.name}{Character.  Name of equation if needed for reference in text.}
  \item{em.space}{Numeric.  Space before equation when rendered in PDF.}
}
\description{
The function relies on \code{options("equation_counter")} in
order to keep track of the current number.
}
\examples{
options(equation_counter=0)
eqnNum()
}
