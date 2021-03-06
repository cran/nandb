% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_var.R
\name{cross_var}
\alias{cross_var}
\title{Calculate the \emph{cross-variance} of two vectors.}
\usage{
cross_var(x, y)
}
\arguments{
\item{x}{A numeric vector.}

\item{y}{A numeric vector with the same length as \code{x}.}
}
\value{
A number
}
\description{
The cross-variance function is defined in the reference.
}
\examples{
cross_var(0:3, 2:5)
}
\references{
Digman, MA, Wiseman, PW, Choi, C, Horwitz, AR, Gratton, E (2009).
Stoichiometry of molecular complexes at adhesions in living cells. Proc.
Natl. Acad. Sci. U.S.A., 106, 7:2170-5.
}
