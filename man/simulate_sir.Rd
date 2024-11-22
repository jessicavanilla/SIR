% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_sir.R
\name{simulate_sir}
\alias{simulate_sir}
\title{Simulating a SIR infection scenario}
\usage{
simulate_sir(prob = 0.125, X)
}
\arguments{
\item{prob}{A float indicating probability of infection.}

\item{X}{A matrix of 0's, 1's, and/or 2's.}
}
\value{
A list of the number of iterations until the simulation ended, the
probability of infection, the proportion of cells infected, and the matrix
itself.
}
\description{
Simulating a SIR infection scenario
}
\examples{
simulate_sir(X = infect_corners(50, 50))

simulate_sir(prob = 0.2, X = random_infection(25, 25))
}
