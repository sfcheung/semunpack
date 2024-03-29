% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_model.R
\name{annotate_matrices}
\alias{annotate_matrices}
\alias{print.annotate_matrices}
\title{Annotate the Matrices of a Lavaan Model}
\usage{
annotate_matrices(fit)

\method{print}{annotate_matrices}(x, ...)
}
\arguments{
\item{fit}{The output of \code{\link[lavaan:lavaan]{lavaan::lavaan()}} or its wrappers, such as
\code{\link[lavaan:cfa]{lavaan::cfa()}} and \code{\link[lavaan:sem]{lavaan::sem()}}.}

\item{x}{A 'annotate_matrices'-class object. The output of
\code{\link[=annotate_matrices]{annotate_matrices()}}.}

\item{...}{Optional arguments. To be passed to the default print method.}
}
\value{
It returns an \code{annotate_matrices}-class object, which is a
list of model matrices, with elements annotated:
\itemize{
\item If a parameter is free, then it is represented by "lhs-op-rhs" according
to the parameter estimate data frame.
\item If a parameter is fixed but appears in the parameter table,
it is represented by "(lhs-op-rhs = x)", x the value it is
fixed to.
\item If a parameter is fixed to zero but not in the parameter table,
then it is represented by 0.
}
}
\description{
Label the elements of the model matrices in a lavaan model.
}
\details{
This function annotates the model matrices in a lavaan
class object. This function is not to be used in analysis. It is
a learning tool, for learners to understand the relation between
the model matrices and the model parameters.

It currently supports a single-level single-group model only.
}
\section{Methods (by generic)}{
\itemize{
\item \code{print(annotate_matrices)}: The print method of the output of \code{\link[=annotate_matrices]{annotate_matrices()}}

}}
\examples{

# Adapted from https://lavaan.ugent.be/tutorial/cfa.html

library(lavaan)
HS.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit_cfa <- cfa(HS.model,
               data = HolzingerSwineford1939)
annotate_matrices(fit_cfa)

}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
