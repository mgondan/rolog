% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portray.R
\name{portray}
\alias{portray}
\title{Translate an R call to a prolog compound and pretty print it}
\usage{
portray(
  query = call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))),
  options = NULL
)
}
\arguments{
\item{query}{an R call. The R call consists of symbols, integers and real numbers,
character strings, boolean values, expressions and lists, and other calls.
Vectors of booleans, integers, floating point numbers, and strings with
length \emph{N} > 1 are translated to prolog compounds !/N, \%/N, #/N and $$/N,
respectively. The names can be modified with the options below.}

\item{options}{This is a list of options controlling translation from and to prolog.
\itemize{
\item \emph{boolvec} (see option \code{rolog.boolvec}, default is !) is the name of the
prolog compound for vectors of booleans.
\item \emph{intvec}, \emph{realvec}, \emph{charvec} define the compound names for vectors of
integers, doubles and strings, respectively (defaults are \%, # and $$).
\item If \emph{scalar} is \code{TRUE} (default), vectors of length 1 are translated to
scalar prolog elements. If \emph{scalar} is \code{FALSE}, vectors of length 1 are
also translated to compounds.
}}
}
\value{
character string with the prolog syntax of the call
}
\description{
Translate an R call to a prolog compound and pretty print it
}
\details{
The R elements are translated to the following prolog citizens:
\itemize{
\item numeric -> real (vectors of size \emph{N} -> #/N)
\item integer -> integer (vectors -> \%/N)
\item character -> string (vectors -> $$/N)
\item symbol/name -> atom
\item expression -> variable
\item call/language -> compound
\item boolean -> true, false (atoms)
\item list -> list
}
}
\seealso{
\code{\link[=rolog_options]{rolog_options()}} for fine-grained control over the translation
}
