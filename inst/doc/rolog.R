## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(rolog)

# [family].
consult(system.file(file.path("pl", "family.pl"), package="rolog"))

# findall(ancestor(X, jim)).
findall(call("ancestor", expression(X), quote(jim)))

## -----------------------------------------------------------------------------
# member(X, [1, 2, Y]).
query(call("member", expression(X), list(1, 2, expression(Y))))

# first solution for X
submit()

# next solution
submit()

# next solution
submit()

# no more results
submit()

# warning
submit()

## -----------------------------------------------------------------------------
# [telescope].
consult(system.file(file.path("pl", "telescope.pl"), package="rolog"))

# findall(sentence(Tree, "john saw a man with a telescope")).
findall(call("sentence", expression(Tree), "john saw a man with a telescope"))

## -----------------------------------------------------------------------------
library(rolog)
consult(system.file(file.path("pl", "mathml.pl"), package="rolog"))

# R interface to prolog predicate r2mathml/2
mathml = function(term, simplified=TRUE)
{
	if(simplified)
		term = as.rolog(term)
	
  t = once(call("r2mathml", term, expression(X)))
  cat(paste(t$X, collapse=""))
}

## ---- results="asis"----------------------------------------------------------
term = quote(pbinom(k, N, p))
mathml(term)

k = 10
N = 22
p = 0.4
eval(term)

term = quote(integrate(sin, 0L, 2L*pi))
mathml(term)
eval(term)

## -----------------------------------------------------------------------------
# [interval].
consult(system.file(file.path("pl", "interval.pl"), package="rolog"))

# findall(1 ... 2 / -3 ... 3, Res).
query = quote(int(`...`(1, 2) / `...`(-3, 3), .Res))
findall(as.rolog(query))

# t-ratio
D  = quote(`...`(5.7, 5.8))
mu = 4
s  = quote(`...`(3.8, 3.9))
N  = 24L
tratio = call("/", call("-", D, mu), call("/", s, call("sqrt", N)))
findall(call("int", tratio, expression(Res)))

