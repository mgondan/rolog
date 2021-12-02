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
mathml = function(term)
{
  t = once(call("r2mathml", term, expression(X)))
  cat(paste(t$X, collapse=""))
}

## ---- results="asis"----------------------------------------------------------
term = call("pbinom", quote(k), quote(N), quote(p))
mathml(term)

k = 10
N = 22
p = 0.4
eval(term)

term = call("integrate", quote(sin), 0L, call("*", 2L, quote(pi)))
mathml(term)
eval(term)

## -----------------------------------------------------------------------------
# [interval].
consult(system.file(file.path("pl", "interval.pl"), package="rolog"))

# findall(1 ... 2 / -3 ... 3, Res).
x = call("...", 1, 2)
y = call("...", -3, 3)
findall(call("int", call("/", x, y), expression(Res)))

# t-ratio
D  = call("...", 5.7, 5.8)
mu = 4
s  = call("...", 3.8, 3.9)
N  = 24L
findall(call("int", call("/", call("-", D, mu), call("/", s, call("sqrt", N))),
						 expression(Res)))

