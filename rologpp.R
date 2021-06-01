.onLoad = function(libname, pkgname)
{
	rologpp_init(libname, pkgname, commandArgs()[1])
}

rologpp_init = function(libname, pkgname, argv1)
{
	init(commandArgs()[1])
}

rologpp_done = function()
{
  done()
}

rologpp_consult = function(fname='pl/likes.pl')
{
  consult(fname)
}

rologpp_call = function(call = quote(consult('pl/likes')))
{
  call(call)
}

rologpp_findall = function(predicate=quote(likes(sam)))
{
  l = as.list(findall(predicate))
  l[length(l):1]
}
