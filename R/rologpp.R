.onLoad = function(libname, pkgname)
{
	rolog_init(libname, pkgname, commandArgs()[1])
}

rolog_init = function(libname, pkgname, argv1)
{
	init(argv1)
}

rolog_done = function()
{
  done()
}

rolog_consult = function(fname='pl/likes.pl')
{
  consult(fname)
}

rolog_call = function(call = quote(consult('pl/likes')))
{
  call(call)
}

rolog_findall = function(predicate=quote(likes(sam)))
{
  l = as.list(findall(predicate))
  l[length(l):1]
}
