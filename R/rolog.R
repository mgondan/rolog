.onLoad = function(libname, pkgname)
{
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  path = list.files(pattern=name, path=system.file(package="rolog"), recursive=TRUE)[1]
  dyn.load(path, local=FALSE, TRUE)
  rolog_init(libname, pkgname, commandArgs()[1])
}

rolog_init = function(libname, pkgname, argv1)
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  path = list.files(pattern=name, path=system.file(package="rolog"), recursive=TRUE)[1]
  dyn.unload(path)
}

rolog_consult = function(fname='likes.pl')
{
  consult_(fname)
}

rolog_call = function(call = quote(consult('likes')))
{
  call_(call)
}

rolog_findall = function(predicate=quote(likes(sam)))
{
  l = as.list(findall_(predicate))
  l[length(l):1]
}
