.onLoad = function(libname, pkgname)
{
  dyn.load(system.file(paste('libs', .Platform$file.sep, 'rolog', .Platform$dynlib.ext, sep=''), package='rolog'), local=FALSE, TRUE)
  rolog_init(libname, pkgname, commandArgs()[1])
}

rolog_init = function(libname, pkgname, argv1)
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
  dyn.unload(system.file(paste('libs', .Platform$file.sep, 'rolog', .Platform$dynlib.ext, sep=''), package='rolog'))
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
