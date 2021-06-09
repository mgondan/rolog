.onLoad = function(libname, pkgname)
{
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  path = paste(libname, sep=.Platform$file.sep, pkgname)
  lib = list.files(path=path, pattern=name, recursive=TRUE)
  write(name)
  write(path)
  write(lib)
  stop("111", name, path, lib)
  if(length(lib) == 0)
    stop("Unable to find shared library", libname, pkgname, name, path)

  assign('m_sharedlib', lib[1], envir = topenv())
  dyn.load(m_sharedlib, local=FALSE, TRUE)
  rolog_init(libname, pkgname, commandArgs()[1])
}

rolog_init = function(libname, pkgname, argv1)
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
  dyn.unload(m_sharedlib)
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
