.onLoad = function(libname, pkgname)
{
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  print(name)
  path = paste(libname, sep=.Platform$file.sep, pkgname)
  print(path)
  recursive = TRUE
  if(.Platform$r_arch != '')
  {
    arch = list.files(path=path, pattern=.Platform$r_arch, recursive=TRUE, include.dirs=TRUE)
    print(arch)
    if(length(arch) > 0)
    {
      path = paste(path, sep=.Platform$file.sep, arch)
      print(path)
      recursive = FALSE 
    }
  }
  lib = list.files(path=path, pattern=name, recursive=recursive)
  print(lib)
  if(length(lib) == 0)
    stop("Unable to find shared library", libname, pkgname, name, path)
  
  full = paste(path, sep=.Platform$file.sep, lib[1])
  print(full)
  dyn.load(full, local=FALSE, TRUE)
  rolog_init(libname, pkgname, commandArgs()[1])
}

.onUnload = function(libpath)
{
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  path = libpath
  recursive = TRUE
  if(.Platform$r_arch != '')
  {
    arch = list.files(path=path, pattern=.Platform$r_arch, recursive=TRUE, include.dirs=TRUE)
    if(length(arch) > 0)
    {
      path = paste(path, sep=.Platform$file.sep, arch)
      recursive = FALSE 
    }
  }
  lib = list.files(path=path, pattern=name, recursive=recursive)
  if(length(lib) == 0)
    stop("Unable to find shared library", libpath, " ", name)
  
  full = paste(libpath, sep=.Platform$file.sep, lib[1])
  dyn.unload(full)
}

rolog_init = function(libname, pkgname, argv1)
{
  init_(argv1)
}

rolog_done = function()
{
  done_()
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
