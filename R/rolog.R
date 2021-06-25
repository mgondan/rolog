.onLoad = function(libname, pkgname)
{
  name = paste('rolog', .Platform$dynlib.ext, sep='')
  print(c(name=name))
  path = paste(libname, sep=.Platform$file.sep, pkgname)
  print(c(path=path))
  recursive = TRUE
  if(.Platform$r_arch != '')
  {
    arch = list.files(path=path, pattern=.Platform$r_arch, recursive=TRUE, include.dirs=TRUE)
    print(c(arch=arch))
    if(length(arch) > 0)
    {
      path = paste(path, sep=.Platform$file.sep, arch)
      print(c(path=path))
      recursive = FALSE 
    }
  }
  lib = list.files(path=path, pattern=name, recursive=recursive)
  print(c(lib=lib))
  if(length(lib) == 0)
    stop("Unable to find shared library", libname, pkgname, name, path)
  
  full = paste(path, sep=.Platform$file.sep, lib[1])  
  print(c(full=full))
  r = dyn.load(full, local=FALSE, DLLpath=path)
  print(r)
  # rolog_init(libname, pkgname, gsub("Program Files", "PROGRA~1", commandArgs()[1]))
  # rolog_init(libname, pkgname, full)
}

.onUnload = function(libpath)
{
  print("Unloading rolog")
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

.onAttach = function(libname, pkgname)
{
  print("Attaching rolog")  
  if(rolog_init(libname, pkgname, commandArgs()[1]))
    return(TRUE) ;
  
  print("Try again")  
  rolog_init(libname, pkgname, commandArgs()[1])
}

.onDetach = function(libpath)
{
  print("Detaching rolog")
  
  if(!rolog_done())
    stop("rolog: not initialized")  
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
