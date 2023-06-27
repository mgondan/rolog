# rolog: Access SWI-Prolog from R

As the name says, rolog = Prolog for R. The logic programming language Prolog was
invented in the 1970ies by Alain Colmerauer, mostly for the purpose of natural
language processing. Since then, logic programming has become an important driving
force in research on artificial intelligence, natural language processing, program
analysis, knowledge representation and theorem proving. 

This R package connects to an existing installation of SWI-Prolog.
SWI-Prolog (https://www.swi-prolog.org/) is an open-source implementation of the
logic programming language Prolog. SWI-Prolog targets developers of applications,
with many users in academia, research and industry. SWI-Prolog includes a large
number of libraries for "the real world", for example, a web server, encryption,
interfaces to C/C++ and other programming languages, as well as a development
environment and debugger.

rolog supports the following installations of SWI-Prolog, with decreasing priority:

* If the environment variable `SWI_HOME_DIR` is set, the respective installation is
  used.
* If swipl.exe is found on the PATH, that one is used.
* (Windows only): If SWI-Prolog is installed in the system, a respective entry is
  found in the registry.
* R package rswipl that is an embedded SWI-Prolog runtime.

## License

This R package is distributed under a BSD-2 simplified license (see the file LICENSE).

# Installation

## Linux

1. Please install SWI-Prolog on your computer, see
   here: https://www.swi-prolog.org/build/PPA.html. Alternatively, install
   the R package rswipl.

3. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc
   to render Rmd files to html (see Example 2).

`sudo apt install r-base pandoc pandoc-citeproc`

`R`

`install.packages("Rcpp", "rmarkdown", "remotes")`

optional: `install.packages("rswipl")`

3. Please install the "rolog" pack for R

`install.packages("rolog")`

Then please move on to the examples.

## macOS

1. Please install SWI-Prolog on your computer, see here: https://www.swi-prolog.org/download/stable
   Alternatively, install the R package rswipl.

2. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc 
   to render Rmd files to html (see Example 2).

`brew install r-base pandoc pandoc-citeproc`

`R`

`install.packages("Rcpp", "rmarkdown", "remotes")`

optional: `install.packages("rswipl")`

3. Please install the "rolog" pack for R

`install_packages("rolog")`

## Windows

1. Download and install a recent R from https://www.r-project.org/

2. Download and install a recent RStudio from https://www.rstudio.com/

3. Download and install a recent SWI-Prolog from https://www.swi-prolog.org/

optional: R> `install.packages("rswipl")`

install.packages("rolog")

R> `library(rolog)`

Does this message appear?

````
Welcome to SWI-Prolog (threaded, 64 bits, version 8.5.3-28-g85fd90216-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).
````

`once(call("check_installation"))`

Does this output appear?

````
................................................ not present
Warning: See http://www.swi-prolog.org/build/issues/tcmalloc.html
Warning: library(bdb) .......................... NOT FOUND
Warning: See http://www.swi-prolog.org/build/issues/bdb.html
Warning: library(jpl) .......................... NOT FOUND
Warning: See http://www.swi-prolog.org/build/issues/jpl.html
Warning: library(pce) .......................... NOT FOUND
Warning: See http://www.swi-prolog.org/build/issues/xpce.html
Warning: Found 4 issues.
list()
attr(,"query")
[1] "check_installation"
````

# Examples

## Example 1

This is a hello(world).

R> `library(rolog)`

Run a query such as member(X, [1, 2, 3]) with 

R> `findall(call("member", expression(X), list(1L, 2L, 3L)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query 
returns bindings for X that satisfy member(X, [1, 2, 3]).

## Example 2

The second example builds the vignette with nice use cases in Section 4.

`rmarkdown::render(system.file("vignettes", "rolog.Rmd", package="rolog"), output_file="rolog.html", output_dir=getwd())`

You should find an HTML page in `rolog.html` of the current folder. Note that it includes equations with MathML, which look
best in the Firefox browser.
