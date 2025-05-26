# rolog

_Access SWI-Prolog from R. Access R from SWI-Prolog._

## Access SWI-Prolog from R

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

## Installation

R> install.packages("rolog")

R> `library(rolog)`

R> `once(call("check_installation"))`

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

## Examples

This is a hello(world).

R> `library(rolog)`

Run a query such as member(X, [1, 2, 3]) with 

R> `findall(call("member", expression(X), list(1L, 2L, 3L)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query 
returns bindings for X that satisfy member(X, [1, 2, 3]).

The second example builds the vignette with nice use cases in Section 4.

````
rmarkdown::render(system.file("vignettes", "rolog.Rmd", package="rolog"),
    output_file="rolog.html", output_dir=getwd())
````

You should find an HTML page in `rolog.html` of the current folder. Please note that it includes
equations with MathML, which some browsers do not render in a pretty way.

## Access R from SWI-Prolog

As the name says, rolog = R for Prolog. The R system is a programming language
for statistical computing and data visualization. It has been adopted in the 
fields of data mining, bioinformatics and data analysis.

This Prolog pack connects to an existing installation of R. rolog supports the
following installations of R, with decreasing priority:

* If the environment variable `R_HOME` is set, the respective installation is
  used.
* If R or R.exe is found on the PATH, that one is used.

## License

This Prolog pack is distributed under a BSD-2 simplified license (see the file LICENSE).

## Installation

R> install.packages("RInside")

?- pack_install(rolog).

?- use_module(library(rolog)).

## Examples

?- r_eval(rnorm(3), X).

?- r_call(print(rnorm(3))).
