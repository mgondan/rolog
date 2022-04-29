# rolog: Access SWI-Prolog from R

The logic programming language Prolog was invented in the 1970ies by Alain
Colmerauer, mostly for the purpose of natural language processing. Since then,
logic programming has become an important driving force in research on artificial
intelligence, natural language processing, program analysis, knowledge
representation and theorem proving. 

SWI-Prolog (swipl, https://www.swi-prolog.org/) is an open-source implementation
of the logic programming language Prolog. Swipl targets developers of applications, 
with many users in academia, research and industry. SWI-Prolog includes a large number 
of libraries for "the real world", for example, a web server, encryption, interfaces 
to C/C++ and other programming languages, as well as a development environment and 
debugger.

This R package embeds swipl in a package for the R programming language (www.r-project.org).

## License

This R package is distributed under FreeBSD simplified license. SWI-Prolog is 
distributed under its own license (BSD-2).

## Installation

Please use R version >= 4.2. The package is on CRAN, it can be installed using 
`install.packages("rolog")` from the R environment. The current sources can be
installed using 

`install.packages('rmarkdown')`

`install.packages('remotes')`

`remotes::install_github('mgondan/rolog', build_vignettes=TRUE)`

This takes about 20 min on my computer. Then please move on to the examples. Please note that
under Windows, you need the RTools42 build system. 

## Example 1

This is a hello(world).

R> `library(rolog)`

Run a query such as member(X, [1, 2, 3]) with 

R> `findall(call("member", expression(X), list(1L, 2L, 3L)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query 
returns bindings for X that satisfy member(X, [1, 2, 3]).

## Example 2

The second example is the vignette with nice use cases in Section 4.

`rmarkdown::render(system.file("vignettes", "rolog.Rmd", package="rolog"), output_file="rolog.html", output_dir=getwd())`

You should find an HTML page in `rolog.html` of the current folder. Note that it includes equations with MathML, which look
best in the Firefox browser.
