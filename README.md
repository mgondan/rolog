# rolog: Access SWI-Prolog from R

*Please note that this package is currently broken under Windows. I still have to learn the details of the CRAN build system.* 

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

This R package is distributed under a BSD-2 simplified license (see the file LICENSE). SWI-Prolog
is distributed under its own license (BSD-2, as well).

## Linux

1. Please install SWI-Prolog on your computer, see here: https://www.swi-prolog.org/build/PPA.html

2. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc
   to render Rmd files to html (see Example 2).

`sudo apt install r-base pandoc pandoc-citeproc`

`R`

`install.packages("Rcpp", "rmarkdown", "remotes")`

3. Please install the "rologlite" pack for R

`remotes::install_github("mgondan/rologlite", build_vignettes=TRUE)`

Then please move on to the examples.

## macOS

1. Please install SWI-Prolog on your computer, see here: https://www.swi-prolog.org/download/stable

2. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc 
   to render Rmd files to html (see Example 2).

`brew install r-base pandoc pandoc-citeproc`

`R`

`install.packages("Rcpp", "rmarkdown", "remotes")`

3. Please install the "rologlite" pack for R

`remotes::install_github("mgondan/rologlite", build_vignettes=TRUE)`

Please tell me if anything is misssing.

## Windows (R 4.1)

`rologlite` is a source package, so a few things need to be done before you get started.

1. Download and install a recent R from https://www.r-project.org/

2. Download and install a recent RStudio from https://www.rstudio.com/

3. Download RTools4.0 from https://cran.r-project.org/bin/windows/Rtools/rtools40.html and install it, preferably into c:\rtools40, which is the default.

4. The pacman repository of Rtools does not include git, so I installed git for Windows (Git - Downloading Package) and made it accessible to the path.

5. Invoke c:\rtools40\msys2.exe

6. A shell appears, type `pacman -Syuu`.

7. Repeat steps 5 and 6 one time.

8. Check if R is working: Either use R from the command line, or RGui or RStudio.

`/c/Program\ Files/R/R-4.1.2/bin/R`

R> `install.packages("Rcpp", "rmarkdown", "remotes")`

R> `remotes::install_github("mgondan/rologlite")`

9. Check the installation like this:

R> `library(rologlite)`

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

## Example 1

This is a hello(world).

R> `library(rologlite)`

Run a query such as member(X, [1, 2, 3]) with 

R> `findall(call("member", expression(X), list(1L, 2L, 3L)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query 
returns bindings for X that satisfy member(X, [1, 2, 3]).

## Example 2

The second example is the vignette with nice use cases in Section 4.

`rmarkdown::render(system.file("vignettes", "rologlite.Rmd", package="rolog"), output_file="rologlite.html", output_dir=getwd())`

You should find an HTML page in `rologlite.html` of the current folder. Note that it includes equations with MathML, which look
best in the Firefox browser.

# Why is it called rologlite?

I guess the first part is obvious, rolog. The suffix -lite is because the package only connects to prolog (and does not embed it).
