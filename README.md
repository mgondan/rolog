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

1. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc to render Rmd files to html (see Example 2). Please use R version 4.1.

`sudo apt install r-base pandoc pandoc-citeproc`

`R`

`install.packages('Rcpp')`

`install.packages('rmarkdown')`

`install.packages('remotes')`

2. Please install the "rolog" pack for R

`remotes::install_github('mgondan/rolog', build_vignettes=TRUE)`

This takes about 20 min on my computer. Then please move on to the examples.

## macOS

1. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc to render Rmd files to 
   html (see Example 2). Please use R version 4.1.

`brew install r-base pandoc pandoc-citeproc cmake`

`R`

`install.packages('Rcpp')`

`install.packages('rmarkdown')`

`install.packages('remotes')`

2. Please install the "rolog" pack for R

`remotes::install_github('mgondan/rolog', build_vignettes=TRUE)`

Please tell me if anything is misssing.

## Windows (R 4.1)

`rolog` is a source package, so a few things need to be done before you get started.

1. Download and install R-4.1.2 https://www.r-project.org/ (Please do _not_ install the files for 32 bit-support)

2. Download and install a recent RStudio from https://www.rstudio.com/

3. Download RTools4.0 from https://cran.r-project.org/bin/windows/Rtools/rtools40.html and install it, preferably into c:\rtools40, which is the default.

4. The pacman repository of Rtools does not include git, so I installed git for Windows (Git - Downloading Package) and made it accessible to the path.

5. Invoke c:\rtools40\mingw64.exe

6. A shell appears, type `pacman -Syu`

7. `pacman -S mingw-w64-x86_64-cmake mingw-w64-x86_64-make mingw-w64-x86_64-libjpeg mingw-w64-x86_64-libyaml mingw-w64-x86_64-pcre mingw-w64-x86_64-libarchive`

8. Check if R is working: Either use R from the command line, or RGui or RStudio.

`/c/Program\ Files/R/R-4.1.2/bin/R`

R> `install.packages("remotes")`

R> `install.packages("rmarkdown")`

R> `remotes::install_github("mgondan/rolog")`

8. Check the installation like this:

`library(rolog)`

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

The four issues are expected, since tcmalloc, bdb, jpl and xpce are not part of this installation.

## Windows (R-devel)

`rolog` is a source package, so a few things need to be done before you get started.

1. Download and install a recent R-devel from https://cran.r-project.org/bin/windows/base/rdevel.html

2. Download and install a recent RStudio that supports "ucrt".

3. Download RTools4.2 from https://cran.r-project.org/bin/windows/Rtools/ and install it, preferably into c:\rtools42, which is 
   the default.

4. The pacman repository of Rtools does not include git, so I installed git for Windows (Git - Downloading Package) and made it accessible to the path.

5. Invoke c:\rtools42\ucrt64.exe

6. A shell appears, type `pacman -Syu`

7. `pacman -S mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-libjpeg mingw-w64-ucrt-x86_64-libyaml mingw-w64-ucrt-x86_64-pcre mingw-w64-ucrt-x86_64-libarchive mingw-w64-ucrt-x86_64-db mingw-w64-ucrt-x86_64-texlive-latex-recommended mingw-w64-ucrt-x86_64-texlive-fonts-extra mingw-w64-ucrt-x86_64-qpdf`

8. In RStudio, call:

`install.packages("remotes")`

`install.packages("rmarkdown")`

`remotes::install_github("mgondan/rolog", build_vignettes=TRUE)`

9. You can check the installation like this:

`library(rolog)`

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
Warning: library(jpl) .......................... NOT FOUND
Warning: See http://www.swi-prolog.org/build/issues/jpl.html
Warning: library(pce) .......................... NOT FOUND
Warning: See http://www.swi-prolog.org/build/issues/xpce.html
Warning: Found 3 issues.
list()
attr(,"query")
[1] "check_installation"
````

The three issues are expected, since tcmalloc, jpl and xpce are not part of this installation.

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
