# rolog
Access SWI-Prolog from R

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

1. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc to render Rmd files to html (see Example 2). Please use R version 4.1.

`brew install r-base pandoc pandoc-citeproc cmake`

`R`

`install.packages('Rcpp')`

`install.packages('rmarkdown')`

`install.packages('remotes')`

2. Please install the "rolog" pack for R

`remotes::install_github('mgondan/rolog', build_vignettes=TRUE)`

Please tell me if anything is misssing.

## Windows

`rolog` is a source package, so a few things need to be done before you get started.

1. Please make sure you have a `git` client on your computer, the one recommended by RStudio is https://git-scm.com/download/win. As
   before, please allow the program to change the `PATH` so that `git.exe` is found.

2. For the vignette, pandoc is needed. Please do not use the most recent version, as they have changed the way they handle references. Version 2.9.2.1 is found here,
   https://github.com/jgm/pandoc/releases/tag/2.9.2.1 I assume you install it into C:/Program Files/Pandoc, which is the default.

3. Download and install R-devel from https://www.r-project.org/nosvn/winutf8/ucrt3. Note that this is not yet compatible to RStudio, so please say hello again to the "blue R", RGui.exe.

4. Download and install RTools4.2 from the same directory, https://cran.r-project.org/bin/windows/Rtools/.

5. Invoke c:\rtools42\ucrt64.exe

6. A shell appears, type `pacman -Syu`

7. `pacman -S mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-libjpeg mingw-w64-ucrt-x86_64-libyaml mingw-w64-ucrt-x86_64-pcre mingw-w64-ucrt-x86_64-libarchive mingw-w64-ucrt-x86_64-db mingw-w64-ucrt-x86_64-texlive-latex-recommended mingw-w64-ucrt-x86_64-texlive-fonts-extra`

8. Invoke `/c/Program\ Files/R/R-devel/bin/R.exe`

In R, call:

`install.packages("Rcpp", type="source")`

`install.packages("remotes", type="source")`

`install.packages("rmarkdown", type="source")`

`install.packages("devtools", type="source")`

`Sys.setenv(RSTUDIO_PANDOC='c:/program files/pandoc')`

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

I can't tell why the upper part of the output is cropped, but it looks good. The three issues are expected, since tcmalloc, jpl and xpce are not part of this installation.

10. Reinstall your current R, preferably R-4.1, so that RStudio is working again. You
   can now use library(rolog). It issues a warning with a version mismatch, this warning 
   will disappear if R-4.2 is available.

## Example 1

This is a hello(world).

`library(rolog)`

Run a query such as member(X, [1, 2, 3]) with 

`findall(call('member', expression(X), list(1L, 2L, 3L)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query returns bindings for X that satisfy member(X, [1, 2, 3]).

## Example 2

The second example uses Prolog for rendering R expressions as MathML. This is a nice illustration of two-way communication between R and Prolog,
because Prolog has to ask back for the name of the integration variable (the x in dx).

`rmarkdown::render(system.file('pl/mathml.Rmd', package='rolog'), output_file="mathml.html", output_dir=getwd())`

You should find an HTML page with nice equations in the file `mathml.html` of the current folder. Note that it uses MathML, which yields best
results with the Firefox browser.

## More examples

Please have a look at the vignette of the package.
