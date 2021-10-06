# rolog
Access SWI-Prolog from R

## Installation (Linux)

1. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc to render Rmd files to html (see Example 2). Please use R version 4.1.

`sudo apt install r-base pandoc`

`R`

`install.packages('Rcpp')`

`install.packages('rmarkdown')`

`install.packages('remotes')`

3. Please install the "rolog" pack for R

`remotes::install_github('mgondan/rolog')`

This takes about 30 min on my computer. Then please move on to the examples.

## Installation instructions (Windows)

`rolog` is a source package, so a few things need to be done before you get started.

1. Please install the new RTools 4.0 from https://cran.r-project.org/bin/windows/Rtools/. On that page they explain how to make `gcc` and `make` accessible to R and RStudio. I think this is not sufficient. You have to add something like `c:\rtools40\mingw64\bin` and `c:\rtools40\usr\bin` to the system `PATH`, e.g. by right-clicking on My Computer in the Windows explorer and then "Properties", "Extended system settings" and "Environment variables".

2. You also need cmake and a fee more Rtools packages: `pacman -S mingw-w64-x86_64-cmake` from the rtools shell

3. Please make sure you have a `git` client on your computer, the one recommended by RStudio is https://git-scm.com/download/win. As before, please allow the program to change the `PATH` so that `git.exe` is found.

4. Start R or RStudio install the librariy `Rcpp`.

`install.packages("Rcpp", type="source")`

In general, packages are installed below the user's documents folder, e.g., `C:\Users\username\Documents\R\win-library\4.1`. For Step 5, you might need to tell the R program the location of this folder, so please set the environment variable `R_LIBS_USER` accordingly.

5. Then 

`install.packages('remotes')`

`remotes::install_github("mgondan/rolog", INSTALL_opts="--no-multiarch")`

The --no-multiarch is needed to prevent R from compiling the 32-bit version of the package (which doesn't exist).

`library(rolog)`

## Example 1

This is a hello(world).

`library(rolog)`

Load some facts and rules with 

`consult(system.file("pl/likes.pl", package="rolog"))`

Run a query such as likes(sam, X) with 

`findall(call('likes', quote(sam), expression(X)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables. The query returns bindings for X that satisfy likes(sam, X).

## Example 2

The second example uses Prolog for rendering R expressions as MathML. This is a nice illustration of two-way communication between R and Prolog, because Prolog has to ask
back for the name of the integration variable (the x in dx).

`rmarkdown::render(system.file('pl/mathml.Rmd', package='rolog'), output_file="mathml.html", output_dir=getwd())`

You should find an HTML page with nice equations in the file `mathml.html` of the current folder. Note that it uses MathML, which yields best results with the Firefox browser.
