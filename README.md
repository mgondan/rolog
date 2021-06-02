# rolog
Access SWI-Prolog from R

## Installation instructions (Windows)

`rolog` is a source package, so a few things need to be done before you get started.

1. Please install SWI-Prolog from https://www.swi-prolog.org. You may wish to use the development version since this offers the new Picat-style syntax which is used in Example 2 below. In the last dialog, the Prolog installer asks if swipl should be in the `PATH`, please respond with yes.

2. Please install the new RTools 4.0 from https://cran.r-project.org/bin/windows/Rtools/. On that page they explain how to make `gcc` and `make` accessible to R and RStudio. I think this is not sufficient. You have to add something like `c:\rtools40\mingw64\bin` and `c:\rtools40\usr\bin` to the system `PATH`, e.g. by right-clicking on My Computer in the Windows explorer and then "Administration" and "Environment variables".

3. Please make sure you have a `git` client on your computer, the one recommended by RStudio is https://git-scm.com/download/win. As before, please allow the program to change the `PATH` so that `git.exe` is found.

4. Start RStudio and install the libraries `Rcpp`, `RInside`, and `devtools`.

`install.packages("Rcpp")`

`install.packages("RInside")`

`install.packages("devtools")`

5. Start SWI-Prolog and install the `rologpp` library. This one enables invoking R from Prolog, i.e., the other way round.

`pack_install(rologpp).`

You might want to run some tests, e.g.,

`use_module(library(rologpp)).` (should just say true*)

`r_init.` (should just say true)

`r_eval(methods::formalArgs(args(sin)), X).` (should respond `X = #("x").`)

6. Restart/Open R or RStudio, then 

`library(devtools)`

`install_github("mgondan/rolog")`

For some reasons, this does not work, the system complains that `init_` is not found. Write me if you know how to fix this. Otherwise, move on to the next step.

7. In RStudio, File/New project... and then Version Control and Git. Then type in `https://github.com/mgondan/rolog` as the repository and choose a suitable directory on your own computer.

8. In RStudio again, choose Build/Install and restart. You should now see the Welcome message from SWI-Prolog. 

## Example 1

This is a hello(world).

`library(rolog)`

Load some facts and rules with `rolog_consult(system.file("likes.pl", package="rolog"))`

Run a query such as `findall(X, likes(sam, X), List)` with `rolog_findall(quote(likes(sam)))`.

## Example 2

`library(rolog)`

Open the mathml.Rmd example, you find it in `system.file("mathml.Rmd", package="rolog"))`

Knit the file for HTML output. The example uses Prolog for rendering R expressions as MathML.
