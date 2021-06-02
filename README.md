# rolog
Access SWI-Prolog from R

## Installation instructions (Windows)

`rolog` is a source package, so a few things need to be done before you get started.

1. Please install SWI-Prolog from https://www.swi-prolog.org. You can use both the development version or the stable version. In the last dialog, the Prolog installer asks if swipl should be in the `PATH`, please respond with yes.

2. Please install the new RTools 4.0 from https://cran.r-project.org/bin/windows/Rtools/. On that page they explain how to make `gcc` and `make` accessible to R and RStudio. I think this is not sufficient. You have to add something like `c:\rtools40\mingw64\bin` and `c:\rtools40\usr\bin` to the system `PATH`, e.g. by right-clicking on My Computer in the Windows explorer and then "Administration" and "Environment variables".

3. Please make sure you have a `git` client on your computer, the one recommended by RStudio is https://git-scm.com/download/win. As before, please allow the program to change the `PATH` so that `git.exe` is found.

4. Start RStudio _as administrator_ and install the two libraries `Rcpp` and `RInside`. There are obviously better ways to do this, and I will update this step in the future.

`install.packages("Rcpp", lib=.Library)`

`install.packages("RInside", lib=.Library)`

5. Start SWI-Prolog and install the `rologpp` library. This one enables invoking R from Prolog, i.e., the other way round.

`pack_install(rologpp).`

6. Restart/Open R or RStudio to take the change in the `PATH` into effect, then 

`install.packages("devtools")` (if needed)

`library(devtools)`

`install_github("mgondan/rolog")`

For some reasons, this does not work, the system complains that `init_` is not found. Write me if you know how to fix this. Otherwise, move on to the next step.

7. In RStudio, File/New project... and then Version Control and Git. Then type in `https://github.com/mgondan/rolog` as the repository and choose a suitable directory on your own computer.

8. In RStudio again, choose Build/Install and restart. You should now see the Welcome message from SWI-Prolog. 

## Example 1

`library(rolog)`

Load some facts and rules with `rolog_consult(system.file("likes.pl", package="rolog"))`

Run a query such as `findall(X, likes(sam, X), List)` with `rolog_findall(quote(likes(sam)))`.

