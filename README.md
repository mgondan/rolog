# rolog
Access SWI-Prolog from R

## Installation (Linux)

1. You obviously need R and a few packages on your computer, maybe also rmarkdown and pandoc to render Rmd files to html (see Example 2). Please use R version 4.1.

`sudo apt install r-base pandoc`

`R`

`install.packages('Rcpp')`

`install.packages('RInside')`

`install.packages('rmarkdown')`

`install.packages('remotes')`

2. Please install SWI-Prolog from https://www.swi-prolog.org. You may wish to use the development version since this offers the new Picat-style syntax which is used in Example 2 below. Here's a way to install the current development version:

`git clone https://github.com/SWI-Prolog/swipl-devel`

`cd swipl-devel`

`git submodule update --init`

`mkdir build`

`cd build`

`cmake ..`

`make`

`sudo make install`

More detailed installation instructions are found on the SWI-Prolog webpage.

3. Please install the "rologpp" pack for SWI-prolog. This is needed for the reverse direction (e.g. Prolog asking R the name of a specific function argument).

`swipl`

`pack_install(rologpp).`

`halt.`

4. Please install the "rolog" pack for R

`R`

`remotes::install_github('mgondan/rolog')`

Then move on to the examples.

## Installation instructions (Windows)

`rolog` is a source package, so a few things need to be done before you get started.

1. Please install SWI-Prolog from https://www.swi-prolog.org. You may wish to use the development version since this offers the new Picat-style syntax which is used in Example 2 below. In the last dialog, the Prolog installer asks if swipl should be in the `PATH`, please respond with yes.

2. Please install the new RTools 4.0 from https://cran.r-project.org/bin/windows/Rtools/. On that page they explain how to make `gcc` and `make` accessible to R and RStudio. I think this is not sufficient. You have to add something like `c:\rtools40\mingw64\bin` and `c:\rtools40\usr\bin` to the system `PATH`, e.g. by right-clicking on My Computer in the Windows explorer and then "Properties", "Extended system settings" and "Environment variables".

3. Please make sure you have a `git` client on your computer, the one recommended by RStudio is https://git-scm.com/download/win. As before, please allow the program to change the `PATH` so that `git.exe` is found.

4. Start R or RStudio install the libraries `Rcpp`, `RInside` (please recompile it by setting type="source").

`install.packages("Rcpp")`

`install.packages("RInside", type="source")`

In general, packages are installed below the user's documents folder, e.g., `C:\Users\username\Documents\R\win-library\4.1`. For Step 5, you might need to tell the R program the location of this folder, so please set the environment variable `R_LIBS_USER` accordingly.

5. Start SWI-Prolog and install the `rologpp` library. This one enables invoking R from Prolog, i.e., the other way round.

`assert(prolog_pack:environment('R_LIBS_USER', X) :- getenv('R_LIBS_USER', X)).` (This is needed to make R aware of the R packages installed at the user's home directory, see last step)

`pack_install(rologpp).`

You might want to run some tests, e.g.,

`use_module(library(rologpp)).` (should just say true)

`r_init.` (should just say true)

`r_eval(methods::formalArgs(args(sin)), X).` (should respond `X = #("x").`)

6. Restart/Open R or RStudio as a normal user, then 

`install.packages('remotes')`

`remotes::install_github("mgondan/rolog", INSTALL_opts="--no-multiarch")`

The --no-multiarch is needed to prevent R from compiling the 32-bit version of the package (which doesn't exist).

`library(rolog)`

If you run into problems, check if swipl.exe is visible from RStudio using `Sys.which('swipl')`, and set the PATH within RStudio if needed. Moreover, you might need to set the environment variable SWI_HOME_DIR to the installation folder of SWI-Prolog.

## Example 1

This is a hello(world).

`library(rolog)`

Load some facts and rules with 

`consult(system.file("likes.pl", package="rolog"))`

Run a query such as likes(sam, X) with 

`findall(call('likes', quote(sam), expression(X)))`

Sorry for the cumbersome syntax. At the moment, expression(X) encapsulates variables.

## Example 2

The second example uses Prolog for rendering R expressions as MathML. This is a nice illustration of two-way communication between R and Prolog, because Prolog has to ask
back for the name of the integration variable (the x in dx).

`rmarkdown::render(system.file('mathml.Rmd', package='rolog'), output_file="mathml.html", output_dir=getwd())`

You should find an HTML page with nice equations in the file `mathml.html` of the current folder. Note that it uses MathML, which yields best results with the Firefox browser.
