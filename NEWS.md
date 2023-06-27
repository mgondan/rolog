# rolog 0.9.14

* Maintainance release: more informative error message if SWI-Prolog is missing

# rolog 0.9.13

* Maintainance release: compatible with static libswipl.a from R package rswipl

# rolog 0.9.12

* represent vectors as double hash, dollar, !, %
* matrices triple hash, dollar, !, %
* compatible with R-4.3

# rolog 0.9.11

* Maintainance release: fix problems with exception handling

# rolog 0.9.10

* Support for R environments (`r_eval`)
* Backward compatible with swipl 8.4.2

# rolog 0.9.9

* Support for formulae (convert to call)
* LinkingTo: rswipl

# rolog 0.9.8

* Support for matrices
* Support for exceptions

# rolog 0.9.7

* Represent R functions as ':-'/2 in Prolog

# rolog 0.9.6

* Separate SWI-Prolog runtime in R package rswipl
* Connect to installed SWI-Prolog (Windows registry, `PATH`, `SWI_HOME_DIR`)

# rolog 0.9.5

* skipped. Will use updated C++ interface at a later stage.

# rolog 0.9.4

* Added a vignette with a manuscript for JSS
* Patch on swipl to suppress a deprecation warning under macOS (vfork)

# rolog 0.9.3

* Added a `NEWS.md` file to track changes to the package.
* Temporarily remove diagrams from the package vignette because DiagrammeR is currently not available in r-devel.
* Slightly faster build on Windows
