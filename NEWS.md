# rolog 0.9.25

* run swipl's unit tests during R CMD check
* maintainance release, compatible with swipl >= 10.1.2

# rolog 0.9.24

* add mutex to prevent simultaneous calls to R

# rolog 0.9.23

* as.rolog evaluates symbols in (a), not in (a + 1)

# rolog 0.9.22

* Prolog pack now running on MSYS2 (requires R in PATH and R\_HOME)

# rolog 0.9.21

* Migrate back to cpp2
* fix https://github.com/mgondan/rolog/issues/10
* Avoids the use of non-API calls BODY, FORMALS.

# rolog 0.9.20

* Bidirectional support: Access SWI-Prolog from R and vice-versa.
* Changed license to BSD-2

# rolog 0.9.19

* Maintainance release: fixes problems reported by UBSAN

# rolog 0.9.18

* bugfix: PL-get-atom-chars
* workaround for Rcpp::Language

# rolog 0.9.17

* Maintainance release: improve behavior with parallel make

# rolog 0.9.16

* Maintainance release: improve detection of swi-prolog at runtime

# rolog 0.9.15

* Migrate to C functions (prepare wrapper library for rswipl)

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
