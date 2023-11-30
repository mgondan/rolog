// #include <SWI-cpp.h>
#include <SWI-Prolog.h>
#include "Rcpp.h"
using namespace Rcpp ;

// Translate prolog expression to R
//
// [] -> NULL
// real -> NumericVector
// #(r1, r2, r3) -> NumericVector (# is a default, see option realvec)
// ##(#(row1), #(row2), ...) -> Matrix
// integer -> IntegerVector
// %(i1, i2, i3) -> IntegerVector (see option intvec for the name)
// %%(%(row1), %(row2), ...) -> Matrix
// string -> CharacterVector
// $$(s1, s2, s3) CharacterVector
// $$$($$(row1), $$(row2), ...) -> Matrix
// na (atom) -> NA
// true, false (atoms) -> LogicalVector
// !(l1, l2, l3) -> LogicalVector (see option boolvec)
// !!(!(row1), !(row2), ...) -> Matrix
// the empty atom -> ""
// other atoms -> symbol/name
// variable -> expression(variable name)
// compound -> call (aka. "language")
// list -> list
//
RObject pl2r(term_t pl, CharacterVector& names, term_t& vars, List options) ;

// Translate R expression to prolog
//
// NULL -> []
// numeric vector of length 1 -> real (unless rolog.scalar == FALSE)
// numeric vector of length > 1 -> e.g., #(1.0, 2.0, 3.0) (see rolog.realvec)
// integer vector of length 1 -> integer
// integer vector of length > 1 -> %(1, 2, 3)
// character vector of length 1 -> string
// character vector of length > 1 -> $("a", "b", "c")
// logical vector of length 1 -> the atoms true, false or na
// logical vector of length > 1 -> $(true, false, na)
// other symbols/name -> atom
// expression -> variable
// call/language -> compound
// list -> list
//
term_t r2pl(SEXP r, CharacterVector& names, term_t& vars, List options) ;

// Consult one or more files. If something fails, the procedure stops, and
// will not try to consult the remaining files.
//
// [[Rcpp::export(.consult)]]
LogicalVector consult_(CharacterVector files)
{
  for(R_xlen_t i=0; i<files.size(); i++)
  {
    term_t fname = PL_new_term_ref() ;
    PL_put_atom_chars(fname, files(i)) ;

    predicate_t p = PL_predicate("consult", 1, NULL) ;
    if(!PL_call_predicate(NULL, PL_Q_NORMAL, p, fname))
      stop("failed to consult %s", (char*) files(i)) ;
  }

  return true ;
}

// Prolog -> R
RObject pl2r_null()
{
  return R_NilValue ;
}

// This helper function checks for na and then translates an individual PlTerm 
// to a double.
double pl2r_double(term_t pl)
{
  if(PL_is_atom(pl) && !strcmp("na", PL_atom_nchars(pl, NULL)))
    return NA_REAL ;

  double f ;
  if(PL_get_float(pl, &f))
    return f ;

  char* s ;
  if(PL_get_chars(pl, &s, CVT_ALL|BUF_STACK|REP_UTF8))
    warning("cannot convert %s to float", s) ;
  else
    warning("cannot convert to float") ;

  return NA_REAL ;
}

// Convert scalar real to DoubleVector of length 1
DoubleVector pl2r_real(term_t pl)
{
  return DoubleVector::create(pl2r_double(pl)) ;
}

// Convert vector of reals (e.g., #(1.0, 2.0, na)) to DoubleVector
DoubleVector pl2r_realvec(term_t pl)
{
  size_t arity ;
  if(!PL_get_name_arity(pl, NULL, &arity))
    stop("pl2r: cannot convert realvec") ;

  DoubleVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    if(!(arg = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, arg))
      stop("pl2r: cannot convert realvec (argument %ld)", i + 1) ;

    r(i) = pl2r_double(arg) ;
  }

  return r ;
}

// Convert matrix of reals (e.g., ##(#(1.0, 2.0), #(na, ...), ...))
NumericMatrix pl2r_realmat(term_t pl)
{
  size_t nrow ;
  if(!PL_get_name_arity(pl, NULL, &nrow))
    stop("pl2r: cannot convert realmat") ;

  size_t ncol = 0 ;
  if(nrow > 0)
    for(size_t i=0; i<nrow; i++)
    {
      term_t row ;
      if(!(row = PL_new_term_ref())
          || !PL_get_arg(i + 1, pl, row))
        stop("pl2r: Cannot convert realmat") ;

      if(i == 0)
      {
        if(!PL_get_name_arity(row, NULL, &ncol))
          stop("pl2r: cannot convert realmat") ;
      }
      else
      {
        size_t col ;
        if(!PL_get_name_arity(row, NULL, &col))
          stop("pl2r: cannot convert realmat") ;

        if(col != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
    }

  NumericMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    if(!(row = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, row))
      stop("pl2r: cannot convert realmat") ;

    r.row(i) = pl2r_realvec(row) ;
  }

  return r ;
}

// See above for pl2r_double
long pl2r_int(term_t pl)
{
  if(PL_is_atom(pl) && !strcmp("na", PL_atom_nchars(pl, NULL)))
    return NA_INTEGER ;

  long f ;
  if(PL_get_long(pl, &f))
    return f ;

  char* s ;
  if(PL_get_chars(pl, &s, CVT_ALL|BUF_STACK|REP_UTF8))
    warning("cannot convert %s to integer", s) ;
  else
    warning("cannot convert to integer") ;

  return NA_INTEGER ;
}

IntegerVector pl2r_integer(term_t pl)
{
  return IntegerVector::create(pl2r_int(pl)) ;
}

IntegerVector pl2r_intvec(term_t pl)
{
  size_t arity ;
  if(!PL_get_name_arity(pl, NULL, &arity))
    stop("pl2r: cannot convert intvec") ;

  IntegerVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    if(!(arg = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, arg))
      stop("pl2r: cannot convert intvec") ;

    r(i) = pl2r_int(arg) ;
  }

  return r ;
}

IntegerMatrix pl2r_intmat(term_t pl)
{
  size_t nrow ;
  if(!PL_get_name_arity(pl, NULL, &nrow))
    stop("pl2r: cannot convert intmat") ;

  size_t ncol = 0 ;
  if(nrow > 0)
    for(size_t i=0; i<nrow; i++)
    {
      term_t row ;
      if(!(row = PL_new_term_ref())
          || !PL_get_arg(i + 1, pl, row))
        stop("pl2r: Cannot convert intmat") ;

      if(i == 0)
      {
        if(!PL_get_name_arity(row, NULL, &ncol))
          stop("pl2r: cannot convert realmat") ;
      }
      else
      {
        size_t col ;
        if(!PL_get_name_arity(row, NULL, &col))
          stop("pl2r: cannot convert intmat") ;

        if(col != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
    }

  IntegerMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    if(!(row = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, row))
      stop("pl2r: cannot convert intmat") ;

    r.row(i) = pl2r_intvec(row) ;
  }

  return r ;
}

// See above for pl2r_double
String pl2r_string(term_t pl)
{
  if(PL_is_atom(pl) && !strcmp("na", PL_atom_nchars(pl, NULL)))
    return NA_STRING ;
 
  char* s ;
  if(PL_get_chars(pl, &s, CVT_ALL|BUF_STACK|REP_UTF8))
    return String(s) ;

  warning("cannot convert to string") ;
  return NA_STRING ;
}

CharacterVector pl2r_char(term_t pl)
{
  return CharacterVector::create(pl2r_string(pl)) ;
}

CharacterVector pl2r_charvec(term_t pl)
{
  size_t arity ;
  if(!PL_get_name_arity(pl, NULL, &arity))
    stop("pl2r: cannot convert charvec") ;

  CharacterVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    if(!(arg = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, arg))
      stop("pl2r: cannot convert charvec") ;

    r(i) = pl2r_string(arg) ;
  }

  return r ;
}

CharacterMatrix pl2r_charmat(term_t pl)
{
  size_t nrow ;
  if(!PL_get_name_arity(pl, NULL, &nrow))
    stop("pl2r: cannot convert charmat") ;

  size_t ncol = 0 ;
  if(nrow > 0)
    for(size_t i=0; i<nrow; i++)
    {
      term_t row ;
      if(!(row = PL_new_term_ref())
          || !PL_get_arg(i + 1, pl, row))
        stop("pl2r: Cannot convert boolmat") ;

      if(i == 0)
      {
        if(!PL_get_name_arity(row, NULL, &ncol))
          stop("pl2r: cannot convert realmat") ;
      }
      else
      {
        size_t col ;
        if(!PL_get_name_arity(row, NULL, &col))
          stop("pl2r: cannot convert intmat") ;

        if(col != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
    }

  CharacterMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    if(!(row = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, row))
      stop("pl2r: cannot convert charmat") ;

    r.row(i) = pl2r_charvec(row) ;
  }

  return r ;
}

// Convert prolog atom to R symbol (handle na, true, false)
RObject pl2r_symbol(term_t pl)
{
  char *s ;
  if(!PL_get_atom_chars(pl, &s))
    stop("pl2r: cannot create symbol") ;

  if(!strcmp(s, "na"))
    return wrap(NA_LOGICAL) ;
  
  if(!strcmp(s, "true"))
    return wrap(true) ;
  
  if(!strcmp(s, "false"))
    return wrap(false) ;

  // Empty symbols
  if(!strcmp(s, ""))
    return Function("substitute")() ;

  // general atoms
  return wrap(Symbol(s)) ;
}

// Forward declaration, needed below
RObject pl2r_compound(term_t pl, CharacterVector& names, term_t& vars, List options) ;

// Convert prolog neck to R function
RObject pl2r_function(term_t pl, CharacterVector& names, term_t& vars, List options)
{
  term_t plhead, plbody ;
  if(!(plhead = PL_new_term_ref())
      || !PL_get_arg(1, pl, plhead)
      || !(plbody = PL_new_term_ref())
      || !PL_get_arg(2, pl, plbody))
    stop("Cannot convert pl compound") ;

  Language head("alist") ;
  size_t arity ;
  if(!PL_get_name_arity(plhead, NULL, &arity))
    stop("Cannot convert pl compound") ;

  for(size_t i=1 ; i<=arity ; i++)
  {
    term_t arg ;
    if(!(arg = PL_new_term_ref())
        || !PL_get_arg(i, plhead, arg))
      stop("Cannot convert pl compound") ;

    // Compounds like mean=100 are translated to named function arguments
    term_t arg_name ;
    size_t arg_arity ;
    if(!(arg_name = PL_new_term_ref()))
      stop("Cannot convert pl compound") ;

    if(PL_is_compound(arg) && PL_get_name_arity(arg, &arg_name, &arg_arity)
       && !strcmp(PL_atom_nchars(arg_name, NULL), "=") && arg_arity == 2)
    {
      term_t arg1, arg2 ;
      if(!(arg1 = PL_new_term_ref())
          || !PL_get_arg(1, arg, arg1)
          || !(arg2 = PL_new_term_ref())
          || !PL_get_arg(2, arg, arg2))
        stop("Cannot convert pl compound") ;

      if(PL_is_atom(arg1))
      {
        head.push_back(Named(PL_atom_nchars(arg1, NULL)) = pl2r(arg2, names, vars, options)) ;
        continue ;
      }
    }

    // just the name, no argument like in alist(a=, b=)
    char* s ;
    if(!PL_get_atom_chars(arg, &s))
      stop("Cannot convert pl compound") ;

    head.push_back(Named(s) = Function("substitute")()) ;
  }

  RObject body = pl2r_compound(plbody, names, vars, options) ;
  head.push_back(body) ;

  Function as_function("as.function") ;
  return wrap(as_function(head)) ;
}

LogicalVector pl2r_boolvec(term_t pl)
{
  size_t arity ;
  if(!PL_get_name_arity(pl, NULL, &arity))
    stop("pl2r: cannot convert boolvec") ;

  LogicalVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t t ;
    if(!(t = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, t))
      stop("r2pl: cannot convert boolvec") ;

    if(PL_is_atom(t))
    {
      if(!strcmp(PL_atom_nchars(t, NULL), "na"))
      {
        r(i) = NA_LOGICAL ;
        continue ;
      }
      
      if(!strcmp(PL_atom_nchars(t, NULL), "true"))
      {
        r(i) = 1 ;
        continue ;
      }
      
      if(!strcmp(PL_atom_nchars(t, NULL), "false"))
      {
        r(i) = 0 ;
        continue ;
      }
    }
    
    warning("pl2r_logical: invalid item %s, returning NA", PL_atom_nchars(t, NULL)) ;
    r(i) = NA_LOGICAL ;
  }

  return r ;
}

LogicalMatrix pl2r_boolmat(term_t pl)
{
  size_t nrow ;
  if(!PL_get_name_arity(pl, NULL, &nrow))
    stop("pl2r: cannot convert charmat") ;

  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
    {
      term_t row ;
      if(!(row = PL_new_term_ref())
          || !PL_get_arg(i + 1, pl, row))
        stop("pl2r: Cannot convert boolmat") ;

      if(i == 0)
      {
        if(!PL_get_name_arity(row, NULL, &ncol))
          stop("pl2r: cannot convert realmat") ;
      }
      else
      {
        size_t col ;
        if(!PL_get_name_arity(row, NULL, &col))
          stop("pl2r: cannot convert intmat") ;

        if(col != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
    }
  }

  LogicalMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    if(!(row = PL_new_term_ref())
        || !PL_get_arg(i + 1, pl, row))
      stop("pl2r: Cannot convert boolmat") ;

    r.row(i) = pl2r_boolvec(row) ;
  }

  return r ;
}

// Translate prolog variables to R expressions.
RObject pl2r_variable(term_t pl, CharacterVector& names, term_t& vars)
{
  // names and vars is a list of all the variables from the R query,
  // a typical member of names is something like X, a member of vars 
  // is something like _1545.
  //
  // Search for the variable (e.g., _1545) in names and return its R name as an
  // expression (say, X).
  term_t head, tail ;
  if(!(head = PL_new_term_ref())
      || !(tail = PL_copy_term_ref(vars)))
    stop("pl2r: Cannot convert variable 1") ;

  for(int i=0 ; i<names.length() ; i++)
  {
    PL_get_list_ex(tail, head, tail) ;
    if(PL_compare(pl, head) == 0)
      return ExpressionVector::create(Symbol(names(i))) ;
  }

  // If the variable is not found, it's a new one created by Prolog, e.g., in
  // queries like member(1, Y), Y is unified with [1 | _NewVar ]. This variable
  // cannot be translated to a human-readable name, so it is returned as _1545.
  char* name ;
  if(!PL_get_chars(pl, &name, CVT_VARIABLE|BUF_DISCARDABLE|REP_UTF8))
    stop("pl2r: Cannot convert variable 2") ;

  return ExpressionVector::create(Symbol(name)) ;
}

// Translate prolog compound to R call
//
// This function takes care of special compound names (#, %, $, !) for vector
// objects in R, as well as "named" function arguments like "mean=100", in
// rnorm(10, mean=100, sd=15).
RObject pl2r_compound(term_t pl, CharacterVector& names, term_t& vars, List options)
{
  // This function does not (yet) work for cyclic terms
  if(!PL_is_acyclic(pl))
  {
    char* n ;
    if(PL_get_chars(pl, &n, CVT_ALL|BUF_DISCARDABLE|REP_UTF8))
      stop("pl2r: Cannot convert cyclic term %s", n) ;

    stop("pl2r: Cannot convert cyclic term") ;
  }

  // Convert ###(##(...), ...) to NumericMatrix
  term_t name ; 
  size_t arity ;
  if(!(name = PL_new_term_ref())
      || !PL_get_name_arity(pl, &name, &arity))
    stop("Cannot convert R call 1") ;

  if(!strcmp(PL_atom_nchars(name, NULL), options("realmat")))
    return pl2r_realmat(pl) ;

  // Convert ##(1.0, 2.0, 3.0) to DoubleVector (# given by options("realvec"))
  if(!strcmp(PL_atom_nchars(name, NULL), options("realvec")))
    return pl2r_realvec(pl) ;

  // Convert %%%(%%(...), ...) to IntegerMatrix
  if(!strcmp(PL_atom_nchars(name, NULL), options("intmat")))
    return pl2r_intmat(pl) ;

  // Convert %%(1.0, 2.0, 3.0) to IntegerVector
  if(!strcmp(PL_atom_nchars(name, NULL), options("intvec")))
    return pl2r_intvec(pl) ;

  // Convert $$$($$(...), ...) to StringMatrix
  if(!strcmp(PL_atom_nchars(name, NULL), options("charmat")))
    return pl2r_charmat(pl) ;

  // Convert $$(1.0, 2.0, 3.0) to CharacterVector
  if(!strcmp(PL_atom_nchars(name, NULL), options("charvec")))
    return pl2r_charvec(pl) ;

  // Convert !!!(!!(...), ...) to LogicalMatrix
  if(!strcmp(PL_atom_nchars(name, NULL), options("boolmat")))
    return pl2r_boolmat(pl) ;

  // Convert !!(1.0, 2.0, 3.0) to LogicalVector
  if(!strcmp(PL_atom_nchars(name, NULL), options("boolvec")))
    return pl2r_boolvec(pl) ;

  // Convert :- to function
  if(!strcmp(PL_atom_nchars(name, NULL), ":-"))
    return pl2r_function(pl, names, vars, options) ;

  // Other compounds
  Language r(PL_atom_nchars(name, NULL)) ;
  for(size_t i=1 ; i<=arity ; i++)
  {
    term_t arg ;
    if(!(arg = PL_new_term_ref())
        || !PL_get_arg(i, pl, arg))    
      stop("Cannot convert R call 2") ;

    // Compounds like mean=100 are translated to named function arguments
    if(PL_is_compound(arg))
    {
      term_t arg_name ;
      size_t arg_arity ;
      if(!(arg_name = PL_new_term_ref())
          || !PL_get_name_arity(arg, &arg_name, &arg_arity))
        stop("Cannot convert R call 3") ;

      if(!strcmp(PL_atom_nchars(arg_name, NULL), "=") && arg_arity == 2)
      {
        term_t arg1, arg2 ;
        if(!(arg1 = PL_new_term_ref())
            || !PL_get_arg(1, arg, arg1)
            || !(arg2 = PL_new_term_ref())
            || !PL_get_arg(2, arg, arg2))
          stop("Cannot convert R call 4") ;

        if(PL_is_atom(arg1))
        {
          term_t name ;
          if(!(name = PL_new_term_ref())
              || !PL_get_name_arity(arg1, &name, NULL))
            stop("Cannot convert R call 5") ;

          r.push_back(Named(PL_atom_nchars(name, NULL)) = pl2r(arg2, names, vars, options)) ;
          continue ;
        }
      }
    }

    // argument has no name
    r.push_back(pl2r(arg, names, vars, options)) ;
  }

  return as<RObject>(r) ;
}

// Translate prolog list to R list
//
// This code allows for lists like [1, 2 | Tail] with variable tail. These 
// cannot be processed by PlTerm_tail, therefore, the code is a bit more 
// complicated, also because it can handle named arguments.
//
// Examples:
// [1, 2, 3] -> list(1, 2, 3)
// [1, 2 | X] -> `[|]`(1, `[|]`(2, expression(X)))
// [a-1, b-2, c-3] -> list(a=1, b=2, c=3)
//
RObject pl2r_list(term_t pl, CharacterVector& names, term_t& vars, List options)
{
  term_t head ;
  if(!(head = PL_new_term_ref())
      || !PL_get_arg(1, pl, head))
    stop("Cannot convert list 1") ;

  // if the tail is a list or empty, return a normal list
  term_t tail ;
  if(!(tail = PL_new_term_ref())
      || !PL_get_arg(2, pl, tail))
    stop("Cannot convert list 2") ;

  RObject rest = pl2r(tail, names, vars, options) ;
  if(TYPEOF(rest) == VECSXP || TYPEOF(rest) == NILSXP)
  {
    List r = as<List>(rest) ;

    // convert prolog pair a-X to named list element
    if(PL_is_compound(head))
    {
      term_t head_name ;
      size_t head_arity ;
      if(!(head_name = PL_new_term_ref())
          || !PL_get_name_arity(head, &head_name, &head_arity))
        stop("Cannot convert list 3") ;

      if(!strcmp(PL_atom_nchars(head_name, NULL), "-") && head_arity == 2)
      {
        term_t arg1, arg2 ;
        if(!(arg1 = PL_new_term_ref())
            || !PL_get_arg(1, head, arg1)
            || !(arg2 = PL_new_term_ref())
            || !PL_get_arg(2, head, arg2))
          stop("Cannot convert list 4") ;

        if(PL_is_atom(arg1))
        {
          r.push_front(pl2r(arg2, names, vars, options), PL_atom_nchars(arg1, NULL)) ;
          return r ;
        }
      }
    }
    
    // element has no name
    r.push_front(pl2r(head, names, vars, options)) ; 
    return r ;
  }
    
  // if the tail is something else, return [|](head, tail)
  term_t name ;
  size_t arity ;
  if(!(name = PL_new_term_ref())
      || !PL_get_name_arity(pl, &name, &arity))
    stop("Cannot convert list 5") ;

  Language r(PL_atom_nchars(name, NULL)) ;
  // convert prolog pair a-X to named list element
  if(PL_is_compound(head) && !strcmp(PL_atom_nchars(name, NULL), "-") && arity == 2)
  {
    term_t arg1, arg2 ;
    if(!(arg1 = PL_new_term_ref())
        || !PL_get_arg(1, head, arg1)
        || !(arg2 = PL_new_term_ref())
        || !PL_get_arg(2, head, arg2))
      stop("Cannot convert list 6") ;

    if(PL_is_atom(arg1))
    {
      r.push_back(Named(PL_atom_nchars(arg1, NULL)) = pl2r(arg2, names, vars, options)) ;
      r.push_back(rest) ;
      return as<RObject>(r) ;
    }
  }

  // element has no name
  r.push_back(pl2r(head, names, vars, options)) ; 
  r.push_back(rest) ;
  return as<RObject>(r) ;
}

RObject pl2r(term_t pl, CharacterVector& names, term_t& vars, List options)
{
  if(PL_term_type(pl) == PL_NIL)
    return pl2r_null() ;

  if(PL_is_integer(pl))
    return pl2r_integer(pl) ;

  if(PL_is_float(pl))
    return pl2r_real(pl) ;

  if(PL_is_string(pl))
    return pl2r_char(pl) ;

  if(PL_is_atom(pl))
    return pl2r_symbol(pl) ;

  if(PL_is_list(pl))
    return pl2r_list(pl, names, vars, options) ;

  if(PL_is_compound(pl))
    return pl2r_compound(pl, names, vars, options) ;

  if(PL_is_variable(pl))
    return pl2r_variable(pl, names, vars) ;

  term_t name ;
  size_t arity ;
  if((name = PL_new_term_ref())
      && PL_get_name_arity(pl, &name, &arity))
    stop("pl2r: Cannot convert %s/%ld", PL_atom_nchars(name, NULL), arity) ;

  stop("pl2r: Cannot convert") ;
}

// Translate R expression to prolog
//
// Forward declarations
term_t r2pl_real(NumericVector r, List options) ;
term_t r2pl_logical(LogicalVector r, List options) ;
term_t r2pl_integer(IntegerVector r, List options) ;
term_t r2pl_string(CharacterVector r, List options) ;

// This returns an empty list
term_t r2pl_null()
{
  term_t null ;
  if(!(null = PL_new_term_ref())
      || !PL_put_nil(null))
    stop("r2pl: cannot create null") ;

  return null ;
}

// Prolog representation of R's NA.
term_t r2pl_na()
{
  term_t na ;
  if(!(na = PL_new_term_ref())
      || !PL_put_atom_chars(na, "na"))
    stop("r2pl: cannot create na") ;

  return na ;
}

// Translate to matrix ###(##(1.0, 2.0, 3.0), ##(4.0, 5.0, 6.0))
term_t r2pl_matrix(Matrix<REALSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows ;
  if(!(rows = PL_new_term_refs(r.nrow())))
    stop("Could not convert R realmat") ;

  for(int i=0 ; i<r.nrow() ; i++)
    if(!PL_put_term(rows + i, r2pl_real(r.row(i), options)))
      stop("Could not convert R realmat") ;

  functor_t functor ;
  term_t matrix ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) aoptions("realmat")), r.nrow()))
      || !(matrix = PL_new_term_ref())
      || !PL_cons_functor_v(matrix, functor, rows))
    stop("Could not convert R realmat") ;

  return matrix ;
}

// Translate to (scalar) real or compounds like ##(1.0, 2.0, 3.0)
term_t r2pl_real(NumericVector r, List options)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<REALSXP>>(r), options) ;

  if(r.length() == 0)
    return r2pl_null() ;

  LogicalVector nan = is_nan(r) ;
  LogicalVector na = is_na(r) ;
  
  // Translate to scalar
  if(as<LogicalVector>(options("scalar"))(0) && r.length() == 1)
  {
    if(na[0] && !nan[0])
      return r2pl_na() ;
    
    term_t f ;
    if(!(f = PL_new_term_ref())
        || !PL_put_float(f, (double) r(0)))
      stop("Could not convert R realvec") ;

    return f ;
  }

  // Translate to vector ##(1.0, 2.0, 3.0)
  size_t len = (size_t) r.length() ;
  term_t args ;
  if(!(args = PL_new_term_refs(len)))
    stop("Could not convert R realvec") ;

  for(size_t i=0 ; i<len ; i++)
    if(na[i] && !nan[i])
    {
      if(!PL_put_term(args + i, r2pl_na()))
        stop("Could not convert R realvec") ;
    }
    else
    {
      if(!PL_put_float(args + i, (double) r(i)))
        stop("Could not convert R realvec") ;
    }

  term_t vec ;
  functor_t functor ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) options("realvec")), len))
      || !(vec = PL_new_term_ref())
      || !PL_cons_functor_v(vec, functor, args))
    stop("Could not convert R realvec") ;

  return vec ;
}

// Translate to matrix !!!(!!(true, false), !(false, true))
term_t r2pl_matrix(Matrix<LGLSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows ;
  if(!(rows = PL_new_term_refs(r.nrow())))
    stop("Could not convert R boolmat") ;

  for(int i=0 ; i<r.nrow() ; i++)
    if(!PL_put_term(rows+i, r2pl_logical(r.row(i), options)))
      stop("Could not convert R boolmat") ;

  term_t matrix ;
  functor_t functor ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) aoptions("boolmat")), r.nrow()))
      || !(matrix = PL_new_term_ref())
      || !PL_cons_functor_v(matrix, functor, rows))
    stop("Could not convert R boolmat") ;

  return matrix ;
}

// Translate to (scalar) boolean or compounds like !!(true, false, na)
term_t r2pl_logical(LogicalVector r, List options)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<LGLSXP>>(r), options) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar boolean
  if(as<LogicalVector>(options("scalar"))(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    term_t pl ;
    if(!(pl = PL_new_term_ref())
        || !PL_put_atom_chars(pl, r[0] ? "true" : "false"))
      stop("r2pl: cannot create boolean") ;

    return pl ;
  }

  // LogicalVector !!(true, false, na)
  size_t len = (size_t) r.length() ;
  term_t args ;
  if(!(args = PL_new_term_refs(len)))
    stop("Could not convert R boolvec") ;

  for(size_t i=0 ; i<len ; i++)
    if(na[i])
    {
      if(!PL_put_term(args+i, r2pl_na()))
        stop("Could not convert R boolvec") ;
    }
    else
    {
      if(!PL_put_atom_chars(args + i, r(i) ? "true" : "false"))
        stop("Could not convert R boolvec") ;
    }

  term_t vec ;
  functor_t functor ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) options("boolvec")), len))
      || !(vec = PL_new_term_ref())
      || !PL_cons_functor_v(vec, functor, args))
    stop("Could not convert R boolvec") ;

  return vec ;
}

// Translate to matrix %%%(%%(1, 2), %(3, 4))
term_t r2pl_matrix(Matrix<INTSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows ;
  if(!(rows = PL_new_term_refs(r.nrow())))
    stop("Could not convert R intmat") ;

  for(int i=0 ; i<r.nrow() ; i++)
    if(!PL_put_term(rows+i, r2pl_integer(r.row(i), options)))
      stop("Could not convert R intmat") ;

  term_t matrix ;
  functor_t functor ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) aoptions("intmat")), r.nrow()))
      || !(matrix = PL_new_term_ref())
      || !PL_cons_functor_v(matrix, functor, rows))
    stop("Could not convert R intmat") ;

  return matrix ;
}

// Translate to (scalar) integer or compounds like %%(1, 2, 3)
term_t r2pl_integer(IntegerVector r, List options)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<INTSXP>>(r), options) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar integer
  if(as<LogicalVector>(options("scalar"))(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;

    term_t f = PL_new_term_ref() ;
    if(!PL_put_integer(f, (long) r(0)))
      stop("Could not convert R intvec") ;

    return f ;
  }
  
  // IntegerVector %%(1, 2, 3)
  size_t len = (size_t) r.length() ;
  term_t args ;
  if(!(args = PL_new_term_refs(len)))
    stop("Could not convert R intvec") ;

  for(size_t i=0 ; i<len ; i++)
    if(na[i])
    {
      if(!PL_put_term(args + i, r2pl_na()))
        stop("Could not convert R intvec") ;
    }
    else
    {
      if(!PL_put_integer(args + i, (long) r[i]))
        stop("Could not convert R intvec") ;
    }

  functor_t functor ;
  term_t vec ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) options("intvec")), len))
      || !(vec = PL_new_term_ref())
      || !PL_cons_functor_v(vec, functor, args))
    stop("Could not convert R intvec") ;

  return vec ;
}

// Translate to matrix $$$($$(1, 2), $$(3, 4))
term_t r2pl_matrix(Matrix<STRSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows ;
  if(!(rows = PL_new_term_refs(r.nrow())))
    stop("Could not convert R matrix") ;

  for(int i=0 ; i<r.nrow() ; i++)
    if(!PL_put_term(rows+i, r2pl_string(r.row(i), options)))
      stop("Could not convert R matrix") ;

  functor_t functor ;
  term_t matrix ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) aoptions("charmat")), r.nrow()))
      || !(matrix = PL_new_term_ref())
      || !PL_cons_functor_v(matrix, functor, rows))
    stop("Could not convert R matrix") ;

  return matrix ;
}

// Translate CharacterVector to (scalar) string or things like $$("a", "b", "c")
term_t r2pl_string(CharacterVector r, List options)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<STRSXP>>(r), options) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar string
  if(as<LogicalVector>(options["scalar"])(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
  
    term_t f ;
    if(!(f = PL_new_term_ref())
        || !PL_put_string_chars(f, r(0)))
      stop("Could not convert R charvec") ;

    return f ;
  }

  // compound like $$("a", "b", "c")
  size_t len = (size_t) r.length() ;
  term_t args ;
  if(!(args = PL_new_term_refs(len)))
    stop("Could not convert R charvec") ;

  for(size_t i=0 ; i<len ; i++)
    if(na[i])
    {
      if(!PL_put_term(args + i, r2pl_na()))
        stop("Could not convert R charvec") ;
    }
    else
    {
      if(!PL_put_string_chars(args + i, r(i)))
        stop("Could not convert R charvec") ;
    }

  functor_t functor ;
  term_t vec ;
  if(!(functor = PL_new_functor(PL_new_atom((const char*) options("charvec")), len))
      || !(vec = PL_new_term_ref())
      || !PL_cons_functor_v(vec, functor, args))
    stop("Could not convert R charvec") ;

  return vec ;
}

// Translate R expression to prolog variable
//
// This function keeps a record of the names of the variables in 
// use (e.g., _1545) as well as the corresponding R names (e.g., X). If a new
// variable is encountered, its name is looked up in the list of known 
// variables, and it is unified with it if the name is found. Otherwise, a new
// variable is created.
//
// If options("atomize") is true, no variable is created, but an atom is created 
// with the variable name from R. This is only used for pretty printing.
term_t r2pl_variable(ExpressionVector r, CharacterVector& names, term_t& vars, List options)
{
  // Variable name in R
  Symbol n = as<Symbol>(r[0]) ;
  
  // If the variable should be "atomized" for pretty printing
  if(as<LogicalVector>(options("atomize"))(0) == true)
  {
    term_t pl ;
    if(!(pl = PL_new_term_ref())
        || !PL_unify_atom_chars(pl, n.c_str()))
      stop("r2pl: cannot create variable name") ;

    return pl ;
  }

  // Do not map the anonymous variable to a known variable name
  if(n == "_")
  {
    term_t pl ;
    if(!(pl = PL_new_term_ref()))
      stop("r2pl: cannot create variable") ;

    return pl ;
  }

  // Unify with existing variable of the same name
  term_t head = PL_new_term_ref() ;
  term_t tail = PL_copy_term_ref(vars) ;
  for(R_xlen_t i=0 ; i<names.length() ; i++)
  {
    PL_get_list_ex(tail, head, tail) ;
    if(n == names(i))
      return head ;
  }

  // If no such variable exists, create a new one and remember the name
  names.push_back(n.c_str()) ;

  term_t pl ;
  if(!(pl = PL_new_term_ref())
      || !PL_unify_list(tail, pl, tail))
    stop("Could not convert R expression") ;

  return pl ;
}

// Translate R symbol to prolog atom
term_t r2pl_atom(Symbol r)
{
  term_t pl ;
  if(!(pl = PL_new_term_ref())
      || !PL_put_atom_chars(pl, r.c_str()))
    stop("r2pl: cannot create atom from symbol") ;

  return pl ;
}

// Translate R call to prolog compound, taking into account the names of the
// arguments, e.g., rexp(50, rate=1) -> rexp(50, =(rate, 1))
term_t r2pl_compound(Language r, CharacterVector& names, term_t& vars, List options)
{
  // For convenience, collect arguments in a list
  List l = as<List>(CDR(r)) ;

  // R functions with no arguments are translated to compounds (not atoms)
  size_t len = (size_t) l.size() ;
  if(len == 0)
  {
    functor_t functor ;
    term_t pl ;
    if(!(functor = PL_new_functor(PL_new_atom(as<Symbol>(CAR(r)).c_str()), 0))
        || !(pl = PL_new_term_ref())
        || !PL_cons_functor(pl, functor))
      stop("Could not convert R call") ;

    return pl ;
  }

  // Extract names of arguments
  CharacterVector n ;
  // if there are no names, l.names() returns NULL and n has length 0
  if(TYPEOF(l.names()) == STRSXP)
    n = l.names() ;
  
  functor_t functor ;
  term_t args ;
  if(!(functor = PL_new_functor(PL_new_atom(as<Symbol>(CAR(r)).c_str()), len))
      || !(args = PL_new_term_refs(len)))
    stop("Could not convert R call") ;

  for(size_t i=0 ; i<len ; i++)
  {
    term_t arg = r2pl(l(i), names, vars, options) ;
    
    // Convert named arguments to prolog compounds a=X
    if(n.length() && n(i) != "")
    {
      functor_t eq ;
      term_t name, named ;
      if(!(eq = PL_new_functor(PL_new_atom("="), 2))
          || !(name = PL_new_term_ref())
          || !PL_put_atom_chars(name, n(i))
          || !(named = PL_new_term_ref())
          || !PL_cons_functor(named, eq, name, arg)
          || !PL_put_term(args + i, named))
        stop("Could not convert R call") ;
    }
    else
    {
      // no name
      if(!PL_put_term(args + i, arg))
        stop("Could not convert R call") ;
    }
  }

  term_t pl ;
  if(!(pl = PL_new_term_ref())
      || !PL_cons_functor_v(pl, functor, args))
    stop("Could not convert R call") ;

  return pl ;
}

// Translate R list to prolog list, taking into account the names of the
// elements, e.g., list(a=1, b=2) -> [a-1, b-2]. This may change, since the
// minus sign is a bit specific to prolog, and the conversion in the reverse
// direction may be ambiguous.
//
term_t r2pl_list(List r, CharacterVector& names, term_t& vars, List options)
{
  // Names of list elements (empty vector if r.names() == NULL)  
  CharacterVector n ;
  if(TYPEOF(r.names()) == STRSXP)
    n = as<CharacterVector>(r.names()) ;

  term_t pl = PL_new_term_ref() ;
  term_t tail = PL_copy_term_ref(pl) ;
  term_t item = PL_new_term_ref() ;
  for(R_xlen_t i=0; i<r.size(); i++)
  {
    term_t elem = r2pl(r(i), names, vars, options) ;

    // Convert named argument to prolog pair a-X.
    if(n.length() && n(i) != "")
    {
      functor_t eq ;
      term_t name, named ;
      if(!(eq = PL_new_functor(PL_new_atom("-"), 2))
          || !(name = PL_new_term_ref())
          || !(named = PL_new_term_ref())
          || !PL_put_atom_chars(name, n(i))
          || !PL_cons_functor(named, eq, name, elem)
          || !PL_unify_list(tail, item, tail)
          || !PL_unify(item, named))
        stop("Could not convert R list") ;
    }
    else
    {
      // no name
      if(!PL_unify_list(tail, item, tail)
          || !PL_unify(item, elem))
        stop("Could not convert R list") ;
    }         
  }

  if(!PL_unify_nil(tail))
    stop("Could not convert R list") ;

  return pl ;
}

// Translate R function to :- ("neck")
term_t r2pl_function(Function r, CharacterVector& names, term_t& vars, List options)
{
  term_t body = r2pl_compound(BODY(r), names, vars, options) ;
  
  List formals = as<List>(FORMALS(r)) ;
  size_t len = (size_t) formals.size() ;

  term_t function = PL_new_term_ref() ;
  if(len == 0)
  {
    functor_t functor ;
    if(!(functor = PL_new_functor(PL_new_atom("function"), 0))
        || !PL_cons_functor(function, functor))
      stop("Could not convert R function") ;
  }
  else
  {
    CharacterVector n = formals.names() ;
    term_t args = PL_new_term_refs(len) ;
    for(size_t i=0 ; i<len ; i++)
      PL_put_atom_chars(args + i, n(i)) ;

    functor_t functor ;
    if(!(functor = PL_new_functor(PL_new_atom("function"), len))
        || !PL_cons_functor_v(function, functor, args))
      stop("Could not convert R function") ;
  }

  functor_t neck ;
  term_t pl ;
  if(!(neck = PL_new_functor(PL_new_atom(":-"), 2))
      || !(pl = PL_new_term_ref())
      || !PL_cons_functor(pl, neck, function, body))
    stop("Could not convert R function") ;

  return pl ;
}

// Translate R primitive to :- ("neck")
term_t r2pl_builtin(Function r, CharacterVector& names, term_t& vars, List options)
{
  term_t body = r2pl_null() ;

  List formals = as<List>(FORMALS(r)) ;
  size_t len = (size_t) formals.size() ;

  functor_t functor ;
  term_t function ;
  if(!(function = PL_new_term_ref()))
    stop("Could not convert R builtin function") ;

  if(len == 0)
  {
    if(!(functor = PL_new_functor(PL_new_atom("function"), 0))
        || !PL_cons_functor(function, functor))
      stop("Could not convert R builtin function") ;
  }
  else
  {
    CharacterVector n = formals.names() ;
    term_t args = PL_new_term_refs(len) ;
    for(size_t i=0 ; i<len ; i++)
      PL_put_atom_chars(args + i, n(i)) ;

    if(!(functor = PL_new_functor(PL_new_atom("function"), len))
        || !PL_cons_functor_v(function, functor, args))
      stop("Could not convert R builtin function") ;
  }

  functor_t neck ;
  term_t pl ;
  if(!(neck = PL_new_functor(PL_new_atom(":-"), 2))
      || !(pl = PL_new_term_ref())
      || !PL_cons_functor(pl, neck, function, body))
    stop("Could not convert R builtin function") ;

  return pl ;
}

term_t r2pl(SEXP r, CharacterVector& names, term_t& vars, List options)
{
  if(TYPEOF(r) == LANGSXP)
    return r2pl_compound(r, names, vars, options) ;

  if(TYPEOF(r) == REALSXP)
    return r2pl_real(r, options) ;
  
  if(TYPEOF(r) == LGLSXP)
    return r2pl_logical(r, options) ;
  
  if(TYPEOF(r) == INTSXP)
    return r2pl_integer(r, options) ;
  
  if(TYPEOF(r) == EXPRSXP)
    return r2pl_variable(r, names, vars, options) ;

  if(TYPEOF(r) == SYMSXP)
    return r2pl_atom(r) ;

  if(TYPEOF(r) == STRSXP)
    return r2pl_string(r, options) ;

  if(TYPEOF(r) == VECSXP)
    return r2pl_list(r, names, vars, options) ;
  
  if(TYPEOF(r) == NILSXP)
    return r2pl_null() ;
  
  if(TYPEOF(r) == CLOSXP)
    return r2pl_function(r, names, vars, options) ;

  if(TYPEOF(r) == BUILTINSXP)
    return r2pl_builtin(r, names, vars, options) ;

  warning("r2pl: cannot translate R object of class %s, returning NA\n", 
    as<CharacterVector>(Function("class")(r))) ;
  return r2pl_na() ;
}

class RlQuery
{
  CharacterVector names ;
  term_t vars ;
  List options ;
  Environment env ;
  qid_t qid ;

public:
  RlQuery(RObject aquery, List aoptions, Environment aenv) ;
  ~RlQuery() ;

  int next_solution() ;

  List bindings() ;

  const List& get_options() const
  {
    return options ;
  }

  Environment& get_env()
  {
    return env ;
  }
} ;

RlQuery::RlQuery(RObject aquery, List aoptions, Environment aenv)
  : names(),
    vars(PL_new_term_ref()),
    options(aoptions),
    env(aenv),
    qid(0)
{
  options("atomize") = false ;
  term_t pl = r2pl(aquery, names, vars, options) ;
  predicate_t p = PL_predicate("call", 1, NULL) ;
  qid = PL_open_query(NULL, PL_Q_PASS_EXCEPTION|PL_Q_EXT_STATUS, p, pl) ;
}

RlQuery::~RlQuery()
{
  if(qid)
    PL_close_query(qid) ;
}

int RlQuery::next_solution()
{
  if(qid == 0)
    stop("next_solution: no open query.") ;

  int q = PL_next_solution(qid) ;
  if(q == PL_S_TRUE)
    return true ;

  if(q == PL_S_LAST)
    return true ;

  if(q == PL_S_FALSE)
  {
    PL_close_query(qid) ;
    qid = 0 ;
    return false ;
  }

  if(q == PL_S_EXCEPTION)
  {
    PL_close_query(qid) ;
    qid = 0 ;

    term_t ex = PL_exception(0) ;
    char* err ;
    if(PL_get_chars(ex, &err, BUF_DISCARDABLE|CVT_WRITE|REP_UTF8))
    {
      PL_clear_exception() ;
      warning(err) ;
      return false ;
    }

    PL_clear_exception() ;
    warning("query: unknown exception occurred") ;
    return false ;
  }

  // should not be reached
  return q ;
}

List RlQuery::bindings()
{
  List l ;

  term_t head = PL_new_term_ref() ;
  term_t tail = PL_copy_term_ref(vars) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PL_get_list_ex(tail, head, tail) ;
    RObject r = pl2r(head, names, vars, options) ;
    if(TYPEOF(r) == EXPRSXP && names[i] == as<Symbol>(as<ExpressionVector>(r)[0]).c_str())
      continue ;

    l.push_back(r, (const char*) names[i]) ;
  }

  return l ;
}

static RlQuery* query_id = NULL ;

// Open a query for later use.
// [[Rcpp::export(.query)]]
RObject query_(RObject query, List options, Environment env)
{
  if(query_id || PL_current_query())
  {
    warning("Cannot raise simultaneous queries. Please invoke clear()") ;
    return wrap(false) ;
  }

  query_id = new RlQuery(query, options, env) ;
  return wrap(true) ;
}

// Clear query (and invoke cleanup handlers, see PL_close_query)
// [[Rcpp::export(.clear)]]
RObject clear_()
{
  if(query_id)
    delete query_id ;
  query_id = NULL ;

  return wrap(true) ;
}

// Submit query
// [[Rcpp::export(.submit)]]
RObject submit_()
{
  if(query_id == NULL)
  {
    warning("submit: no open query.") ;
    return wrap(false) ;
  }

  if(!query_id->next_solution())
  {
    delete query_id ;
    query_id = NULL ;
    return wrap(false) ;
  }

  return query_id->bindings() ;
}

// Execute a query once and return conditions
//
// Examples:
//
// once(call("=", 1, 2)) -> FALSE
// once(call("=", 1, 1)) -> empty list
// once(call("member", 1, list(2, expression(X)))) -> list stating that X = 1
// once(call("=", list(expression(X), expression(Y)), list(1, expression(Z))))
//   -> list stating that X = 1 and Z = Y
// once(call("member", 1, expression(X))) -> list stating that X = [1 | _]; 
//   e.g., something like [|]`(1, expression(`_6330`)). This is cumbersome, any
//   better ideas are welcome.
//
// [[Rcpp::export(.once)]]
RObject once_(RObject query, List options, Environment env)
{
  fid_t f = PL_open_foreign_frame() ;

  if(!query_(query, options, env))
  {
    PL_discard_foreign_frame(f) ;
    stop("Could not create query.") ;
  }
    
  RObject l = submit_() ;
  clear_() ;
  PL_close_foreign_frame(f) ;
  return l ;
}

// Same as once_ above, but return all solutions to a query.
// [[Rcpp::export(.findall)]]
List findall_(RObject query, List options, Environment env)
{
  fid_t f = PL_open_foreign_frame() ;
  if(!query_(query, options, env))
  {
    PL_discard_foreign_frame(f) ;
    stop("Could not create query.") ;
  }

  List results ;
  while(true)
  {
    RObject l = submit_() ;
    if(TYPEOF(l) == LGLSXP)
      break ;
    
    results.push_back(l) ;
  }
  
  clear_() ;
  PL_close_foreign_frame(f) ;
  return results ;
}

// Pretty print query
//
// [[Rcpp::export(.portray)]]
RObject portray_(RObject query, List options)
{
  if(PL_current_query() != 0)
  {
    warning("Closing the current query.") ;
    clear_() ;
  }

  // translate variables to their R names
  options("atomize") = true ;
  CharacterVector names ;
  term_t vars, pl ;
  if(!(vars = PL_new_term_ref())
      || !(pl = PL_new_term_refs(3))
      || !PL_put_term(pl, r2pl(query, names, vars, options)))
    stop("cannot portray") ;

  functor_t quoted_functor, spacing_functor ;
  term_t list, quoted, quoted_arg, spacing, spacing_arg ;
  if(!(list = PL_new_term_ref())
      || !PL_put_nil(list)
      || !(quoted_functor = PL_new_functor(PL_new_atom("quoted"), 1))
      || !(quoted_arg = PL_new_term_ref())
      || !PL_put_atom_chars(quoted_arg, "false")
      || !(quoted = PL_new_term_ref())
      || !PL_cons_functor(quoted, quoted_functor, quoted_arg)
      || !PL_cons_list(list, quoted, list)
      || !(spacing_functor = PL_new_functor(PL_new_atom("spacing"), 1))
      || !(spacing_arg = PL_new_term_ref())
      || !(PL_put_atom_chars(spacing_arg, "next_argument"))
      || !(spacing = PL_new_term_ref())
      || !PL_cons_functor(spacing, spacing_functor, spacing_arg)
      || !PL_cons_list(list, spacing, list)
      || !PL_put_term(pl + 2, list))
    stop("cannot portray") ;

  fid_t f = PL_open_foreign_frame() ;
  predicate_t p = PL_predicate("term_string", 3, NULL) ;
  if(!PL_call_predicate(NULL, PL_Q_NORMAL, p, pl))
  {
    PL_close_foreign_frame(f) ;
    return wrap(false) ;
  }
  
  RObject r = pl2r(pl + 1, names, vars, options) ;
  PL_close_foreign_frame(f) ;
  return r ;
}

// Call R expression from Prolog
static foreign_t r_eval1(term_t arg1) 
{
  CharacterVector names ;
  term_t vars = PL_new_term_ref() ;
  List options ;
  if(query_id)
    options = query_id->get_options() ;
  else
    options = List::create(
      Named("realvec") = "##", Named("realmat") = "###",
      Named("boolvec") = "!!", Named("boolmat") = "!!!",
      Named("charvec") = "$$", Named("charmat") = "$$$",
      Named("intvec") = "%%", Named("intmat") = "%%%", 
      Named("atomize") = false, Named("scalar") = true) ;

  RObject Expr = pl2r(arg1, names, vars, options) ;
  RObject Res = Expr ;
  try
  {
    Environment env = query_id->get_env() ;
    Res = Language("dontCheck", Expr).eval(env) ;
  }

  catch(std::exception& cex)
  {
    term_t ex ;
    if((ex = PL_new_term_ref())
        && PL_unify_term(ex, PL_FUNCTOR_CHARS, "r_eval1_error", 2, 
             PL_TERM, arg1, PL_CHARS, cex.what()))
      PL_raise_exception(ex) ;

    return false ;
  }

  catch(...)
  {
    term_t ex ;
    if((ex = PL_new_term_ref())
        && PL_unify_term(ex, PL_FUNCTOR_CHARS, "r_eval1_error", 1, 
             PL_TERM, arg1))
      PL_raise_exception(ex) ;

    return false ;
  }

  return true ;
}

// Evaluate R expression from Prolog
static foreign_t r_eval2(term_t arg1, term_t arg2)
{
  CharacterVector names ;
  term_t vars = PL_new_term_ref() ;
  List options ;
  if(query_id)
    options = query_id->get_options() ;
  else
    options = List::create(
      Named("realvec") = "##", Named("realmat") = "###",
      Named("boolvec") = "!!", Named("boolmat") = "!!!",
      Named("charvec") = "$$", Named("charmat") = "$$$",
      Named("intvec") = "%%", Named("intmat") = "%%%", 
      Named("atomize") = false, Named("scalar") = true) ;

  RObject Expr = pl2r(arg1, names, vars, options) ;
  RObject Res = Expr ;
  try
  {
    Environment env = query_id->get_env() ;
    Res = Language("dontCheck", Expr).eval(env) ;
  }

  catch(std::exception& cex)
  {
    term_t ex ;
    if((ex = PL_new_term_ref())
        && PL_unify_term(ex, PL_FUNCTOR_CHARS, "r_eval2_error", 3, 
             PL_TERM, arg1, PL_TERM, arg2, PL_CHARS, cex.what()))
      PL_raise_exception(ex) ;

    return false ;
  }

  catch(...)
  {
    term_t ex ;
    if((ex = PL_new_term_ref())
        && PL_unify_term(ex, PL_FUNCTOR_CHARS, "r_eval2_error", 2,
             PL_TERM, arg1, PL_TERM, arg2))
      PL_raise_exception(ex) ;

    return false ;
  }
  
  term_t pl = r2pl(Res, names, vars, options) ;
  return PL_unify(arg2, pl) ;
}

static foreign_t r_eval(term_t args, int arity, void* context)
{
  if(arity == 1)
    return r_eval1(args) ;

  if(arity == 2)
    return r_eval2(args, args + 1) ;

  term_t ex ;
  if((ex = PL_new_term_ref()) 
      && PL_unify_term(ex, PL_FUNCTOR_CHARS, "domain_error", 2, 
           PL_CHARS, "1, 2", PL_CHARS, "arity"))
    PL_raise_exception(ex) ;

  return false ;
}

// The SWI system should not be initialized twice; therefore, we keep track of
// its status.
bool pl_initialized = false ;

// Initialize SWI-prolog. This needs a list of the command-line arguments of 
// the calling program, the most important being the name of the main 
// executable, argv[0]. I added "-q" to suppress SWI prolog's welcome message
// which is shown in .onAttach anyway.
// [[Rcpp::export(.init)]]
LogicalVector init_(String argv0)
{
  if(pl_initialized)
    warning("Please do not initialize SWI-prolog twice in the same session.") ;
  
  // Prolog documentation requires that argv is accessible during the entire 
  // session. I assume that this pointer is valid during the whole R session,
  // and that I can safely cast it to const.
  const int argc = 2 ;
  const char* argv[argc] ;
  argv[0] = argv0.get_cstring() ;
  argv[1] = "-q" ;
  if(!PL_initialise(argc, (char**) argv))
    stop("rolog_init: initialization failed.") ;

  PL_register_foreign("r_eval", 1, (void*) r_eval, PL_FA_VARARGS) ;
  PL_register_foreign("r_eval", 2, (void*) r_eval, PL_FA_VARARGS) ;
  pl_initialized = true ;  
  return true ;
}

// [[Rcpp::export(.done)]]
LogicalVector done_()
{
  if(!pl_initialized)
  {
    warning("rolog_done: swipl has not been initialized") ;
    return true ;
  }

  // Just in case there are open queries
  clear_() ;

  PL_cleanup(0) ;
  pl_initialized = false ;
  return true ;
}
