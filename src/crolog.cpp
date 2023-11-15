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
    if(PL_call_predicate(NULL, PL_Q_NORMAL, p, fname) == FALSE)
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
  size_t arity = PL_functor_arity(pl) ;
  DoubleVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    PL_get_arg(pl, i+1, arg) ;
    r(i) = pl2r_double(arg) ;
  }

  return r ;
}

// Convert matrix of reals (e.g., ##(#(1.0, 2.0), #(na, ...), ...))
NumericMatrix pl2r_realmat(term_t pl)
{
  size_t nrow = PL_functor_arity(pl) ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
      {
        term_t row1 ;
        PL_get_arg(pl, 1, row1) ;
        ncol = PL_functor_arity(row1) ;
      }
      else
      {
        term_t row ;
        PL_get_arg(pl, i+1, row) ;
        if(PL_functor_arity(row) != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  NumericMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    PL_get_arg(pl, i+1, row) ;
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
  size_t arity = PL_functor_arity(pl) ;
  IntegerVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    PL_get_arg(pl, i+1, arg) ;
    r(i) = pl2r_int(arg) ;
  }

  return r ;
}

IntegerMatrix pl2r_intmat(term_t pl)
{
  size_t nrow = PL_functor_arity(pl) ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
      {
        term_t row1 ;
        PL_get_arg(pl, 1, row1) ;
        ncol = PL_functor_arity(row1) ;
      }
      else
      {
        term_t row ;
        PL_get_arg(pl, i+1, row) ;
        if(PL_functor_arity(row) != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  IntegerMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    PL_get_arg(pl, i+1, row) ;
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
  size_t arity = PL_functor_arity(pl) ;
  CharacterVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t arg ;
    PL_get_arg(pl, i+1, arg) ;
    r(i) = pl2r_string(arg) ;
  }

  return r ;
}

CharacterMatrix pl2r_charmat(term_t pl)
{
  size_t nrow = PL_functor_arity(pl) ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
      {
        term_t row1 ;
        PL_get_arg(pl, 1, row1) ;
        ncol = PL_functor_arity(row1) ;
      }
      else
      {
        term_t row ;
        PL_get_arg(pl, i+1, row) ;
        if(PL_functor_arity(row) != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  CharacterMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    PL_get_arg(pl, i+1, row) ;
    r.row(i) = pl2r_charvec(row) ;
  }

  return r ;
}

// Convert prolog atom to R symbol (handle na, true, false)
RObject pl2r_symbol(term_t pl)
{
  if(!strcmp(PL_atom_nchars(pl, NULL), "na"))
    return wrap(NA_LOGICAL) ;
  
  if(!strcmp(PL_atom_nchars(pl, NULL), "true"))
    return wrap(true) ;
  
  if(!strcmp(PL_atom_nchars(pl, NULL), "false"))
    return wrap(false) ;

  // Empty symbols
  if(!strcmp(PL_atom_nchars(pl, NULL), ""))
    return Function("substitute")() ;

  return wrap(Symbol(PL_atom_nchars(pl, NULL))) ;
}

// Forward declaration, needed below
RObject pl2r_compound(term_t pl, CharacterVector& names, term_t& vars, List options) ;

// Convert prolog neck to R function
RObject pl2r_function(term_t pl, CharacterVector& names, term_t& vars, List options)
{
  term_t plhead ;
  PL_get_arg(pl, 1, plhead) ;

  term_t plbody ;
  PL_get_arg(pl, 2, plbody) ;

  Language head("alist") ;
  for(size_t i=1 ; i<=PL_functor_arity(plhead) ; i++)
  {
    term_t arg ;
    PL_get_arg(plhead, i, arg) ;

    // Compounds like mean=100 are translated to named function arguments
    term_t arg_name = PL_new_term_ref() ;
    size_t arg_arity ;
    if(PL_is_compound(arg) && PL_get_name_arity(arg, &arg_name, &arg_arity)
       && !strcmp(PL_atom_nchars(arg_name, NULL), "=") && arg_arity == 2)
    {
      term_t a1 ;
      PL_get_arg(arg, 1, a1) ;

      term_t a2 ;
      PL_get_arg(arg, 2, a2) ;
      if(PL_is_atom(a1))
      {
        head.push_back(Named(PL_atom_nchars(a1, NULL)) = pl2r(a2, names, vars, options)) ;
        continue ;
      }
    }

    // just the name, no argument like in alist(a=, b=)
    head.push_back(Named(PL_atom_nchars(arg, NULL)) = Function("substitute")()) ;
  }

  RObject body = pl2r_compound(plbody, names, vars, options) ;
  head.push_back(body) ;

  Function as_function("as.function") ;
  return wrap(as_function(head)) ;
}

LogicalVector pl2r_boolvec(term_t pl)
{
  size_t arity = PL_functor_arity(pl) ;
  LogicalVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    term_t t ;
    PL_get_arg(pl, i+1, t) ;
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
    
    warning("r2pl_logical: invalid item %s, returning NA", PL_atom_nchars(t, NULL)) ;
    r(i) = NA_LOGICAL ;
  }

  return r ;
}

LogicalMatrix pl2r_boolmat(term_t pl)
{
  size_t nrow = PL_functor_arity(pl) ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
      {
        term_t row1 ;
        PL_get_arg(pl, 1, row1) ;
        ncol = PL_functor_arity(row1) ;
      }
      else
      {
        term_t row ;
        PL_get_arg(pl, i+1, row) ;
        if(PL_functor_arity(row) != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  LogicalMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
  {
    term_t row ;
    PL_get_arg(pl, i+1, row) ;
    r.row(i) = pl2r_boolvec(row) ;
  }

  return r ;
}


void
put_list(term_t l, int n, char **words)
{ term_t a = PL_new_term_ref();

  PL_put_nil(l);
  while( --n >= 0 )
  { PL_put_atom_chars(a, words[n]);
    PL_cons_list(l, a, l);
  }
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
  char* n ;
  PL_get_chars(pl, &n, CVT_ALL|BUF_MALLOC|CVT_EXCEPTION) ;
  
  term_t head = PL_new_term_ref() ;
  term_t tail = PL_copy_term_ref(vars) ;
  for(size_t i=0 ; i<names.length() ; i++)
  {
    PL_get_list_ex(tail, head, tail) ;
    char *s ;
    PL_get_chars(head, &s, CVT_ATOM|BUF_MALLOC|CVT_EXCEPTION) ;
    if(!strcmp(s, n))
    {
      ExpressionVector Symb((char*) names(i)) ;
      PL_free(s) ;
      PL_free(n) ;
      return Symb ;
    }

    PL_free(s) ;
  }

  PL_free(n) ;

  // If the variable is not found, it's a new one created by Prolog, e.g., in
  // queries like member(1, Y), Y is unified with [1 | _NewVar ]. This variable
  // cannot be translated to a human-readable name, so it is returned as _1545.
  PL_get_chars(pl, &n, CVT_ALL|BUF_MALLOC|REP_UTF8) ;
  ExpressionVector Symb(n) ;
  PL_free(n) ;
  return Symb ;
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
    PL_get_chars(pl, &n, CVT_ALL|BUF_DISCARDABLE|REP_UTF8) ;
    stop("pl2r: Cannot convert cyclic term %s", n) ;
  }

  // Convert ###(##(...), ...) to NumericMatrix
  term_t name = PL_new_term_ref() ;
  size_t arity ;
  PL_get_name_arity(pl, &name, &arity) ;
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
    term_t arg = PL_new_term_ref() ;
    PL_get_arg(pl, i, arg) ;

    term_t arg_name = PL_new_term_ref() ;
    size_t arg_arity ;
    PL_get_name_arity(arg, &arg_name, &arg_arity) ;

    // Compounds like mean=100 are translated to named function arguments
    if(PL_is_compound(arg) && !strcmp(PL_atom_nchars(arg_name, NULL), "=") && arg_arity == 2)
    {
      term_t a1 = PL_new_term_ref() ;
      PL_get_arg(arg, 1, a1) ;
      
      term_t a2 = PL_new_term_ref() ;
      PL_get_arg(arg, 2, a2) ;
      if(PL_is_atom(a1))
      {
        term_t name = PL_new_term_ref() ;
        PL_get_name_arity(a1, &name, NULL) ;
        r.push_back(Named(PL_atom_nchars(name, NULL)) = pl2r(a2, names, vars, options)) ;
        continue ;
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
  term_t head = PL_new_term_ref() ;
  PL_get_arg(pl, 1, head) ;
  term_t head_name = PL_new_term_ref() ;
  size_t head_arity ;
  PL_get_name_arity(head, &head_name, &head_arity) ;

  // if the tail is a list or empty, return a normal list
  term_t tail = PL_new_term_ref() ;
  PL_get_arg(pl, 2, tail) ;
  RObject rest = pl2r(tail, names, vars, options) ;
  if(TYPEOF(rest) == VECSXP || TYPEOF(rest) == NILSXP)
  {
    List r = as<List>(rest) ;
    
    // convert prolog pair a-X to named list element
    if(PL_is_compound(head) && !strcmp(PL_atom_nchars(head_name, NULL), "-") && head_arity == 2)
    {
      term_t a1 = PL_new_term_ref() ;
      PL_get_arg(head, 1, a1) ;

      term_t a2 = PL_new_term_ref() ;
      PL_get_arg(head, 2, a2) ;

      if(PL_is_atom(a1))
      {
        r.push_front(pl2r(a2, names, vars, options), PL_atom_nchars(a1, NULL)) ;
        return r ;
      }
    }
    
    // element has no name
    r.push_front(pl2r(head, names, vars, options)) ; 
    return r ;
  }
    
  // if the tail is something else, return [|](head, tail)
  term_t name = PL_new_term_ref() ;
  size_t arity ;
  PL_get_name_arity(pl, &name, &arity) ;
  Language r(PL_atom_nchars(name, NULL)) ;
  
  // convert prolog pair a-X to named list element
  if(PL_is_compound(head) && !strcmp(PL_atom_nchars(name, NULL), "-") && arity == 2)
  {
    term_t a1 = PL_new_term_ref() ;
    PL_get_arg(head, 1, a1) ;

    term_t a2 = PL_new_term_ref() ;
    PL_get_arg(head, 2, a2) ;
    if(PL_is_atom(a1))
    {
      r.push_back(Named(PL_atom_nchars(a1, NULL)) = pl2r(a2, names, vars, options)) ;
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

  term_t name = PL_new_term_ref() ;
  size_t arity ;
  PL_get_name_arity(pl, &name, &arity) ;
  stop("pl2r: Cannot convert %s/%ld", PL_atom_nchars(name, NULL), arity) ;
  return pl2r_null() ;
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
  return ATOM_nil ;
}

// Prolog representation of R's NA.
term_t r2pl_na()
{
  return PL_new_atom("na") ;
}

// Translate to matrix ###(##(1.0, 2.0, 3.0), ##(4.0, 5.0, 6.0))
term_t r2pl_matrix(Matrix<REALSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;

  term_t data_functor = PL_new_functor(PL_new_atom("data"), r.nrow()) ;

  term_t rows = PL_new_term_refs(r.nrow()) ;
  for(size_t i=0 ; i<r.nrow() ; i++)
    PL_put_term(rows+i, r2pl_real(r.row(i), options)) ;

  term_t data = PL_new_term_ref() ;
  PL_cons_functor_v(data, data_functor, rows) ;

  term_t matrix_functor = PL_new_functor(PL_new_atom((const char*) aoptions("realmat")), 5) ;

  term_t dim = PL_new_term_refs(5) ;
  PL_put_integer(dim, r.nrow()) ;
  PL_put_integer(dim+1, r.ncol()) ;
  PL_put_term(dim+2, r2pl_string(rownames(r), options)) ;
  PL_put_term(dim+3, r2pl_string(colnames(r), options)) ;
  PL_put_term(dim+4, data) ;

  term_t matrix = PL_new_term_ref() ;
  PL_cons_functor_v(matrix, matrix_functor, dim) ;
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
    
    term_t f = PL_new_term_ref() ;
    PL_put_float(f, (double) r[0]) ;
    return f ;
  }

  // Translate to vector ##(1.0, 2.0, 3.0)
  size_t len = (size_t) r.length() ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i] && !nan[i])
      PL_put_term(args+i, r2pl_na()) ;
    else
      PL_put_float(args+i, (double) r[i]) ;
  }

  term_t vec_functor = PL_new_functor(PL_new_atom((const char*) options("realvec")), len) ;

  term_t vec = PL_new_term_ref() ;
  PL_cons_functor_v(vec, vec_functor, args) ;
  return vec ;
}

// Translate to matrix !!!(!!(true, false), !(false, true))
term_t r2pl_matrix(Matrix<LGLSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows = PL_new_term_refs(r.nrow()) ;
  for(size_t i=0 ; i<r.nrow() ; i++)
    PL_put_term(rows+i, r2pl_logical(r.row(i), options)) ;

  term_t matrix = PL_new_term_ref() ;
  term_t matrix_functor = PL_new_functor(PL_new_atom((const char*) aoptions("boolmat")), r.nrow()) ;
  PL_cons_functor_v(matrix, matrix_functor, rows) ;
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
    
    return PL_new_atom(r[0] ? "true" : "false") ;
  }

  // LogicalVector !!(true, false, na)
  size_t len = (size_t) r.length() ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PL_put_term(args+i, r2pl_na()) ;
    else
      PL_put_atom(args+i, PL_new_atom(r[i] ? "true" : "false")) ;
  }

  term_t vec = PL_new_term_ref() ;
  term_t vec_functor = PL_new_functor(PL_new_atom((const char*) options("boolvec")), len) ;
  PL_cons_functor_v(vec, vec_functor, args) ;
  return vec ;
}

// Translate to matrix %%%(%%(1, 2), %(3, 4))
term_t r2pl_matrix(Matrix<INTSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows = PL_new_term_refs(r.nrow()) ;
  for(size_t i=0 ; i<r.nrow() ; i++)
    PL_put_term(rows+i, r2pl_integer(r.row(i), options)) ;

  term_t matrix = PL_new_term_ref() ;
  term_t matrix_functor = PL_new_functor(PL_new_atom((const char*) aoptions("intmat")), r.nrow()) ;
  PL_cons_functor_v(matrix, matrix_functor, rows) ;
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
    PL_put_integer(f, (long) r[0]) ;
    return f ;
  }
  
  // IntegerVector %%(1, 2, 3)
  size_t len = (size_t) r.length() ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PL_put_term(args+i, r2pl_na()) ;
    else
      PL_put_integer(args+i, (long) r[i]) ;
  }

  term_t vec_functor = PL_new_functor(PL_new_atom((const char*) options("intvec")), len) ;

  term_t vec = PL_new_term_ref() ;
  PL_cons_functor_v(vec, vec_functor, args) ;
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
term_t r2pl_var(ExpressionVector r, CharacterVector& names, term_t& vars, List options)
{
  // Variable name in R
  Symbol n = as<Symbol>(r[0]) ;
  
  // If the variable should be "atomized" for pretty printing
  if(as<LogicalVector>(options("atomize"))(0))
    return PL_new_atom(n.c_str()) ;

  // Do not map the anonymous variable to a known variable name
  if(n == "_")
    return PL_new_term_ref() ;

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
  term_t pl = PL_new_term_ref() ;
  PL_cons_list(tail, pl, tail) ; // tail should be [] at this stage
  return pl ;
}

// Translate R symbol to prolog atom
term_t r2pl_atom(Symbol r)
{
  return PL_new_atom(r.c_str()) ;
}

// Translate to matrix $$$($$(1, 2), $$(3, 4))
term_t r2pl_matrix(Matrix<STRSXP> r, List aoptions)
{
  List options(aoptions) ;
  options("scalar") = false ;
  term_t rows = PL_new_term_refs(r.nrow()) ;
  for(size_t i=0 ; i<r.nrow() ; i++)
    PL_put_term(rows+i, r2pl_string(r.row(i), options)) ;

  term_t matrix = PL_new_term_ref() ;
  term_t matrix_functor = PL_new_functor(PL_new_atom((const char*) aoptions("charmat")), r.nrow()) ;
  PL_cons_functor_v(matrix, matrix_functor, rows) ;
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
  
    term_t f = PL_new_term_ref() ;
    PL_put_string_chars(f, r(0)) ;
    return f ;
  }

  // compound like $$("a", "b", "c")
  size_t len = (size_t) r.length() ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PL_put_term(args+i, r2pl_na()) ;
    else
      PL_put_string_chars(args+i, r(i)) ;
  }

  term_t vec_functor = PL_new_functor(PL_new_atom((const char*) options("charvec")), len) ;
  term_t vec = PL_new_term_ref() ;
  PL_cons_functor_v(vec, vec_functor, args) ;
  return vec ;
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
    term_t functor = PL_new_functor(PL_new_atom(as<Symbol>(CAR(r)).c_str()), 0) ;
    term_t pl = PL_new_term_ref() ;
    PL_cons_functor(pl, functor) ;
    return pl ;
  }

  // Extract names of arguments
  CharacterVector n ;
  // if there are no names, l.names() returns NULL and n has length 0
  if(TYPEOF(l.names()) == STRSXP)
    n = l.names() ;
  
  term_t functor = PL_new_functor(PL_new_atom(as<Symbol>(CAR(r)).c_str()), len) ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    term_t arg = r2pl(l(i), names, vars, options) ;
    
    // Convert named arguments to prolog compounds a=X
    if(n.length() && n(i) != "")
    {
      term_t eq = PL_new_functor(PL_new_atom("="), 2) ;
      term_t name = PL_new_atom(n(i)) ;
      term_t named = PL_new_term_ref() ;
      PL_cons_functor(named, eq, name, arg) ;
      PL_put_term(args + i, named) ;
    }
    else
      PL_put_term(args + i, arg) ; // no name
  }

  term_t pl = PL_new_term_ref() ;
  PL_cons_functor_v(pl, functor, args) ;
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
  PL_put_nil(pl) ;
  for(R_xlen_t i=r.size()-1; i>=0; i--)
  {
    term_t elem = r2pl(r(i), names, vars, options) ;

    // Convert named argument to prolog pair a-X.
    if(n.length() && n(i) != "")
    {
      term_t eq = PL_new_functor(PL_new_atom("-"), 2) ;
      term_t name = PL_new_atom(n(i)) ;
      term_t named = PL_new_term_ref() ;
      PL_cons_functor(named, eq, name, elem) ;
      PL_cons_list(pl, named, pl) ;
    }
    else
      PL_cons_list(pl, elem, pl) ; // no name
  }

  return pl ;
}

// Translate R function to :- ("neck")
term_t r2pl_function(Function r, CharacterVector& names, term_t& vars, List options)
{
  term_t body = r2pl_compound(BODY(r), names, vars, options) ;
  
  List formals = as<List>(FORMALS(r)) ;
  size_t len = (size_t) formals.size() ;

/*
  if(len == 0)
  {
    PlTermv pl(3) ;
    pl[1] = PlAtom("function") ;
    pl[2] = (long) 0 ;
    PlCall("compound_name_arity", pl) ;

    fun[0] = pl[0] ;
    return PlCompound(":-", fun) ;
  }
*/
  
  CharacterVector n = formals.names() ;
  term_t args = PL_new_term_refs(len) ;
  for(size_t i=0 ; i<len ; i++)
    PL_put_atom(args + i, PL_new_atom(n(i))) ;

  term_t name = PL_new_functor(PL_new_atom("function"), len) ;
  term_t function = PL_new_term_ref() ;
  PL_cons_functor_v(function, name, args) ;

  term_t neck = PL_new_functor(PL_new_atom(":-"), 2) ;
  term_t pl = PL_new_term_ref() ;
  PL_cons_functor(pl, neck, function, body) ;
  return pl ;
}

// Translate R primitive to :- ("neck")
PlTerm r2pl_builtin(Function r, CharacterVector& names, PlTerm& vars, List options)
{
  PlTermv fun(2) ;
  fun[1] = r2pl_null() ;
  
  List formals = as<List>(FORMALS(r)) ;
  size_t len = (size_t) formals.size() ;
  if(len == 0)
  {
    PlTermv pl(3) ;
    pl[1] = PlAtom("function") ;
    pl[2] = (long) 0 ;
    PlCall("compound_name_arity", pl) ;

    fun[0] = pl[0] ;
    return PlCompound(":-", fun) ;
  }
  
  CharacterVector n = formals.names() ;
  PlTermv pl(len) ;
  for(size_t i=0 ; i<len ; i++)
    pl[i] = PlAtom(n(i)) ;

  fun[0] = PlCompound("function", pl) ;
  return PlCompound(":-", fun) ;
}

PlTerm r2pl(SEXP r, CharacterVector& names, PlTerm& vars, List options)
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
    return r2pl_var(r, names, vars, options) ;

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
  PlTerm vars ;
  List options ;
  Environment env ;
  PlQuery* qid ;
  
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
    vars(),
    options(aoptions),
    env(aenv),
    qid(NULL)
{
  options("atomize") = false ;
  PlTerm pl = r2pl(aquery, names, vars, options) ;
  qid = new PlQuery("call", PlTermv(PlTerm(pl))) ;
}

RlQuery::~RlQuery()
{
  if(qid)
    delete qid ;
}

int RlQuery::next_solution()
{
  if(qid == NULL)
    stop("next_solution: no open query.") ;
    
  int q ;
  try
  {
    q = qid->next_solution() ;
  }

  catch(PlException& ex)
  {
    char* err = (char*) ex ;
    PL_clear_exception() ;
    warning("Prolog exception: %s", err) ;
    stop(err) ;
  }

  return q ;
}

List RlQuery::bindings()
{
  List l ;

  PlTail tail(vars) ;
  PlTerm v ;
  for(int i=0 ; i<names.length() ; i++)
  {
    tail.next(v) ;
    RObject r = pl2r(v, names, vars, options) ;
    if(TYPEOF(r) == EXPRSXP && names[i] == as<Symbol>(as<ExpressionVector>(r)[0]).c_str())
    continue ;

    l.push_back(r, (const char*) names[i]) ;
  }

  return l ;
}

RlQuery* query_id = NULL ;

// Open a query for later use.
// [[Rcpp::export(.query)]]
RObject query_(RObject query, List options, Environment env)
{
  if(PL_current_query() != 0)
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
  PlFrame f ;
  if(!query_(query, options, env))
    stop("Could not create query.") ;
    
  RObject l = submit_() ;
  clear_() ;
  return l ;
}

// Same as once_ above, but return all solutions to a query.
// [[Rcpp::export(.findall)]]
List findall_(RObject query, List options, Environment env)
{
  PlFrame f ;
  if(!query_(query, options, env))
    stop("Could not create query.") ;
    
  List results ;
  while(true)
  {
    RObject l = submit_() ;
    if(TYPEOF(l) == LGLSXP)
      break ;
    
    results.push_back(l) ;
  }
  
  clear_() ;
  return results ;
}

// Pretty print query. Maybe simplify to something like this:
// with_output_to(string(S), write_term(member(X), [variable_names(['X'=X])])).
//
// [[Rcpp::export(.portray)]]
RObject portray_(RObject query, List options)
{
  if(PL_current_query() != 0)
  {
    warning("Closing the current query.") ;
    clear_() ;
  }

  CharacterVector names ;
  PlTerm vars ;
  options("atomize") = true ; // translate variables to their R names
  PlTermv pl(3) ;
  pl[0] = r2pl(query, names, vars, options) ;
  PlTail tail(pl[2]) ;
  tail.append(PlCompound("quoted", PlTermv(PlAtom("false")))) ;
  tail.append(PlCompound("spacing", PlTermv(PlAtom("next_argument")))) ;
  tail.close() ;

  PlFrame f ;
  PlQuery q("term_string", pl) ;
  try
  {
    if(!q.next_solution())
      return wrap(false) ;
  }
  
  catch(PlException& ex)
  {
    char* err = (char*) ex ;
    PL_clear_exception() ;
    stop("portray of %s failed: %s", (char*) pl[0], err) ;
  }
  
  return pl2r(pl[1], names, vars, options) ;
}

// Call R expression from Prolog
PREDICATE(r_eval, 1)
{
  CharacterVector names ;
  PlTerm vars ;
  List options ;
  if(query_id)
    options = query_id->get_options() ;
  else
    options = List::create(Named("realvec") = "#", Named("realmat") = "##",
      Named("boolvec") = "!", Named("boolmat") = "!!",
      Named("charvec") = "$$", Named("charmat") = "$$$",
      Named("intvec") = "%", Named("intmat") = "%%", 
      Named("atomize") = false, Named("scalar") = true) ;

  RObject Expr = pl2r(A1, names, vars, options) ;
  RObject Res = Expr ;
  try
  {
    Environment env = query_id->get_env() ;
    Res = Language("dontCheck", Expr).eval(env) ;
  }

  catch(std::exception& ex)
  {
    throw PlException(PlCompound("r_eval", PlTermv(A1, PlAtom(ex.what())))) ;
  }

  catch(...)
  {
    throw PlException(PlCompound("r_eval", PlTermv(A1, PlString("exception occurred")))) ;
  }

  return true ;
}

// Evaluate R expression from Prolog
PREDICATE(r_eval, 2)
{
  CharacterVector names ;
  PlTerm vars ;
  List options ;
  if(query_id)
    options = query_id->get_options() ;
  else
    options = List::create(Named("realvec") = "#", Named("realmat") = "##",
      Named("boolvec") = "!", Named("boolmat") = "!!",
      Named("charvec") = "$$", Named("charmat") = "$$$",
      Named("intvec") = "%", Named("intmat") = "%%", 
      Named("atomize") = false, Named("scalar") = true) ;

  RObject Expr = pl2r(A1, names, vars, options) ;
  RObject Res = Expr ;
  try
  {
    Environment env = query_id->get_env() ;
    Res = Language("dontCheck", Expr).eval(env) ;
  }
  
  catch(std::exception& ex)
  {
    throw PlException(PlCompound("r_eval", PlTermv(A1, PlAtom(ex.what())))) ;
  }

  catch(...)
  {
    throw PlException(PlCompound("r_eval", PlTermv(A1, PlAtom("exception occurred")))) ;
  }

  PlTerm pl = r2pl(Res, names, vars, options) ;
  return A2 = pl ;
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
