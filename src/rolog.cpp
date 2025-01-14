#ifndef RPACKAGE
#include "SWI-cpp.h"

#include "Rcpp.h"
using namespace Rcpp;
#include "RInside.h"

// Translate prolog expression to R
//
// [] -> NULL
// real -> NumericVector
// #(r1, r2, r3) -> NumericVector (# is a default, see option realvec)
// integer -> IntegerVector
// %(i1, i2, i3) -> IntegerVector (see option intvec for the name)
// string -> CharacterVector
// $$(s1, s2, s3) CharacterVector
// na (atom) -> NA
// true, false (atoms) -> LogicalVector
// !(l1, l2, l3) -> LogicalVector (see option boolvec)
// other atoms -> symbol/name
// compound -> call (aka. "language")
// list -> list
//
RObject pl2r(PlTerm pl) ;

// Translate R expression to prolog
//
// NULL -> []
// numeric vector of length 1 -> real (unless rolog.scalar == FALSE)
// numeric vector of length > 1 -> e.g., #(1.0, 2.0, 3.0) (see rolog.realvec)
// integer vector of length 1 -> integer
// integer vector of length > 1 -> %(1, 2, 3)
// character vector of length 1 -> string
// character vector of length > 1 -> $$("a", "b", "c")
// logical vector of length 1 -> the atoms true, false or na
// logical vector of length > 1 -> !(true, false, na)
// symbol/name -> atom
// call/language -> compound
// list -> list
//
PlTerm r2pl(SEXP r) ;

// Prolog -> R
RObject pl2r_null()
{
  return R_NilValue ;
}

// This helper function checks for na and then translates an individual PlTerm 
// to a double.
double pl2r_double(PlTerm pl)
{
  if(PL_is_atom(pl) && pl == "na")
    return NA_REAL ;

  try 
  {
    return (double) pl ;
  }

  catch(PlException& ex)
  { 
    warning("cannot convert %s to float: %s", (char*) pl, (char*) ex) ;
    PL_clear_exception() ;
    return NA_REAL ;
  }
}

// Convert scalar real to DoubleVector of length 1
DoubleVector pl2r_real(PlTerm pl)
{
  return DoubleVector::create(pl2r_double(pl)) ;
}

// Convert vector of reals (e.g., #(1.0, 2.0, na)) to DoubleVector
DoubleVector pl2r_realvec(PlTerm pl)
{
  DoubleVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_double(pl.operator[](i+1)) ;

  return r ;
}

// See above for pl2r_double
long pl2r_int(PlTerm pl)
{
  if(PL_is_atom(pl) && pl == "na")
    return NA_INTEGER ;

  try 
  {
    return (long) pl ;
  }
  
  catch(PlException& ex)
  { 
    warning("Cannot convert %s to integer: %s", (char*) pl, (char*) ex) ;
    PL_clear_exception() ;
    return NA_INTEGER ;
  }
}

IntegerVector pl2r_integer(PlTerm pl)
{
  return IntegerVector::create(pl2r_int(pl)) ;
}

IntegerVector pl2r_intvec(PlTerm pl)
{
  IntegerVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_int(pl.operator[](i+1)) ;

  return r ;
}

// See above for pl2r_double
String pl2r_string(PlTerm pl)
{
  if(PL_is_atom(pl) && pl == "na")
    return NA_STRING ;
  
  return (char*) pl ;
}

CharacterVector pl2r_char(PlTerm pl)
{
  return CharacterVector::create(pl2r_string(pl)) ;
}

CharacterVector pl2r_charvec(PlTerm pl)
{
  CharacterVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_string(pl.operator[](i+1)) ;

  return r ;
}

// Convert prolog atom to R symbol (handle na, true, false)
RObject pl2r_symbol(PlTerm pl)
{
  if(pl == "na")
    return LogicalVector::create(NA_LOGICAL) ;

  if(pl == "NA")
    return LogicalVector::create(NA_LOGICAL) ;
  
  if(pl == "true")
    return LogicalVector::create(1) ;
  
  if(pl == "TRUE")
    return LogicalVector::create(1) ;
  
  if(pl == "false")
    return LogicalVector::create(0) ;

  if(pl == "FALSE")
    return LogicalVector::create(0) ;

  return as<RObject>(Symbol((char*) pl)) ;
}

LogicalVector pl2r_boolvec(PlTerm pl)
{
  LogicalVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
  {
    PlTerm t = pl.operator[](i+1) ;
    if(PL_is_atom(t))
    {
      if(t == "na")
      {
        r(i) = NA_LOGICAL ;
        continue ;
      }
      
      if(t == "true")
      {
        r(i) = 1 ;
        continue ;
      }
      
      if(t == "false")
      {
        r(i) = 0 ;
        continue ;
      }
    }
    
    warning("pl2r_logical: invalid item %s, returning NA", (char*) t) ;
    r(i) = NA_LOGICAL ;
  }

  return r ;
}

// Translate prolog compound to R call
//
// This function takes care of special compound names (#, %, &, !) for vector
// objects in R, as well as "named" function arguments like "mean=100", in
// rnorm(10, mean=100, sd=15).
RObject pl2r_compound(PlTerm pl)
{
  // This function does not (yet) work for cyclic terms
  if(!PL_is_acyclic(pl))
    stop("pl2r: Cannot convert cyclic term %s", (char*) pl) ;

  // Convert #(1.0, 2.0, 3.0) to DoubleVectors (# given by options("realvec"))
  if(!strcmp(pl.name(), "#"))
    return pl2r_realvec(pl) ;

  // Convert %(1.0, 2.0, 3.0) to IntegerVectors
  if(!strcmp(pl.name(), "%"))
    return pl2r_intvec(pl) ;

  // Convert $$(1.0, 2.0, 3.0) to CharacterVectors
  if(!strcmp(pl.name(), "$$"))
    return pl2r_charvec(pl) ;

  // Convert !(1.0, 2.0, 3.0) to LogicalVectors
  if(!strcmp(pl.name(), "!"))
    return pl2r_boolvec(pl) ;

  // Other compounds
  Language r(pl.name()) ;
  for(unsigned int i=1 ; i<=pl.arity() ; i++)
  {
    PlTerm arg = pl[i] ;

    // Compounds like mean=100 are translated to named function arguments
    if(PL_is_compound(arg) && !strcmp(arg.name(), "=") && arg.arity() == 2)
    {
      PlTerm a1 = arg.operator[](1) ;
      PlTerm a2 = arg.operator[](2) ;
      if(PL_is_string(a1))
      {
        r.push_back(Named((char*) a1) = pl2r(a2)) ;
        continue ;
      }
  
      if(PL_is_atom(a1))
      {
        r.push_back(Named(a1.name()) = pl2r(a2)) ;
        continue ;
      }
    }

    // argument has no name
    r.push_back(pl2r(arg)) ;
  }
  
  return as<RObject>(r) ;
}

// Translate prolog list to R list
//
// This code allows for lists like [1, 2 | Tail] with variable tail. These 
// cannot be processed by PlTail, therefore, the code is a bit more 
// complicated, also because it can handle named arguments.
//
// Examples:
// [1, 2, 3] -> list(1, 2, 3)
// [1, 2 | X] -> `[|]`(1, `[|]`(2, expression(X)))
// [a-1, b-2, c-3] -> list(a=1, b=2, c=3)
//
RObject pl2r_list(PlTerm pl)
{
  PlTerm head = pl.operator[](1) ;
  
  // if the tail is a list or empty, return a normal list
  RObject tail = pl2r(pl.operator[](2)) ;
  if(TYPEOF(tail) == VECSXP || TYPEOF(tail) == NILSXP)
  {
    List r = as<List>(tail) ;
    
    // convert prolog pair a-X to named list element
    if(PL_is_compound(head) && !strcmp(head.name(), "-") && head.arity() == 2)
    {
      PlTerm a1 = head.operator[](1) ;
      PlTerm a2 = head.operator[](2) ;
      if(PL_is_atom(a1))
      {
        r.push_front(pl2r(a2), a1.name()) ;
        return r ;
      }
    }
    
    // element has no name
    r.push_front(pl2r(head)) ; 
    return r ;
  }
    
  // if the tail is something else, return [|](head, tail)
  Language r(pl.name()) ;
  
  // convert prolog pair a-X to named list element
  if(PL_is_compound(head) && !strcmp(head.name(), "-") && head.arity() == 2)
  {
    PlTerm a1 = head.operator[](1) ;
    PlTerm a2 = head.operator[](2) ;
    if(PL_is_atom(a1))
    {
      r.push_back(Named(a1.name()) = pl2r(a2)) ;
      r.push_back(tail) ;
      return as<RObject>(r) ;
    }
  }

  // element has no name
  r.push_back(pl2r(head)) ; 
  r.push_back(tail) ;
  return as<RObject>(r) ;
}

RObject pl2r(PlTerm pl)
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
    return pl2r_list(pl) ;
  
  if(PL_is_compound(pl))
    return pl2r_compound(pl) ;
  
  stop("pl2r: Cannot convert %s", (char*) pl) ;
}

// Translate R expression to prolog
//
// This returns an empty list
PlTerm r2pl_null()
{
  PlTerm pl ;
  PlTail(pl).close() ;
  return pl ;
}

// Prolog representation of R's NA.
PlTerm r2pl_na()
{
  return PlAtom("na") ;
}

// Translate to (scalar) real or compounds like #(1.0, 2.0, 3.0)
PlTerm r2pl_real(NumericVector r)
{
  if(r.length() == 0)
    return r2pl_null() ;

  LogicalVector nan = is_nan(r) ;
  LogicalVector na = is_na(r) ;
  
  // Translate to scalar
  if(r.length() == 1)
  {
    if(na[0] && !nan[0])
      return r2pl_na() ;
    
    return PlTerm((double) r[0]) ;
  }

  // Translate to vector #(1.0, 2.0, 3.0)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i] && !nan[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm((double) r[i]) ;
  }
  
  return PlCompound("#", args) ;
}

// Translate to (scalar) boolean or compounds like !(true, false, na)
PlTerm r2pl_logical(LogicalVector r)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar boolean
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm(r[0] ? "true" : "false") ;
  }

  // LogicalVector !(true, false, na)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm(r[i] ? "true" : "false") ;
  }

  return PlCompound("!", args) ;
}

// Translate to (scalar) integer or compounds like %(1, 2, 3)
PlTerm r2pl_integer(IntegerVector r)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar integer
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm((long) r(0)) ;
  }
  
  // IntegerVector %(1, 2, 3)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm((long) r[i]) ;
  }
  
  return PlCompound("%", args) ;
}

// Translate R symbol to prolog atom
PlTerm r2pl_atom(Symbol r)
{
  return PlAtom(r.c_str()) ;
}

// Translate CharacterVector to (scalar) string or things like $$("a", "b", "c")
PlTerm r2pl_string(CharacterVector r)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar string
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlString(r(0)) ;
  }

  // compound like $$("a", "b", "c")
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlString(r(i)) ;
  }

  return PlCompound("$$", args) ;
}

// Translate R call to prolog compound, taking into account the names of the
// arguments, e.g., rexp(50, rate=1) -> rexp(50, =(rate, 1))
PlTerm r2pl_compound(Language r)
{
  // For convenience, collect arguments in a list
  List l = as<List>(CDR(r)) ;

  // Extract names of arguments
  CharacterVector n ;
  // if there are no names, l.names() returns NULL and n has length 0
  if(TYPEOF(l.names()) == STRSXP)
    n = l.names() ;
  
  size_t len = (size_t) l.size() ;
  PlTermv pl(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    PlTerm arg = r2pl(l(i)) ;
    
    // Convert named arguments to prolog compounds a=X
    if(n.length() && n(i) != "")
      pl[i] = PlCompound("=", PlTermv(PlAtom(n(i)), arg)) ;
    else
      pl[i] = arg ; // no name
  }

  return PlCompound(as<Symbol>(CAR(r)).c_str(), pl) ;
}

// Translate R list to prolog list, taking into account the names of the
// elements, e.g., list(a=1, b=2) -> [a-1, b-2]. This may change, since the
// minus sign is a bit specific to prolog, and the conversion in the reverse
// direction may be ambiguous.
//
PlTerm r2pl_list(List r)
{
  // Names of list elements (empty vector if r.names() == NULL)  
  CharacterVector n ;
  if(TYPEOF(r.names()) == STRSXP)
    n = as<CharacterVector>(r.names()) ;
  
  PlTerm pl ;
  PlTail tail(pl) ;
  for(R_xlen_t i=0; i<r.size() ; i++)
  {
    PlTerm arg = r2pl(r(i)) ;
    
    // Convert named argument to prolog pair a-X.
    if(n.length() && n(i) != "")
      tail.append(PlCompound("-", PlTermv(PlAtom(n(i)), arg))) ;
    else
      tail.append(arg) ; // no name
  }
  
  tail.close() ;
  return pl ;
}

PlTerm r2pl(SEXP r)
{
  if(TYPEOF(r) == LANGSXP)
    return r2pl_compound(r) ;

  if(TYPEOF(r) == REALSXP)
    return r2pl_real(r) ;
  
  if(TYPEOF(r) == LGLSXP)
    return r2pl_logical(r) ;
  
  if(TYPEOF(r) == INTSXP)
    return r2pl_integer(r) ;
  
  if(TYPEOF(r) == SYMSXP)
    return r2pl_atom(r) ;

  if(TYPEOF(r) == STRSXP)
    return r2pl_string(r) ;

  if(TYPEOF(r) == VECSXP)
    return r2pl_list(r) ;
  
  if(TYPEOF(r) == NILSXP)
    return r2pl_null() ;
  
  return r2pl_na() ;
}

RInside* r_instance = NULL ;

PREDICATE(r_init, 0)
{
  if(r_instance)
    return true ;

  static int argc ;
  static char** argv ;
  if(!PL_is_initialised(&argc, &argv))
  {
    throw PlException(PlTerm("Prolog not initialized. Exiting.")) ;
    return false ;
  }

  r_instance = new RInside(argc, argv) ;
  return true ;
}

LibExtern char *R_TempDir;    

PREDICATE(r_eval_, 1)
{
  if(!R_TempDir)
    throw PlException(PlTerm("R not initialized. Please invoke r_init.")) ;
  
  RObject Expr = pl2r(A1) ;
  RObject Res = Expr ;
  try 
  {
    Language id("identity") ;
    id.push_back(Expr) ;
    Res = id.eval() ;
  } 
  catch(std::exception& ex)
  {
    throw PlException(PlTerm(ex.what())) ;
    return false ;
  }
  
  return true ;
}

PREDICATE(r_eval_, 2)
{
  if(!R_TempDir)
    throw PlException(PlTerm("R not initialized. Please invoke r_init.")) ;
  
  RObject Expr = pl2r(A1) ;
  RObject Res = Expr ;
  try 
  {
    Language id("identity") ;
    id.push_back(Expr) ;
    Res = id.eval() ;
  } 
  catch(std::exception& ex)
  {
    throw PlException(PlTerm(ex.what())) ;
    return false ;
  }

  PlTerm a2 ;
  try
  {
    a2 = r2pl(Res) ;
  }
  catch(std::exception& ex)
  {
    throw PlException(PlTerm(ex.what())) ;
    return false ;
  }

  return A2 = a2 ;
}

#endif // RPACKAGE

#ifdef PROLOGPACK

void noop()
{}

#endif
