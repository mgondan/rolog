#include <SWI-cpp.h>
#include "Rcpp.h"
using namespace Rcpp ;

bool pl_initialized = false ;

// [[Rcpp::export]]
LogicalVector init_(String argv0)
{
  if(pl_initialized)
    stop("rolog_init: already initialized") ;
  
  // Prolog documentation requires that argv is accessible during the entire session,
  // unsure if this is guaranteed here
  char* argv = const_cast<char*>(argv0.get_cstring()) ;
  int ret = PL_initialise(1, &argv) ;
  if(!ret)
    stop("rolog_init: failed initialize, return value %i") ;

  pl_initialized = true ;  
  return true ;
}

// [[Rcpp::export]]
LogicalVector done_()
{
  if(!pl_initialized)
    stop("rolog_done: engine has not been initialized") ;

  // Prolog documentation says that PL_cleanup is not fully functional, so this
  // code is preliminary. In particular, it is currently not possible to unload 
  // rolog and load it again in the same R session.
  PL_cleanup(0);
  pl_initialized = false ;
  return true ;
}

// [[Rcpp::export]]
LogicalVector consult_(CharacterVector files)
{
  for(R_xlen_t i=0; i<files.size(); i++)
  {
    try 
    {
      PlCall("consult", PlString(files(i))) ;
    }
    
    catch(PlException& ex)
    { 
      PL_clear_exception() ;
      stop("failed to consult %s: %s", (char*) files(i), (char*) ex) ;
    }
  }

  return true ;
}

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTerm& varlist) ;

SEXP pl2r_null()
{
  return R_NilValue ;
}

DoubleVector pl2r_real(PlTerm arg)
{
  DoubleVector r(1) ;
  r(0) = arg ;
  return r ;
}

IntegerVector pl2r_integer(PlTerm arg)
{
  IntegerVector r(1) ;
  r(0) = arg ;
  return r ;
}

CharacterVector pl2r_char(PlTerm arg)
{
  CharacterVector r(1) ;
  r(0) = (char*) arg ;
  return r ;
}

Symbol pl2r_symbol(PlTerm arg)
{
  return Symbol((char*) arg) ;
}

SEXP pl2r_variable(PlTerm arg, CharacterVector& names, PlTerm& varlist)
{
  ExpressionVector r(1) ;

  // names and varlist is a list of all the variables from the R query,
  // a typical member of names is something like X, a member of varlist 
  // is something like _1545.
  //
  // For the return value, I search for the variable (e.g., _1545) and
  // return its R name (say, X) as an expression.
  PlTail tail(varlist) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PlTerm v ;
    tail.next(v) ;
    if(!strcmp((const char*) arg, (const char*) v))
    {
      r(0) = names(i) ;
      return r ;
    }
  }
  
  // If the variable is not found, it's a new one created by Prolog, e.g., in
  // queries like member(1, Y), Y is unified with [1 | _NewVar ]. This variable
  // cannot be translated.
  r(0) = Symbol((const char*) arg) ;
  return r ;
}

Language pl2r_compound(PlTerm term, CharacterVector& names, PlTerm& varlist)
{
  if(!PL_is_acyclic(term))
  {
    Rcout << "pl2r: Cannot convert cyclic term" << (char*) term << std::endl ;
    return R_NilValue ;
  }
  
  Language r(term.name()) ;
  
  for(unsigned int i=1 ; i<=term.arity() ; i++)
  {
    PlTerm t = term[i] ;

    // Compounds like '='(x, y) are named arguments
    if(PL_is_compound(t) && !strcmp(t.name(), "=") && t.arity() == 2 && PL_is_atom(t[1]))
      r.push_back(Named(t[1].name()) = pl2r(t[2], names, varlist)) ;
    else
      r.push_back(pl2r(t, names, varlist)) ;
  }
  
  return r ;
}

SEXP pl2r_list(PlTerm arg, CharacterVector& names, PlTerm& varlist)
{
  /* This does not work for lists like [1 | Tail]
   * 
   * PlTail tail(arg) ;
   * PlTerm e ;
   * while(tail.next(e))
   * {
   *   Rcout << "pl2r: elem " << (char*) e << std::endl ;
   *   r.push_back(pl2r(e, names, varlist)) ;
   * }
   * return r ;
   */

  
  // [_ | []] or [_ | [_ | _]]
  PlTerm n = arg[1] ;
  SEXP r = pl2r(arg[2], names, varlist) ;
  if(TYPEOF(r) == VECSXP || TYPEOF(r) == NILSXP)
  {
    List l = as<List>(r) ;
    
    if(PL_is_compound(n) && !strcmp(n.name(), "-") && n.arity() == 2 && PL_is_atom(n[1]))
    {
      // Convert prolog pair a-X to named list element
      l.push_front(pl2r(n[2], names, varlist), n[1].name()) ;
    }
    else
      l.push_front(pl2r(n, names, varlist)) ;
    
    return l ;
  }
    
  // [_ | Variable]
  Language l(arg.name()) ;
  if(PL_is_compound(n) && !strcmp(n.name(), "-") && n.arity() == 2 && PL_is_atom(n[1]))
  {
    // Convert prolog pair a-X to named list element
    l.push_back(Named(n[1].name()) = pl2r(n[2], names, varlist)) ;
  }
  else
    l.push_back(pl2r(n, names, varlist)) ;
  
  l.push_back(pl2r(arg[2], names, varlist)) ;
  return l ;
}

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTerm& varlist)
{
  if(PL_term_type(arg) == PL_NIL)
    return pl2r_null() ;
  
  if(PL_is_integer(arg))
    return pl2r_integer(arg) ;
  
  if(PL_is_float(arg))
    return pl2r_real(arg) ;
  
  if(PL_is_string(arg))
    return pl2r_char(arg) ;
  
  if(PL_is_atom(arg))
    return pl2r_symbol(arg) ;
  
  if(PL_is_list(arg))
    return pl2r_list(arg, names, varlist) ;
  
  if(PL_is_compound(arg))
    return pl2r_compound(arg, names, varlist) ;
  
  if(PL_is_variable(arg))
    return pl2r_variable(arg, names, varlist) ;
  
  Rcout << "pl2r: Cannot convert " << (char*) arg << std::endl ;
  return R_NilValue ;
}

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTerm& varlist, bool atomize) ;

PlTerm r2pl_null()
{
  PlTerm r ;
  PlTail(r).close() ;
  return r ;
}

PlTerm r2pl_na()
{
  return PlTerm("NA") ;
}

PlTerm r2pl_real(NumericVector arg)
{
  if(arg.length() == 0)
    return r2pl_null() ;
  
  if(arg(0) == NA_REAL)
    return r2pl_na() ;
  
  return PlTerm(arg(0)) ;
}

PlTerm r2pl_logical(LogicalVector arg)
{
  if(arg.length() == 0)
    return r2pl_null() ;
  
  if(arg(0) == NA_LOGICAL)
    return r2pl_na() ;
  
  return PlAtom(arg(0) ? "TRUE" : "FALSE") ;
}

PlTerm r2pl_integer(IntegerVector arg)
{
  if(arg.length() == 0)
    return r2pl_null() ;
  
  if(arg(0) == NA_INTEGER)
    return r2pl_na() ;
  
  return PlTerm((long) arg(0)) ;
}

PlTerm r2pl_var(ExpressionVector arg, CharacterVector& names, PlTerm& varlist, bool atomize)
{
  CharacterVector n = as<CharacterVector>(arg[0]) ;
  
  // If the variable should be "atomized" for pretty printing
  if(atomize)
    return PlAtom(as<Symbol>(n[0]).c_str()) ;

  // anonymous variable
  if(n[0] == "_")
    return PlTerm() ;

  
  // Unify with existing variable of the same name
  PlTail tail(varlist) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PlTerm v ;
    tail.next(v) ;
    if(n[0] == names[i])
      return v ;
  }

  // Create new variable
  names.push_back(n[0]) ;
  PlTerm v ;
  tail.append(v) ;
  return v ;
}

PlTerm r2pl_atom(Symbol arg)
{
  return PlAtom(arg.c_str()) ;
}

PlTerm r2pl_string(CharacterVector arg)
{
  if(arg.length() == 0)
    return r2pl_null() ;
  
  if(arg(0) == NA_STRING)
    return r2pl_na() ;
  
  return PlString(arg(0)) ;
}

PlTerm r2pl_compound(Language arg, CharacterVector& names, PlTerm& varlist, bool atomize)
{
  List l = as<List>(CDR(arg)) ;
  
  CharacterVector n ;
  if(TYPEOF(l.names()) == STRSXP)
    n = as<CharacterVector>(l.names()) ;
  
  PlTermv args(l.size()) ;
  for(R_xlen_t i=0 ; i<l.size() ; i++)
  {
    PlTerm elem = r2pl(l(i), names, varlist, atomize) ;
    
    // Convert named arguments to prolog compounds a=X
    if(n.length() && n(i) != "")
      args[i] = PlCompound("=", PlTermv(PlAtom(n(i)), elem)) ;
    else
      args[i] = elem ;
  }

  return PlCompound(as<Symbol>(CAR(arg)).c_str(), args) ;
}

PlTerm r2pl_list(List arg, CharacterVector& names, PlTerm& varlist, bool atomize)
{
  PlTerm r ;
  PlTail tail(r) ;
  
  CharacterVector n ;
  if(TYPEOF(arg.names()) == STRSXP)
    n = as<CharacterVector>(arg.names()) ;
  
  for(R_xlen_t i=0; i<arg.size() ; i++)
  {
    PlTerm elem = r2pl(arg(i), names, varlist, atomize) ;
    
    if(n.length() && n(i) != "")
    {
      // Convert named arguments to prolog pairs a-X. This may change, since 
      // the minus sign is a bit specific to prolog, and the conversion in the 
      // reverse direction may be ambiguous.
      tail.append(PlCompound("-", PlTermv(PlAtom(n(i)), elem))) ;
    }
    else
      tail.append(elem) ;
  }
  
  tail.close() ;
  return r ;
}

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTerm& varlist, bool atomize)
{
  if(TYPEOF(arg) == LANGSXP)
    return r2pl_compound(arg, names, varlist, atomize) ;

  if(TYPEOF(arg) == REALSXP)
    return r2pl_real(arg) ;
  
  if(TYPEOF(arg) == LGLSXP)
    return r2pl_logical(arg) ;
  
  if(TYPEOF(arg) == INTSXP)
    return r2pl_integer(arg) ;
  
  if(TYPEOF(arg) == EXPRSXP)
    return r2pl_var(arg, names, varlist, atomize) ;

  if(TYPEOF(arg) == SYMSXP)
    return r2pl_atom(arg) ;

  if(TYPEOF(arg) == STRSXP)
    return r2pl_string(arg) ;

  if(TYPEOF(arg) == VECSXP)
    return r2pl_list(arg, names, varlist, atomize) ;
  
  if(TYPEOF(arg) == NILSXP)
    return r2pl_null() ;
  
  return r2pl_na() ;
}

// [[Rcpp::export]]
RObject once_(RObject lang)
{
  CharacterVector names ;
  PlTerm varlist ;
  PlTerm arg = r2pl(lang, names, varlist, false) ;

  PlQuery q("call", arg) ;
  try
  {
    if(!q.next_solution())
      return LogicalVector(false) ;
  }
  
  catch(PlException& ex)
  { 
    Rcerr << "call failed: " << (char*) arg << std::endl ;
    Rcerr << (char*) ex << std::endl ;
    PL_clear_exception() ;
    return NULL ;
  }

  List l ;
  {
    PlTail tail(varlist) ;
    for(int i=0 ; i<names.length() ; i++)
    {
      PlTerm v ;
      tail.next(v) ;

      RObject r = pl2r(v, names, varlist) ;
      if(TYPEOF(r) == EXPRSXP && !strcmp(as<Symbol>(names[i]).c_str(), as<Symbol>(as<ExpressionVector>(r)[0]).c_str()))
        continue ;
      
      l.push_back(r, as<Symbol>(names[i]).c_str()) ;
    }
  }

  return l ;
}

// [[Rcpp::export]]
List findall_(RObject lang)
{
  CharacterVector names ;
  PlTerm varlist ;
  PlTerm arg = r2pl(lang, names, varlist, false) ;

  PlQuery q("call", arg) ;
  List all ;
  while(true)
  {
    try
    {
      if(!q.next_solution())
        return all ;
    }
    
    catch(PlException& ex)
    { 
      Rcerr << "call failed: " << (char*) arg << std::endl ;
      Rcerr << (char*) ex << std::endl ;
      PL_clear_exception() ;
      return all ;
    }
    
    List l ;
    PlTail tail(varlist) ;
    PlTerm v ;
    for(int i=0 ; i<names.length() ; i++)
    {
      tail.next(v) ;

      RObject r = pl2r(v, names, varlist) ;
      if(TYPEOF(r) == EXPRSXP && !strcmp(as<Symbol>(names[i]).c_str(), as<Symbol>(as<ExpressionVector>(r)[0]).c_str()))
        continue ;
      
      l.push_back(r, as<Symbol>(names[i]).c_str()) ;
    }
    
    all.push_back(l) ;
  }
}

// [[Rcpp::export]]
RObject portray_(RObject lang)
{
  CharacterVector names ;
  PlTerm varlist ;
  PlTermv arg(3) ;
  arg[0] = r2pl(lang, names, varlist, true) ;
  PlTail tail(arg[2]) ;
  tail.append(PlCompound("quoted", PlTermv(PlAtom("false")))) ;
  tail.append(PlCompound("spacing", PlTermv(PlAtom("next_argument")))) ;
  tail.close() ;

  PlQuery q("term_string", arg) ;
  try
  {
    if(!q.next_solution())
      return LogicalVector(false) ;
  }
  
  catch(PlException& ex)
  { 
    Rcerr << "call failed: " << (char*) arg[0] << std::endl ;
    Rcerr << (char*) ex << std::endl ;
    PL_clear_exception() ;
    return NULL ;
  }
  
  return pl2r(arg[1], names, varlist) ;
}

// with_output_to(string(S), write_term(member(X), [variable_names(['X'=X])])).
