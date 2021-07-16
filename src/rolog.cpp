#include <SWI-cpp.h>
#include "Rcpp.h"
using namespace Rcpp ;

int pl = 0 ;
char *av[2];

// [[Rcpp::export]]
LogicalVector init_(String argv0)
{
  if(pl)
    stop("rolog_init: engine already initialized") ;
  
  int ac = 0;
  av[ac++] = const_cast<char*>(argv0.get_cstring()) ;
  av[ac]   = NULL;
  
  int ret = PL_initialise(ac, av) ;
  if(!ret)
    stop("rolog_init: failed initialize, return value %i") ;

  pl = 1 ;  
  return true ;
}

// [[Rcpp::export]]
LogicalVector done_()
{
  if(!pl)
    stop("rolog_done: engine has not been initialized") ;

  PL_cleanup(0);
  pl = 0 ;
  return true ;
}

// [[Rcpp::export]]
LogicalVector consult_(CharacterVector files)
{
  for(R_xlen_t i=0; i<files.size(); i++)
  {
    try 
    {
      PlCall("consult", PlString((char*) files(i))) ;
    }
    
    catch(PlException& ex)
    { 
      PL_clear_exception() ;
      stop("failed to consult %s: %s", (char*) files(i), (char*) ex) ;
    }
  }

  return true ;
}

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTerm& vars) ;

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

SEXP pl2r_variable(PlTerm arg, CharacterVector& names, PlTerm& vars)
{
  ExpressionVector r(1) ;

  // Find variable
  PlTail tail(vars) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    if(!strcmp((const char*) arg, (const char*) vars))
    {
      r(0) = names(i) ;
      return r ;
    }
    
    tail.next(vars) ;
  }
  
  // is this ever needed?
  r(0) = (const char*) arg ;
  return r ;
}

List pl2r_list(PlTerm arg, CharacterVector& names, PlTerm& vars)
{
  List r ;
  
  PlTail tail(arg) ;
  PlTerm e ;
  while(tail.next(e))
    r.push_back(pl2r(e, names, vars)) ;
  
  return r ;
}

Language pl2r_compound(PlTerm term, CharacterVector& names, PlTerm& vars)
{
  if(!PL_is_acyclic(term))
  {
    Rcout << "pl2r: Cannot convert cyclic term" << (char*) term << std::endl ;
    return R_NilValue ;
  }
  
  Language r(term.name()) ;
  
  for(unsigned int i=1 ; i<=term.arity() ; i++)
  {
    /*
     * // compounds like '='(x, y) are named arguments
     if(PL_is_compound(t[i]) && t[i].name() == std::string("=") && t[i].arity() == 2)
     {
     PlTerm u = t[i] ;
     l.push_back(Named(u[1].name()) = pl2r_leaf(u[2])) ;
     continue ;
     }
     */
    
    r.push_back(pl2r(term[i], names, vars)) ;
  }
  
  return r ;
}

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTerm& vars)
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
    return pl2r_list(arg, names, vars) ;
  
  if(PL_is_compound(arg))
    return pl2r_compound(arg, names, vars) ;
  
  if(PL_is_variable(arg))
    return pl2r_variable(arg, names, vars) ;
  
  Rcout << "pl2r: Cannot convert " << (char*) arg << std::endl ;
  return R_NilValue ;
}

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTerm& vars) ;

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

PlTerm r2pl_var(ExpressionVector arg, CharacterVector& names, PlTerm vars)
{
  CharacterVector n = as<CharacterVector>(arg[0]) ;

  // anonymous variable
  if(n[0] == "_")
    return PlTerm() ;

  // Unify with existing variable of the same name
  PlTerm t = vars ;
  PlTail l(t) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    if(n[0] == names[i])
      return t ;
    
    l.next(t) ;
  }
  
  // Create new variable
  names.push_back(n[0]) ;
  
  PlTerm v ;
  l.append(v) ;
  return v ;
}

PlTerm r2pl_atom(Symbol arg)
{
  return PlAtom(as<Symbol>(arg).c_str()) ;
}

PlTerm r2pl_string(CharacterVector arg)
{
  if(arg.length() == 0)
    return r2pl_null() ;
  
  if(arg(0) == NA_STRING)
    return r2pl_na() ;
  
  return PlString(arg(0)) ;
}

PlTerm r2pl_compound(Language arg, CharacterVector& names, PlTerm& vars)
{
  PlTermv args(arg.size() - 1) ;
  
  R_xlen_t i=0 ;
  for(SEXP cons=CDR(arg) ; cons != R_NilValue ; cons = CDR(cons))
    args[i++] = r2pl(CAR(cons), names, vars) ;

  return PlCompound(as<Symbol>(CAR(arg)).c_str(), args) ;
}

PlTerm r2pl_list(List arg, CharacterVector& names, PlTerm& vars)
{
  PlTerm r ;
  PlTail l(r) ;
  
  for(R_xlen_t i=0; i<arg.size() ; i++)
    l.append(r2pl(arg(i), names, vars)) ;
  
  l.close() ;
  return r ;
}

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTerm& vars)
{
  if(TYPEOF(arg) == LANGSXP)
    return r2pl_compound(arg, names, vars) ;

  if(TYPEOF(arg) == REALSXP)
    return r2pl_real(arg) ;
  
  if(TYPEOF(arg) == LGLSXP)
    return r2pl_logical(arg) ;
  
  if(TYPEOF(arg) == INTSXP)
    return r2pl_integer(arg) ;
  
  if(TYPEOF(arg) == EXPRSXP)
    return r2pl_var(arg, names, vars) ;

  if(TYPEOF(arg) == SYMSXP)
    return r2pl_atom(arg) ;

  if(TYPEOF(arg) == STRSXP)
    return r2pl_string(arg) ;

  if(TYPEOF(arg) == VECSXP)
    return r2pl_list(arg, names, vars) ;
  
  if(TYPEOF(arg) == NILSXP)
    return r2pl_null() ;
  
  return r2pl_na() ;
}

// [[Rcpp::export]]
RObject once_(RObject lang)
{
  CharacterVector names ;
  PlTerm vars ;
  PlTerm arg = r2pl(lang, names, vars) ;

  PlTerm t = vars ;  
  {
    PlTail tail(t) ;
    for(int i=0 ; i<names.length() ; i++)
    {
      Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) t << std::endl ;
      tail.next(t) ;
    }
  }
  
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

  t = vars ;
  {
    PlTail tail(t) ;
    for(int i=0 ; i<names.length() ; i++)
    {    
      Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) t << std::endl ;
      tail.next(t) ;
    }
  }
  
  List l ;
  t = vars ;
  for(int i=0 ; i<names.length() ; i++)
  {
    RObject r = pl2r(t, names, vars) ;
    if(TYPEOF(r) == EXPRSXP && !strcmp(as<Symbol>(names[i]).c_str(), as<Symbol>(as<ExpressionVector>(r)[0]).c_str()))
      continue ;

    l.push_back(r, as<Symbol>(names[i]).c_str()) ;
  }

  return l ;
}

// [[Rcpp::export]]
List findall_(RObject lang)
{
  CharacterVector names ;
  PlTerm vars ;
  PlTerm arg = r2pl(lang, names, vars) ;
  
  PlTerm t = vars ;
  PlTail tail(t) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) t << std::endl ;
    tail.next(t) ;
  }

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
    t = vars ;
    for(int i=0 ; i<names.length() ; i++)
    {
      RObject r = pl2r(t, names, vars) ;
      if(TYPEOF(r) == EXPRSXP && !strcmp(as<Symbol>(names[i]).c_str(), as<Symbol>(as<ExpressionVector>(r)[0]).c_str()))
        continue ;
      
      l.push_back(r, as<Symbol>(names[i]).c_str()) ;
    }
    
    all.push_back(l) ;
  }
}
