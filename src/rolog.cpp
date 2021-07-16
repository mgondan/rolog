#include <SWI-cpp.h>
#include "Rcpp.h"
using namespace Rcpp ;

// PlEngine* pl = NULL ;
int pl = 0 ;
char *av[2];

// [[Rcpp::export]]
LogicalVector init_(String argv0)
{
  if(pl)
  {
    Rcerr << "rolog_init: engine already initialized" << std::endl ;
    return false ;
  }
  
  // pl = new PlEngine(const_cast<char*>(argv0.get_cstring())) ;
  
  int ac = 0;
  av[ac++] = const_cast<char*>(argv0.get_cstring()) ;
  av[ac]   = NULL;
  
  Rcerr << "rolog_init: initialize with " << av[0] << std::endl ;      
  int ret = PL_initialise(ac, av) ;
  if(!ret)
  {
    Rcerr << "rolog_init: failed initialize, return value " << ret << std::endl ;      
    return false ; // throw PlError("failed to initialise");
  }

  pl = 1 ;  
  return true ;
}

// [[Rcpp::export]]
LogicalVector done_()
{
  if(!pl)
  {
    Rcerr << "rolog_done: engine has not been initialized" << std::endl ;
    return false ;
  }

  // delete pl ;
  // pl = NULL ;
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
      Rcerr << "failed to consult " << (char*) files(i) << std::endl ;
      Rcerr << (char*) ex << std::endl ;
      PL_clear_exception() ;
      return false ;
    }
  }

  return true ;
}

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTermv& vars) ;

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

SEXP pl2r_variable(PlTerm arg, CharacterVector& names, PlTermv& vars)
{
  ExpressionVector r(1) ;

  // Find variable
  for(int i=0 ; i<names.length() ; i++)
    if(!strcmp((const char*) arg, (const char*) vars[i]))
    {
      r(0) = names(i) ;
      return r ;
    }
  
  // is this ever needed?
  r(0) = (const char*) arg ;
  return r ;
}

List pl2r_list(PlTerm arg, CharacterVector& names, PlTermv& vars)
{
  List r ;
  
  PlTail tail(arg) ;
  PlTerm e ;
  while(tail.next(e))
    r.push_back(pl2r(e, names, vars)) ;
  
  return r ;
}

Language pl2r_compound(PlTerm term, CharacterVector& names, PlTermv& vars)
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

SEXP pl2r(PlTerm arg, CharacterVector& names, PlTermv& vars)
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

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTermv& vars) ;

PlTerm r2pl_real(NumericVector arg)
{
  return PlTerm(arg(0)) ;
}

PlTerm r2pl_logical(LogicalVector arg)
{
  if(arg(0) == NA_LOGICAL)
    return r2pl_na() ;
  
  return PlAtom(arg(0) ? "TRUE" : "FALSE") ;
}

PlTerm r2pl_integer(IntegerVector arg)
{
  return PlTerm((long) arg(0)) ;
}

PlTerm r2pl_var(ExpressionVector arg, CharacterVector& names, PlTermv vars)
{
  CharacterVector n = as<CharacterVector>(arg[0]) ;

  // anonymous variable
  PlTerm t ;
  if(n[0] == "_")
    return t ;

  // Unify with existing variable of the same name
  for(int i=0 ; i<names.length() ; i++)
    if(n[0] == names[i])
    {
      t = vars[i] ;
      return t ;
    }
  
  // Create new variable
  names.push_back(n[0]) ;  
  t = vars[names.length()-1] ;
  return t ;
}

PlTerm r2pl_atom(Symbol arg)
{
  return PlAtom(as<Symbol>(arg).c_str()) ;
}

PlTerm r2pl_string(CharacterVector arg)
{
  return PlString(arg(0)) ;
}

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

PlTerm r2pl_compound(Language arg, CharacterVector& names, PlTermv& vars)
{
  PlTermv args(arg.size() - 1) ;
  
  R_xlen_t i=0 ;
  for(SEXP cons=CDR(arg) ; cons != R_NilValue ; cons = CDR(cons))
    args[i++] = r2pl(CAR(cons), names, vars) ;

  return PlCompound(as<Symbol>(CAR(arg)).c_str(), args) ;
}

PlTerm r2pl_list(List arg, CharacterVector& names, PlTermv& vars)
{
  PlTerm r ;
  PlTail l(r);
  for(R_xlen_t i=0; i<arg.size() ; i++)
    l.append(r2pl(arg(i), names, vars)) ;
  l.close() ;
  
  return r ;
}

PlTerm r2pl(SEXP arg, CharacterVector& names, PlTermv& vars)
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
  PlTermv vars(5) ;
  PlTerm arg = r2pl(lang, names, vars) ;

  for(int i=0 ; i<names.length() ; i++)
    Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) vars[i] << std::endl ;
  
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

  for(int i=0 ; i<names.length() ; i++)
    Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) vars[i] << std::endl ;

  List l ;
  for(int i=0 ; i<names.length() ; i++)
  {
    RObject r = pl2r(vars[i], names, vars) ;
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
  PlTermv vars(5) ;
  PlTerm arg = r2pl(lang, names, vars) ;
  
  for(int i=0 ; i<names.length() ; i++)
    Rcerr << (char*) as<Symbol>(names[i]).c_str() << ": " << (char*) vars[i] << std::endl ;

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
    for(int i=0 ; i<names.length() ; i++)
    {
      RObject r = pl2r(vars[i], names, vars) ;
      if(TYPEOF(r) == EXPRSXP && !strcmp(as<Symbol>(names[i]).c_str(), as<Symbol>(as<ExpressionVector>(r)[0]).c_str()))
        continue ;
      
      l.push_back(r, as<Symbol>(names[i]).c_str()) ;
    }
    
    all.push_back(l) ;
  }
}
