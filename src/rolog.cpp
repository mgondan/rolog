#include <SWI-cpp.h>
#include "Rcpp.h"
using namespace Rcpp ;

// Initialize SWI-prolog. This needs a list of the command-line arguments of 
// calling program, the most important being the name of the main executable,
// argv[0]. The SWI system should not be initialized twice; therefore, we keep
// track of it using a global flag.
bool pl_initialized = false ;

// [[Rcpp::export(.init)]]
LogicalVector init_(String argv0)
{
  // See comment below in done_
  if(pl_initialized)
    warning("Please do not initialize SWI-prolog twice in the same session.") ;
  
  // Prolog documentation requires that argv is accessible during the entire 
  // session. I assume that this pointer is valid during the whole R session,
  // and that I can safely cast it to const.
  int argc = 2 ;
  char* argv[argc] ;
  argv[0] = const_cast<char*>(argv0.get_cstring()) ;
  
  // Suppress SWI-Prolog's welcome message
  argv[1] = const_cast<char*>("-q") ;
  
  // If initialization fails, SWI raises an exception with some diagnostic
  // information.
  if(!PL_initialise(argc, argv))
    stop("rolog_init: initialization failed.") ;

  pl_initialized = true ;  
  return true ;
}

// [[Rcpp::export(.done)]]
LogicalVector done_()
{
  if(!pl_initialized)
    stop("rolog_done: engine has not been initialized") ;

  // Prolog documentation says that PL_cleanup is not fully functional, so this
  // code is preliminary. In particular, it is currently not possible to unload 
  // rolog and load it again in the same R session.
  //
  // For these reasons, the call to cleanup is currently suppressed.
  //
  // PL_cleanup(0) ;
  // pl_initialized = false ;
  return true ;
}

// Consult one or more files. If something fails, the procedure stops, and
// will not try to consult the remaining files.
//
// [[Rcpp::export(.consult)]]
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

// Translate prolog expression to R
//
// [] -> NULL
// real -> NumericVector
// #(r1, r2, r3) -> NumericVector (# is a default, see option realvec)
// integer -> IntegerVector
// %(i1, i2, i3) -> IntegerVector (see option intvec for the name)
// string -> CharacterVector
// $(s1, s2, s3) CharacterVector
// na (atom) -> NA
// true, false (atoms) -> LogicalVector
// !(l1, l2, l3) -> LogicalVector (see option boolvec)
// other atoms -> symbol/name
// variable -> expression(variable name)
// compound -> call (aka. "language")
// list -> list
//
RObject pl2r(PlTerm pl, CharacterVector& names, PlTerm& vars, List options) ;

RObject pl2r_null()
{
  return R_NilValue ;
}

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
    PL_clear_exception() ;
    warning("cannot convert to floating point %s", (char*) pl) ;
    return NA_REAL ;
  }
}

DoubleVector pl2r_real(PlTerm pl)
{
  DoubleVector r(1) ;
  r(0) = pl2r_double(pl) ;
  return r ;
}

DoubleVector pl2r_realvec(PlTerm pl)
{
  DoubleVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_double(pl.operator[](i+1)) ;

  return r ;
}

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
    PL_clear_exception() ;
    warning("cannot convert to integer %s", (char*) pl) ;
    return NA_INTEGER ;
  }
}

IntegerVector pl2r_integer(PlTerm pl)
{
  IntegerVector r(1) ;
  r(0) = pl2r_int(pl) ;
  return r ;
}

IntegerVector pl2r_intvec(PlTerm pl)
{
  IntegerVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_int(pl.operator[](i+1)) ;

  return r ;
}

String pl2r_string(PlTerm pl)
{
  if(PL_is_atom(pl) && pl == "na")
    return NA_STRING ;
  
  return (char*) pl ;
}

CharacterVector pl2r_char(PlTerm pl)
{
  CharacterVector r(1) ;
  r(0) = pl2r_string(pl) ;
  return r ;
}

CharacterVector pl2r_charvec(PlTerm pl)
{
  CharacterVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_string(pl.operator[](i+1)) ;

  return r ;
}

// Convert prolog atom to R symbol (handle na, true, false).
RObject pl2r_symbol(PlTerm pl)
{
  if(pl == "na")
    return(LogicalVector::create(NA_LOGICAL)) ;
  
  if(pl == "true")
    return(LogicalVector::create(1)) ;
  
  if(pl == "false")
    return(LogicalVector::create(0)) ;
  
  return as<RObject>(Symbol((char*) pl)) ;
}

int pl2r_bool(PlTerm pl)
{
  if(PL_is_atom(pl) && pl == "na")
    return NA_LOGICAL ;
  
  if(PL_is_atom(pl) && pl == "true")
    return 1 ;
  
  if(PL_is_atom(pl) && pl == "false")
    return 0 ;

  warning("r2pl_logical: invalid item %s, returning NA", (char*) pl) ;
  return NA_LOGICAL ;
}

LogicalVector pl2r_boolvec(PlTerm pl)
{
  LogicalVector r(pl.arity()) ;
  for(size_t i=0; i<pl.arity(); i++)
    r(i) = pl2r_bool(pl.operator[](i+1)) ;

  return r ;
}

RObject pl2r_variable(PlTerm pl, CharacterVector& names, PlTerm& vars)
{
  // names and vars is a list of all the variables from the R query,
  // a typical member of names is something like X, a member of vars 
  // is something like _1545.
  //
  // Search for the variable (e.g., _1545) in names and return its R name as an
  // expression (say, X).
  PlTail tail(vars) ;
  PlTerm v ;
  for(int i=0 ; i<names.length() ; i++)
  {
    tail.next(v) ;
    if(pl == (const char*) v)
      return ExpressionVector::create(Symbol(names(i))) ;
  }
  
  // If the variable is not found, it's a new one created by Prolog, e.g., in
  // queries like member(1, Y), Y is unified with [1 | _NewVar ]. This variable
  // cannot be translated.
  return ExpressionVector::create(Symbol((char*) pl)) ;
}

// Translate prolog compound to R call
RObject pl2r_compound(PlTerm pl, CharacterVector& names, PlTerm& vars, List options)
{
  // This function does not (yet) work for cyclic terms
  if(!PL_is_acyclic(pl))
    stop("pl2r: Cannot convert cyclic term %s", (char*) pl) ;

  if(!strcmp(pl.name(), options("realvec")))
    return pl2r_realvec(pl) ;

  if(!strcmp(pl.name(), options("intvec")))
    return pl2r_intvec(pl) ;

  if(!strcmp(pl.name(), options("charvec")))
    return pl2r_charvec(pl) ;

  if(!strcmp(pl.name(), options("boolvec")))
    return pl2r_boolvec(pl) ;

  Language r(pl.name()) ;
  for(unsigned int i=1 ; i<=pl.arity() ; i++)
  {
    PlTerm arg = pl[i] ;

    // Compounds like mean=100 are translated to named function arguments
    if(PL_is_compound(arg) && !strcmp(arg.name(), "=") && arg.arity() == 2)
    {
      PlTerm a1 = arg.operator[](1) ;
      PlTerm a2 = arg.operator[](2) ;
      if(PL_is_atom(a1))
      {
        r.push_back(Named(a1.name()) = pl2r(a2, names, vars, options)) ;
        continue ;
      }
    }

    r.push_back(pl2r(arg, names, vars, options)) ; // no name
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
RObject pl2r_list(PlTerm pl, CharacterVector& names, PlTerm& vars, List options)
{
  PlTerm head = pl.operator[](1) ;
  
  // if the tail is a list or empty, return a normal list
  RObject tail = pl2r(pl.operator[](2), names, vars, options) ;
  if(TYPEOF(tail) == VECSXP || TYPEOF(tail) == NILSXP)
  {
    List r = as<List>(tail) ;
    
    // Convert prolog pair a-X to named list element
    if(PL_is_compound(head) && !strcmp(head.name(), "-") && head.arity() == 2)
    {
      PlTerm a1 = head.operator[](1) ;
      PlTerm a2 = head.operator[](2) ;
      if(PL_is_atom(a1))
      {
        r.push_front(pl2r(a2, names, vars, options), a1.name()) ;
        return r ;
      }
    }
    
    // no name
    r.push_front(pl2r(head, names, vars, options)) ; 
    return r ;
  }
    
  // if the tail is something else, return [|](head, tail)
  Language r(pl.name()) ;
  
  // Convert prolog pair a-X to named list element
  if(PL_is_compound(head) && !strcmp(head.name(), "-") && head.arity() == 2)
  {
    PlTerm a1 = head.operator[](1) ;
    PlTerm a2 = head.operator[](2) ;
    if(PL_is_atom(a1))
    {
      r.push_back(Named(a1.name()) = pl2r(a2, names, vars, options)) ;
      r.push_back(tail) ;
      return as<RObject>(r) ;
    }
  }

  // no name
  r.push_back(pl2r(head, names, vars, options)) ; 
  r.push_back(tail) ;
  return as<RObject>(r) ;
}

RObject pl2r(PlTerm pl, CharacterVector& names, PlTerm& vars, List options)
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
  
  stop("pl2r: Cannot convert %s", (char*) pl) ;
}

// Translate r expression to prolog
//
// NULL -> []
// numeric -> real
// integer -> integer
// character -> string
// symbol/name -> atom
// expression -> variable
// call/language -> compound
// list -> list
// todo: vector -> #(elements)
//
// atomize is a temporary solution that allows for pretty printing the
// prolog query.
//
PlTerm r2pl(SEXP r, CharacterVector& names, PlTerm& vars, bool atomize, List options) ;

PlTerm r2pl_null()
{
  PlTerm pl ;
  PlTail(pl).close() ;
  return pl ;
}

PlTerm r2pl_na()
{
  return PlAtom("na") ;
}

PlTerm r2pl_real(NumericVector r, List options)
{
  if(r.length() == 0)
    return r2pl_null() ;

  LogicalVector nan = is_nan(r) ;
  LogicalVector na = is_na(r) ;
  if(as<LogicalVector>(options["scalar"])(0) && r.length() == 1)
  {
    if(na[0] && !nan[0])
      return r2pl_na() ;
    
    return PlTerm((double) r[0]) ;
  }

  PlTermv args(r.length()) ;
  for(R_xlen_t i=0 ; i<r.length() ; i++)
  {
    if(na[i] && !nan[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm((double) r[i]) ;
  }
  
  return PlCompound((const char*) options("realvec"), args) ;
}

PlTerm r2pl_logical(LogicalVector r, List options)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  if(as<LogicalVector>(options["scalar"])(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm(r[0] ? "true" : "false") ;
  }

  PlTermv args(r.length()) ;
  for(R_xlen_t i=0 ; i<r.length() ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm(r[i] ? "true" : "false") ;
  }

  return PlCompound((const char*) options("boolvec"), args) ;
}

PlTerm r2pl_integer(IntegerVector r, List options)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  if(as<LogicalVector>(options["scalar"])(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm((long) r(0)) ;
  }
  
  PlTermv args(r.length()) ;
  for(R_xlen_t i=0 ; i<r.length() ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlTerm((long) r[i]) ;
  }
  
  return PlCompound((const char*) options("intvec"), args) ;
}

// Translate R expression to prolog variable
//
// This function keeps a record of the used variables as well as their R names.
// If a new variable is encountered, its name is looked up in the list of known
// variables, and it is unified with it if the name is found.
// Otherwise, a new variable is created.
//
// If atomize is true, no variable is created, but an atom is created with the
// variable name from R. This is only used for pretty printing.
//
PlTerm r2pl_var(ExpressionVector r, CharacterVector& names, PlTerm& vars, bool atomize)
{
  // Variable name in R
  Symbol n = as<Symbol>(r[0]) ;
  
  // If the variable should be "atomized" for pretty printing
  if(atomize)
    return PlAtom(n.c_str()) ;

  // Do not map the anonymous variable to a known variable name
  if(n == "_")
    return PlTerm() ;

  // Unify with existing variable of the same name
  PlTail tail(vars) ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PlTerm v ;
    tail.next(v) ;
    if(n == names[i])
      return v ;
  }

  // If no such variable exists, create a new one and remember the name
  names.push_back(n.c_str()) ;
  PlTerm pl ;
  tail.append(pl) ;
  return pl ;
}

PlTerm r2pl_atom(Symbol r)
{
  return PlAtom(r.c_str()) ;
}

PlTerm r2pl_string(CharacterVector r, List options)
{
  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  if(as<LogicalVector>(options["scalar"])(0) && r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlString(r(0)) ;
  }
  
  PlTermv args(r.length()) ;
  for(R_xlen_t i=0 ; i<r.length() ; i++)
  {
    if(na[i])
      args[i] = r2pl_na() ;
    else
      args[i] = PlString(r(i)) ;
  }

  return PlCompound((const char*) options("charvec"), args) ;
}

// Translate R call to prolog compound, taking into account the names of the
// arguments, e.g., rexp(50, rate=1) -> rexp(50, =(rate, 1))
PlTerm r2pl_compound(Language r, CharacterVector& names, PlTerm& vars, bool atomize, List options)
{
  List l = as<List>(CDR(r)) ;

  // Extract names of arguments
  CharacterVector n ;
  if(TYPEOF(l.names()) == STRSXP)
    n = l.names() ;
  
  PlTermv pl(l.size()) ;
  for(R_xlen_t i=0 ; i<l.size() ; i++)
  {
    PlTerm arg = r2pl(l(i), names, vars, atomize, options) ;
    
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
PlTerm r2pl_list(List r, CharacterVector& names, PlTerm& vars, bool atomize, List options)
{
  PlTerm pl ;
  PlTail tail(pl) ;
  
  CharacterVector n ;
  if(TYPEOF(r.names()) == STRSXP)
    n = as<CharacterVector>(r.names()) ;
  
  for(R_xlen_t i=0; i<r.size() ; i++)
  {
    PlTerm arg = r2pl(r(i), names, vars, atomize, options) ;
    
    // Convert named argument to prolog pair a-X.
    if(n.length() && n(i) != "")
      tail.append(PlCompound("-", PlTermv(PlAtom(n(i)), arg))) ;
    else
      tail.append(arg) ; // no name
  }
  
  tail.close() ;
  return pl ;
}

PlTerm r2pl(SEXP r, CharacterVector& names, PlTerm& vars, bool atomize, List options)
{
  if(TYPEOF(r) == LANGSXP)
    return r2pl_compound(r, names, vars, atomize, options) ;

  if(TYPEOF(r) == REALSXP)
    return r2pl_real(r, options) ;
  
  if(TYPEOF(r) == LGLSXP)
    return r2pl_logical(r, options) ;
  
  if(TYPEOF(r) == INTSXP)
    return r2pl_integer(r, options) ;
  
  if(TYPEOF(r) == EXPRSXP)
    return r2pl_var(r, names, vars, atomize) ;

  if(TYPEOF(r) == SYMSXP)
    return r2pl_atom(r) ;

  if(TYPEOF(r) == STRSXP)
    return r2pl_string(r, options) ;

  if(TYPEOF(r) == VECSXP)
    return r2pl_list(r, names, vars, atomize, options) ;
  
  if(TYPEOF(r) == NILSXP)
    return r2pl_null() ;
  
  return r2pl_na() ;
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
RObject once_(RObject query, List options)
{
  CharacterVector names ;
  PlTerm vars ;
  PlTerm pl = r2pl(query, names, vars, false, options) ;

  PlQuery q("call", pl) ;
  try
  {
    if(!q.next_solution())
      return LogicalVector(false) ;
  }
  
  catch(PlException& ex)
  { 
    PL_clear_exception() ;
    stop("%s failed: %s", (char*) pl, (char*) ex) ;
  }

  List l ;

  PlTail tail(vars) ;
  PlTerm v ;
  for(int i=0 ; i<names.length() ; i++)
  {
    tail.next(v) ;

    RObject r = pl2r(v, names, vars, options) ;
    if(TYPEOF(r) == EXPRSXP 
         && names[i] == as<Symbol>(as<ExpressionVector>(r)[0]).c_str())
      continue ;
      
    l.push_back(r, (const char*) names[i]) ;
  }
  
  return l ;
}

// Same as once_ above, but return all solutions to a query.
// [[Rcpp::export(.findall)]]
List findall_(RObject query, List options)
{
  CharacterVector names ;
  PlTerm vars ;
  PlTerm pl = r2pl(query, names, vars, false, options) ;

  PlQuery q("call", pl) ;
  List results ;
  while(true)
  {
    try
    {
      if(!q.next_solution())
        return results ;
    }
    
    catch(PlException& ex)
    {
      PL_clear_exception() ;
      stop("%s failed: %s", (char*) pl, (char*) ex) ;
    }
    
    List l ;
    PlTail tail(vars) ;
    PlTerm v ;
    for(int i=0 ; i<names.length() ; i++)
    {
      tail.next(v) ;

      RObject r = pl2r(v, names, vars, options) ;
      if(TYPEOF(r) == EXPRSXP 
           && names[i] == as<Symbol>(as<ExpressionVector>(r)[0]).c_str())
        continue ;
        
      l.push_back(r, (const char*) names[i]) ;
    }
    
    results.push_back(l) ;
  }
}

// Pretty print query. Maybe simplify to something like this:
// with_output_to(string(S), write_term(member(X), [variable_names(['X'=X])])).
//
// [[Rcpp::export(.portray)]]
RObject portray_(RObject query, List options)
{
  CharacterVector names ;
  PlTerm vars ;
  PlTermv pl(3) ;
  pl[0] = r2pl(query, names, vars, true, options) ;
  PlTail tail(pl[2]) ;
  tail.append(PlCompound("quoted", PlTermv(PlAtom("false")))) ;
  tail.append(PlCompound("spacing", PlTermv(PlAtom("next_argument")))) ;
  tail.close() ;

  PlQuery q("term_string", pl) ;
  try
  {
    if(!q.next_solution())
      return LogicalVector(false) ;
  }
  
  catch(PlException& ex)
  {
    PL_clear_exception() ;
    stop("portray of %s failed: %s", (char*) pl[0], (char*) ex) ;
  }
  
  return pl2r(pl[1], names, vars, options) ;
}
