#' Translate simplified to canonical representation
#'
#' @param query
#' an R call representing a Prolog query with prolog-like syntax,
#' e.g., `member(.X, ""[a, b, .Y])` for use in [query()], [once()], 
#' and [findall()]. The argument is translated to Rolog's representation, 
#' with R calls corresponding to Prolog terms and R expressions corresponding to
#' Prolog variables. Variables and expressions in parentheses are evaluated.
#'
#' @seealso [query()], [once()], [findall()]
#'
#' @examples
#' as.rolog(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]))
#'
#' @examples
#' q <- quote(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]))
#' as.rolog(q, quoted=TRUE)
#' 
#' @examples
#' findall(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]), preproc=as.rolog)
#'
#' @examples
#' q <- quote(member(.X, ""[a, "b", 3L, 4, pi, (pi), TRUE, .Y]))
#' findall(q, preproc=as.rolog, quoted=TRUE)
#'
as.rolog <- function(query=member(.X, ""[a, "b", 3L, 4, (pi), TRUE, .Y]),
										 quoted=FALSE)
{
	if(quoted)
		return(as.rolog1(query))
	
	as.rolog1(substitute(query))
}

as.rolog1 <- function(x)
{
	if(is.symbol(x))
	{
		# Anonymous variable
		if(x == ".")
			return(expression(`_`))
		
		if(substr(x, 1, 1) == ".")
			return(as.expression(as.symbol(substr(x, 2, nchar(x)))))
	}
	
	if(is.call(x))
	{
		args <- as.list(x)
		
		# Things like (2 + 3) or (a) are evaluated
		if(args[[1]] == "(")
			return(as.rolog(eval(args[[2]])))
		
		# `[`("", 1, 2, 3), aka. ""[1, 2, 3] is a list
		if(args[[1]] == "[" & length(args[[2]]) == 1 & args[[2]] == "")
			return(lapply(args[c(-1, -2)], FUN=as.rolog1))
		
		# list(1, 2, 3) is a list not a call
		if(args[[1]] == "list")
			return(lapply(args[-1], FUN=as.rolog1))
		
		args[-1] <- lapply(args[-1], FUN=as.rolog1)
		return(as.call(args))
	}
	
	if(is.list(x))
		return(lapply(x, FUN=as.rolog1))
	
	return(x)
}
