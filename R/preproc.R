# R-to-Prolog translation of not equal etc.
.table = c("!=" = "\\=", "<=" = "=<")

#' Default hook for preprocessing
#' 
#' @param query 
#' the R call representing the Prolog query. 
#'
#' @return
#' The default hook translates the inequality and smaller-than-or-equal-to from
#' R (!=, <=) to Prolog (\=, =<).
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
preproc <- function(query=quote(1 <= 2))
{
	if(is.call(query))
	{
		args <- as.list(query)
		
		index <- which(args[[1]] == names(.table))
		if(length(index) == 1)
			args[[1]] <- as.name(.table[index])
		
		args[-1] <- lapply(args[-1], FUN=preproc)
		return(as.call(args))
	}
	
	if(is.list(query))
		return(lapply(query, FUN=preproc))
	
	return(query)
}

#' Default hook for postprocessing
#' 
#' @param query 
#' the R call representing the Prolog query. 
#'
#' @return
#' The default hook translates the inequality and smaller-than-or-equal-to back
#' from Prolog (\=, =<) to R (!=, <=).
#'
#' @seealso [rolog_options()] for fine-grained control over the translation
#' 
postproc <- function(query=call("=<", 1, 2))
{
	if(is.call(query))
	{
		args <- as.list(query)
		
		index <- which(args[[1]] == .table)
		if(length(index) == 1)
			args[[1]] <- as.name(names(.table)[index])
		
		args[-1] <- lapply(args[-1], FUN=preproc)
		return(as.call(args))
	}
	
	if(is.list(query))
		return(lapply(query, FUN=preproc))
	
	return(query)
}

.postproc_list <- function(constraints=list(call("=<", 1, 2)), postproc=postproc)
{
	if(is.list(constraints))
		return(lapply(constraints, FUN=postproc))
	
	return(constraints)
}
