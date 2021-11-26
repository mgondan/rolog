myquote = function(x)
{
	myq(substitute(x))
}

myq = function(x)
{
	if(is.name(x))
	{
		n = substr(as.character(x), 1, 1)
		
		# Variable
		if(n == toupper(n))
			return(as.expression(x))

		if(n == "_")
			return(as.expression(x))
	}

	if(is.call(x) & x[[1]] == "list")
	{
		args = as.list(x)
		return(lapply(args[-1], FUN=myq))
	}
	
	if(is.call(x))
	{
		args = as.list(x)
		args[-1] = lapply(args[-1], FUN=myq)
		return(as.call(args))
	}
	
	return(x)
}

# myquote(dbinom(`_k`, N, pi))
