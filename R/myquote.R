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
		if(n == toupper(n) & n != tolower(n))
			return(as.expression(x))

		if(n == "_")
			return(as.expression(x))
	}

	if(is.call(x))
	{
		args = as.list(x)
		args[-1] = lapply(args[-1], FUN=myq)
		
		# list(1, 2, 3) is a list not a call
		if(args[[1]] == "list")
			return(args[-1])

		return(as.call(args))
	}
	
	return(x)
}

# myquote(dbinom(`_k`, N, pi))
