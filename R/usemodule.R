#' Use a prolog module
#' 
#' @param fname
#' file name of module
#'
#' @return
#' `TRUE` on success
#'
#' @md
#'
#' @seealso
#' [consult()] for consulting a prolog file
#' 
#' @examples
#' usemodule(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
#' findall(call("ancestor", quote(pam), expression(X)))
#' 
usemodule <- function(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
{
  if(.usemodule(fname))
    return(invisible(TRUE))
	
  return(FALSE)
}
