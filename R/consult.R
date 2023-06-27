#' Consult a prolog database
#' 
#' @param fname
#' file name of database
#'
#' @return
#' `TRUE` on success
#'
#' @md
#'
#' @seealso
#' [once()], [findall()], and [query()]/[submit()]/[clear()] for executing queries
#' 
#' @examples
#' consult(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
#' findall(call("ancestor", quote(pam), expression(X)))
#' 
consult <- function(fname=system.file(file.path("pl", "family.pl"), package="rolog"))
{
  if(.consult(fname))
    return(invisible(TRUE))
	
  return(FALSE)
}
