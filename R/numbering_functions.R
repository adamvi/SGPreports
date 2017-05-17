tblNum <- function(advance.counter=0) {return(getOption("table_number")+advance.counter)}

#  Deprecate
tblNumNext <- function() {return(getOption("table_number")+1)}



tblNumIncrement <- function(advance.counter=1) {options("table_number" = getOption("table_number")+advance.counter); return(getOption("table_number"))}

tblCap <- function(caption.text=NULL, advance.counter=1) {
  if (as.integer(advance.counter) != 0L) tblNumIncrement(advance.counter=advance.counter)
  if (is.null(caption.text))  return(NULL)
  return(paste0("**Table ", getOption("table_number"), ":** ", caption.text))
}


#  Deprecate
tblCap_NO_Increment <- function(caption.text=NULL) {
  return(paste0("**Table ", getOption("table_number"), ":** ", caption.text))
}

#  Deprecate
tblCapNULL <- function(advance.counter=1) {
  options("table_number" = getOption("table_number")+advance.counter)
  return(NULL)
}

eqnNumNext <- function() {options("equation_counter" = getOption("equation_counter")+1); return(getOption("equation_counter"))}

eqnNum <- function(advance.counter=0, eqn.name="t1", em.space=150) {
  if (!is.null(eqn.name)) assign(eqn.name, getOption('equation_counter')+1, envir=.GlobalEnv) else getOption('equation_counter')+1
  return(cat('\\hspace{', em.space, 'em} \\text{(', getOption('equation_counter')+advance.counter, ')}', sep=""))
}
