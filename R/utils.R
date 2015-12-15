
#' Paste two stringr
#' @param str1 string 1
#' @param str2 string 2
`%str%` <- function(str1, str2){
  paste(str1, str2)
}

#' Evaluate dots
#' @param ... additional arguments passed to eval
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' Extracts elements from a list based on index
#' @param l the list
#' @param ext the index
#' @param ... addtional arguments
list_get <- function(l, ext, ...){
  unlist(lapply(l, function(x) paste(x[ext], ...)))
}

#' Trim strings
#' @param x the string
#' @export
strtrim2 <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Reorder a data frame
#' @param df the data frame
#' @param vec the vector by which the data frame is sorted
#' @export
reorder_df <- function(df, vec){
  l.vec <- length(vec)
  colNames <- colnames(df)
  pos.vec <- match(vec, colNames)
  seq.colNames <- seq_along(colNames[-pos.vec])
  seq.colNames <- c(pos.vec, seq.colNames)
  
  df[, seq.colNames]
}

#' @export
escape <- function(x){
  x <- gsub("%", "\\\\\\\\%", x)
  x
}

which.na <- function(x){
  which(is.na(x))
}



