
#' Paste two stringr
`%str%` <- function(str1, str2){
  paste(str1, str2)
}

#' Evaluate dots
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' Extracts elements from a list based on index
#' @param l the list
#' @param ext the index
list_get <- function(l, ext, ...){
  unlist(lapply(l, function(x) paste(x[ext], ...)))
}

#' Trim strings
strtrim <- function (x) gsub("^\\s+|\\s+$", "", x)