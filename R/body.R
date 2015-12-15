#' @export
checkmark <- function(vec){
  vec <- as.logical(vec)
  ifelse(isTRUE(vec), "\\cmark", "\\xmark")
}

#' @export
footnote <- function(str){
  footnote <- list(footnote = str)
  new("tisch_annotation", annotations = footnote)
}

#' @export
caption <- function(str){
  caption <- list(caption = str)
  new("tisch_annotation", annotations = caption)
}

#' @export
label <- function(str){
  #if(str != "character") stop("Annotation must be character")
  
  label <- list(label = str)
  new("tisch_annotation", annotations = label)
}

# tbody <- function(...){
#   
#   args <- dots(...)
#   
#   return(args)
#   
#   body_aes_list <- c("box", "text")
#   not_matched <- setdiff(names(args), body_aes_list)
#   not_matched  <- sprintf("[%s]", paste(not_matched, collapse = ""))
#   
#   warning(sprintf("No %s", not_matched), call. = FALSE)
#   
#   body.args <- args[body_aes_list]
#   # box_args <- eval(box)
#   #new("tisch_adds", x = body_args)
#   return(body.args)
# }

# fu <- body(
#   box(fill = rowwise(.) %>% which.max(),
#       mark = which.max(),
#       bla = asdasd)
#   )
# fu


# list(
#   textnormal = "\textnormal", 
#   textroman = "\textrm",
#   textsf = "textsf", 
#   texttt = "texttt", 
#   textup = "textup", 
#   textit = "textit", 
#   textsl = "textsl", 
#   textsc = "textsc", 
#   uppercase = "uppercase", 
#   bold = "textbf", 
#   textmd = "textmd", 
#   textlf = "textlf")