#' Theming
#' @param ... theme elements
#' @export
theme <- function(...){
  elements <- list(...)
  theme_vals <- c("row_style", "column_style", "text_size", "column_justification", 
                  "row_width", "column_width", "replace_NA")
  
  not_matched <- setdiff(names(elements), theme_vals)
  
  if(length(not_matched) > 0) {
    not_matched <- sprintf("[%s]", paste(not_matched, collapse = ", "))
    warning(sprintf("Arguments %s could not be matched", not_matched), call. = FALSE)
  }
  
  theme_str <- intersect(names(elements), theme_vals)
  elements <- elements[theme_str]
  
  new("tisch_theme", theme = elements)
}

#' Basic table theme
#' @param ... theme elements
#' @export
theme_base <- function(...){
  
  theme_list <- list(
    row_style = loose(),
    column_style = hierarchical(),
    column_justification = "central",
    column_width = NULL,
    text_size = "normal",
    replace_NA = ""
    )
}

