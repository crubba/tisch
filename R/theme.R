#' Theming
#' @export
theme <- function(...){
  elements <- list(...)
  theme_vals <- c("row_style", "text_size", "border_size",
                  "border_color", "border_radius", "cell_padding",
                  "header_font_size", "header_font", "body_font_size",
                  "footer_font_size", "foot_font", "midrule")
  
  not_matched <- setdiff(names(elements), theme_vals)
  not_matched <- sprintf("[%s]", paste(not_matched, collapse = ", "))
  if(length(not_matched) > 0) warning(sprintf("Arguments %s could not be matched", not_matched), call. = FALSE)
  
  theme_str <- intersect(names(elements), theme_vals)
  elements <- elements[theme_str]
  
  new("tisch_theme", theme = elements)
}

#' New york times table theme
#' @export
theme_nyt <- function(...){
  
  theme_list <- list(...)
  
  new("tisch_theme", x = theme_list)
}


#' Basic table theme
#' @export
theme_base <- function(...){
  
  theme_list <- list(
    row_style = steprow(indent = 0.25),
    text_size = "\\normal",
    border_size = 1, 
    border_color = "black", 
    border_radius = "1px", 
    cell_padding = "5px", 
    header_font_size = "12",
    header_font = "Helvetica",
    body_font_size = "12",
    footer_font_size = "11",
    foot_font = "bold",
    midrule = c("a", "b")
    )
}

#' New york times table theme
#' @export
theme_nyt <- function(...){
  
  theme_list <- list(border_size = 1, 
                     border_color = "black", 
                     border_radius = "1px", 
                     cell_padding = "5px", 
                     header_font_size = "12",
                     header_font = "Helvetica",
                     body_font_size = "12",
                     footer_font_size = "11",
                     foot_font = "bold",
                     midrule = c("a", "b"))
  
  new("tisch_theme", x = theme_list)
}
