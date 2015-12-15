#' Write the tisch object to the file system
#' @param x tisch object
#' @param filename a file path string
#' @param full logical, should the table be inserted into a complete text file?
#' @export
write_tisch <- function(x, filename, full = FALSE){
  
  x <- sprintf("
\\documentclass{article}\n
\\usepackage[ngerman]{babel}\n
\\usepackage{booktabs}\n
\\usepackage[flushleft]{threeparttable}\n
\\begin{document}\n%s\n\\end{document}", x)
  filename <- path.expand(filename)
  
  fileConn <- file(filename)
  writeLines(x, fileConn)
  close(fileConn)
  
  invisible()
}