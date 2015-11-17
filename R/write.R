#' Write the tisch object to the file system
#' @param x tisch object
#' @param filename a file path string
#' @export
write_tisch <- function(x, filename){
  
  filename <- normalizePath(filename)
  
  fileConn <- file(filename)
  writeLines(x, fileConn)
  close(fileConn)
  
  invisible()
}