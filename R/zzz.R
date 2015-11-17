.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()
  
#   cite_text <- paste(as.character(toBibtex(citation("tisch"))), collapse = "\n")
#   tips_cite <- sprintf("Was this package useful? Consider citing it:\n %s", cite_text)
  tips <- c(
    "If you find a bug, please report it to 'https://github.com/crubba/tisch/issues'\nUse suppressPackageStartupMessages to eliminate package startup messages."
  )
  tip <- sample(tips, 1)
  packageStartupMessage(tip)
}
