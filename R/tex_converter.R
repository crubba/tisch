#' Converts a tisch object to LaTeX output
#' @param obj the tisch object
#' @export
#' @import infuser
to_tex <- function(obj, ...) UseMethod("to_tex")

#' @export
to_tex.tisch <- function(obj, ...){
  
  # --------
  # Catch dots
  elements <- list(...)

  #  --------
  # Assemble
  #  --------
  struc <- obj@struc
  
  offset.left <- 1
  col.vars <- struc$cols$vals
  row.vars <- struc$rows$vals 
  length.vars <- sapply(col.vars, length)
  body.col.length <- length.vars[1]
  
  # ---------
  # Theme Elements
  # ---------
  theme <- obj@theme
  textsize <- theme$textsize
  textsize <- "\\\\normal"
  
  # ------
  # Cols
  # ------
  
  # Lowest level var
  tex_offset <- paste(rep("& ", offset.left), collapse = "")
  header1 <- paste(rev(col.vars)[[1]], collapse = " & ")
  header1 <- paste(tex_offset, header1, " \\\\\\\\\\")
  
  # Higher level vars
  var2.vars <- col.vars[-length(col.vars)]
  
  if(length(var2.vars) > 0){
    header2 <- multicol_header(col.vars = var2.vars, offset.left)
  } else {
      header2 <- ""
      }
  
  header_str <- paste(header2, header1, sep = " ") #\\\\
  
  # Colnames
  header.length <- length(rev(struc$cols$vals)[[1]])
  header_dim_str <- paste(rep("c", header.length), collapse = "")
  row_dim_str <- paste(rep("l", length(struc$rows$vals)), collapse = "")
  dim_str <- paste(row_dim_str, header_dim_str, sep = "")
  
  # ------
  # Rows
  # ------
  struc$body <- theme$row_style(struc = struc)

  # ------
  # Body
  # ------
  body_str <- lapply(1:nrow(struc$body), function(i) paste(as.character(struc$body[i,]), collapse = " & "))
  Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = " \\\\\\\\\\ \n\t")
  body_str <- Reduce(Reduce_paste, body_str)
  
  
  # ------
  # Annotations
  # ------  
  annotations <- obj@annotations
  
  # Footnote 
  footnote <- annotations$footnote
  ifelse(!is.null(footnote), sprintf("\\footnote{%s}", footnote), "")
  
  # Caption
  caption <- annotations$caption
  caption <- ifelse(!is.null(caption), sprintf("\\\\caption{%s}", caption), "")
  
  # Label
  label <- annotations$label
  label <- ifelse(!is.null(label), sprintf("\\\\label{%s}", label), "")
  
  
  # ----------
  # Assemble the TeX Code
  # ----------
  tex_template <- "\\begin{table}{{textsize}}
\t{{caption}}{{label}}
\\centering
\\begin{tabular}{ {{dim_str}} }
\t \\toprule
\t {{header_str}}
\t \\midrule
\t {{body_str}} \\\\
\t \\bottomrule
\t \\end{tabular}
\t \\end{table}"
  
  tab_tex <- infuse(tex_template,
                textsize = textsize,
                caption = caption,
                label = label,
                dim_str = dim_str,
                header_str = header_str,
                body_str = body_str)
  
  
  structure(tab_tex, class = "tisch_out")
}

#' Printing method for tisch code
print.tisch_out <- function(str){
  cat(tab_tex)
}


#' Create multicolumn TeX headers
#' @param col.vars the column vars
#' @param offset.left offset from the rows variable
#' @export
multicol_header <- function(col.vars, offset.left){
  
  header2 <- lapply(col.vars, function(col){
    rlecol <- rle(col) 
    splits <- c(0, cumsum(rlecol$lengths)) + offset.left + 1
    splits <- sapply(1:(length(splits)-1), function(i) sprintf("%g-%g", splits[i], splits[i+1]-1))
    
    value_str <- paste(sprintf("\\\\multicolumn{%s}{c}{%s}", rlecol$lengths, rlecol$values), collapse = " & ")
    value_str <- paste(paste(rep(" & ", offset.left), collapse = ""), value_str, sep = "")
    midrule_str <- paste(sprintf("\\\\cmidrule(r){%s}", splits), collapse = " ")
    #midrule_str <- paste(paste(rep(" & ", 2), collapse = ""), midrule_str, sep = "")
    
    paste(value_str, midrule_str, sep = "\\\\\\\\\\")
  })
  
  header2 <- paste(header2, collapse = "\\\\\\\\\\")

  return(header2)
}


