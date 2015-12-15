#' Converts a tisch object to LaTeX output
#' @param obj the tisch object
#' @param position character, position of the table in the page, "t" for top (default), 
#' "b" for bottom, "h" for here
#' @param rotation numeric, number od degrees for table rotation
#' @param split character, style of splitting across multiple pages
#' @param open, logical
#' @param ... additional arguments passed to
#' @import infuser
#' @export
to_tex <- function(obj, position, rotation, split, open, ...) UseMethod("to_tex")

#' @export
to_tex.tisch <- function(obj, position = "tbp", rotation = 0, split = NULL, open = FALSE, ...){
  
  # --------
  # Check inputs
  if(!grepl("^[tbph]", position)) {
    warning("Unknown table position. Using [tbp].", call. = FALSE)
    position <- "tbp"
  }
  
  if(!is.numeric(rotation)) stop("Rotation parameter must be numeric.", call. = FALSE)
  
  # --------
  # Catch dots
  elements <- list(...)
  
  # ---------
  # Theme Elements
  # ---------
  theme <- obj@theme
  

  # ------
  # Construct body
  # ------
  obj@struc$body <- data.frame(lapply(obj@struc$body, escape), check.names = FALSE, stringsAsFactors = FALSE)
  if(!is.null(theme$replace_NA)){
    obj@struc$body <- data.frame(lapply(obj@struc$body, function(x) gsub("NA", theme$replace_NA, x)), check.names = FALSE, stringsAsFactors = FALSE)
    obj@struc$body <- data.frame(lapply(obj@struc$body, function(x) {
      x[is.na(x)] <- theme$replace_NA
      x}), check.names = FALSE, stringsAsFactors = FALSE)
  }
  obj@struc <- theme$row_style(struc = obj@struc)
  
  #  --------
  # Assemble
  #  --------
  col.vars <- obj@struc$cols$vals
  row.vars <- obj@struc$rows$vals
  length.vars <- sapply(col.vars, length)
  body.col.length <- length.vars[1]
  
  # ------
  # Cols
  # ------
  
  # Lowest level var
  obj@struc <- theme$column_style(obj_ev = obj)
  
  # --------
  # Header dimension string
  # Justification string
  coljus <- theme$column_justification 
  if(is.null(coljus)) stop("column_justification needs argument where", call. = FALSE)
  if(!coljus %in% c("central", "left", "right")) {
      stop("Justification value must be central, left or right.", call. = FALSE)
    }
  
  coljus <- switch(coljus,
             central = "c",
             left = "l",
             right = "r")
  
  header.length <- length(rev(obj@struc$cols$vals)[[1]])
  header_dim_str <- paste(replicate(header.length, coljus, simplify = T), collapse = "")
  header_dim_str <- paste(header_dim_str, collapse = "") #rep("c", header.length)
  row_dim_str <- paste(rep("l", length(obj@struc$header$row.length)), collapse = "")
  obj@struc$header$dim_str <- paste(row_dim_str, header_dim_str, sep = "")
  
  # ------
  # Annotations
  # ------  
  annotations <- obj@annotations
  
  # Footnote 
  footnote <- annotations$footnote
  footnote <- ifelse(!is.null(footnote), sprintf("\\\\begin{tablenotes}
      \\\\small
      \\\\item %s
    \\\\end{tablenotes}", footnote), "")
  
  text_size <- theme$text_size
  
  table_position = sprintf("[%s]", position)
  
  # Caption
  caption <- annotations$caption
  caption <- ifelse(!is.null(caption), sprintf("\\\\caption{%s}", caption), "")
  
  # Label
  label <- annotations$label
  label <- ifelse(!is.null(label), sprintf("\\\\label{%s}", label), "")
  
  
  # ----------
  # Assemble the TeX Code
  # ----------
  #\\rowcolors{1}{green}{pink}
  tex_template <- "
\\begin{table}{{table_position}}\\begin{threeparttable}\\{{text_size}}
\t{{caption}}{{label}}
\\centering

\\begin{tabular}{ {{dim_str}} }
\t \\toprule
\t {{header_str}}
\t \\midrule
\t {{body_str}} \\\\
\t \\bottomrule
\t \\end{tabular}

\t {{footnote}}
\\end{threeparttable}
\\end{table}"
  
  tab_tex <- infuse(tex_template,
                    table_position = table_position,
                    text_size = text_size,
                    caption = caption,
                    label = label,
                    dim_str = obj@struc$header$dim_str,
                    header_str = obj@struc$header$header_str,
                    footnote = footnote,
                    body_str = obj@struc$body)
  
  if(isTRUE(open)){
    full_tex <- sprintf("
\\documentclass{article}\n
\\usepackage{booktabs}\n
\\begin{document}\n%s\n\\end{document}", tab_tex)
    
    #tmp_dir <- tempdir()
    #tmp_path <- tempfile(tmpdir = tmp_dir, fileext = ".tex")
    tmp_path <- "/home/christian/Dropbox/ff/ff.tex"
    pdf_path <- paste0(tools::file_path_sans_ext(tmp_path), ".pdf")
    write_tisch(x = full_tex, filename = tmp_path)
    
    run_tex <- sprintf("pdflatex \\\\nonstopmode\\\\input %s", tmp_path)
    system(run_tex, intern = TRUE)
    
    sys_call <- sprintf("gnome-open %s", pdf_path)
    system(sys_call, intern = TRUE)
  }
  
  structure(tab_tex, class = "tischout")
}

#' Printing method for tisch code
#' @export
#' @param str the text to be printed
print.tischout <- function(x, ...){
  cat(x, ...)
}


#' Create multicolumn TeX headers
#' @param col.vars the column vars
#' @param offset.left offset from the rows variable
#' @export
multicol_header <- function(col.vars, offset.left){
  
  header2 <- lapply(col.vars, function(col){
    rlecol <- rle(col)
    notempty <- which(rlecol$values  != "" )
   
    # Multicolumn 
    value_str <- paste(sprintf("\\\\multicolumn{%s}{c}{%s}", rlecol$lengths, rlecol$values), collapse = " & ")
    value_str <- paste(paste(rep(" & ", offset.left), collapse = ""), value_str, sep = "")
    
    #  Midrule
    splits <- c(0, cumsum(rlecol$lengths)) + offset.left + 1
    splits <- sapply(1:(length(splits)-1), function(i) sprintf("%g-%g", splits[i], splits[i+1]-1))[notempty]
    rlecol <- lapply(rlecol, function(x)x[notempty])
    midrule_splits <- which(rlecol$lengths > 1)
    midrule_str <- paste(sprintf("\\\\cmidrule(r){%s}", splits[midrule_splits]), collapse = " ")
    
    paste(value_str, midrule_str, sep = "\\\\\\\\\\")
  })
  
  header2 <- paste(header2, collapse = "\\\\\\\\\\")

  return(header2)
}


