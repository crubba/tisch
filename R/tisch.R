#' Create a new tisch object
#' tisch initializes a new tisch object. Designate rows and columns via its formula interface.
#' @param data a data frame
#' @param formula casting formula, see \link[reshape2]{dcast} for specifics
#' @param cols which are the column variables?
#' @param rows which are the row variables?
#' @param sep if column are composites, what is the separator string?
#' @param reorder logical, allow the casting function to reorder the data frame?
#' @param ... arguments passed to the casting function
#' @details There exists two ways of specifying the row and column variables in tisch.
#' \itemize{
#'  \item{Long data frames}{}
#'  \item{Wide data frames}{}
#' }
#' @seealso \code{\link[reshape2]{dcast}}
#' @examples \dontrun{
#' data(tips)
#' tips2 <- tips[, c("day", "time", "sex", "smoker", "tip")]
#' 
#' tt <- tisch(tips2, day + time ~ sex + smoker, fun.aggregate = mean)
#' to_tex(tt)
#' 
#' tt + caption("asd") + footnote("ASd") +
#' theme(asd)
#' to_tex(tt)
#' }
#' @export
#' @import stringr
tisch <- function(data, formula = NULL, cols = NULL, rows = NULL, sep = "_", reorder = FALSE, ...){
  
  # Catch Dots
  #dots <- list(...)
  
  if(!is.null(formula)){
  
    # ------
    # Tisch by formula interface  
    # ------
    
    # Formula and cols/rows var names
    form <- deparse(substitute(formula))
    form <- strsplit(form, "~")
    
    rows <- strtrim2(form[[1]][1])
    row.names <- strtrim2(unlist(strsplit(rows, "\\+")))
    row.order <- data[, row.names, drop = FALSE]
    row.order$merged <- apply(row.order, 1, paste, collapse = " ")
    row.order <- unique(row.order$merged)
    
    cols <- strtrim2(form[[1]][2])
    col.names <- strtrim2(unlist(strsplit(cols, "\\+")))
    col.order <- data[, col.names, drop = FALSE]
    col.order$merged <- apply(col.order, 1, paste, collapse = " ")
    col.order <- unique(col.order$merged)
    
    # Body
    cdata <- reshape2::dcast(data, formula = formula)
    
    if(!is.data.frame(cdata)) stop("Cast object has to be a data frame", call. = FALSE)
    
    if(!isTRUE(reorder)){
      cdata <- reorder_df(df = cdata,
                          row.names = row.names, 
                          row.order = row.order,
                          col.order = col.order)
    }
    
    cdata <- data.frame(lapply(cdata, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    
    cdata <- data.frame(lapply(cdata, as.character), 
                        stringsAsFactors = FALSE, 
                        check.names = FALSE)
    
    # Rows
    row.vals <- as.list(cdata[, row.names, drop = FALSE])
    rows <- list(names = row.names, vals = row.vals)
    
    # Cols
    str <- colnames(cdata)[-(1:length(rows$vals))]
    col.vals <- split_str(str, split = "_")
    cols <- list(names = col.names, vals = col.vals)
    
    
  } else {
    
    # ------
    # Wide Tisch  
    # ------
    
    cdata <- data
    
    rows.index <- num_col_index(rows, cdata)
    cols.index <- num_col_index(cols, cdata)
    
    if(is.null(cols.index)){
      cols.index <- setdiff(seq(ncol(cdata)), rows.index)
    }
    
    # Rows
    row.names <- colnames(cdata)[rows.index]
    row.vals <- as.list(cdata[, row.names, drop = FALSE])
    rows <- list(names = row.names, vals = row.vals)
    
    # Cols
    col.names <- colnames(cdata)[cols.index]
    str <- colnames(cdata)[-(1:length(rows$vals))]
    col.vals <- split_str(str, split = sep)
    cols <- list(names = col.names, vals = col.vals)
  }
  

  # ----------
  # Create tisch object
  # ----------
  struc_list <- list(rows = rows, cols = cols, body = cdata, header = list(names = colnames(cdata)))
  annotations_list <- list(caption = NULL, footnote = NULL, label = NULL)
  theme_list <- theme_base()

  tisch_base <- new("tisch", 
                    struc = struc_list,
                    annotations = annotations_list,
                    theme = theme_list)
  
  invisible(tisch_base)
}


#' Split vars
#' @param str asd
#' @param split splitting string
#' @export
split_str <- function(str, split){
  splitted <- strsplit(str, split = split)
  cols <- max(lengths(splitted))
  mat <- do.call(rbind, lapply(splitted, "length<-", cols))
  
  lapply(1:cols, function(i){
    list_get(splitted, i)
  })
}


#' Reorder the df
#' @export
#' @param df the data frame
#' @param row.names the row names
#' @param row.order the row order
#' @param col.order the col.order
#' @import stringr
reorder_df <- function(df, row.names, row.order, col.order){
  
  # Ordering rows
  df$merged_var <- apply(df[, row.names, drop = FALSE], 1, paste, collapse = " ")
  df$merged_var <- ordered(df$merged_var, levels = row.order)
  df <- df[order(df$merged_var), ]
  df$merged_var <- NULL
  
  # Order columns
  merge_df <- data.frame(merge.var = gsub("_", " ", colnames(df)),
                         colname = colnames(df), 
                         stringsAsFactors = FALSE)
  col.order <- c(row.names, col.order)
  merge_df <- merge_df[match(col.order, merge_df$merge.var),]
  
  df <- df[, merge_df$colname]
  
  return(df)
}

#' Getting index asd
#' @param arg argument
#' @param data data frame
num_col_index <- function(arg, data) UseMethod("num_col_index")

num_col_index.default <- function(arg, data) {
  NULL
}

num_col_index.call <- function(arg, data){
  
  names <- deparse(substitute(arg))
  names <- unlist(strsplit(names, ":"))
  index <- match(names, colnames(data))
  if(length(index) > 1) {
    index[1]:index[2]
  } else {
    index
  }
  
}


num_col_index.character <- function(arg, data){
  
  index <- match(arg, colnames(data))
  if(length(index) != length(arg)) {
    stop(sprintf("Coudn't match variables [%s] to names in data frame", arg))
  }
  index
  
}

num_col_index.numeric <- function(arg, data){
  
  names <- deparse(substitute(arg))
  names <- unlist(strsplit(names, ":"))
  index <- match(names, colnames(data))
  if(length(index) > 1) {
    index[1]:index[2]
  } else {
    index
  }
  
}

# gg <- function(x){
#   substitute(x)
# }
# gg("A")
# gg(1)
# yo <- gg(a:b)
# yo
# class(yo)
