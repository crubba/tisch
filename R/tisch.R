#' Create a new tisch object
#' tisch initializes a new tisch object. Designate rows and columns via its formula interface.
#' @param data a data frame
#' @param formula casting formula, see \link[reshape2]{cast} for specifics
#' @param reorder logical, allow the casting function to reorder the data frame?
#' @param ... arguments passed to the casting function
#' @examples
#' \dontrun{
#' data(asd)
#' tt <- tisch(data, asd + asd ~ asd)
#' to_tex(tt)
#' 
#' tt + caption("asd") + footnote("ASd") +
#' theme(asd)
#' to_tex(tt)
#' 
#' }
#' @export
#' @import stringr
tisch <- function(data, formula, reorder = FALSE, ...){

  # ------
  # Formula and cols/rows var names
  # ------
  form <- deparse(substitute(Level1.var + Level1.quest ~ var + quest))
  form <- strsplit(form, "~")
  
  rows <- strtrim(form[[1]][1])
  row.names <- strtrim(unlist(strsplit(rows, "\\+")))
  row.order <- data[, row.names]
  row.order$merged <- apply(row.order, 1, paste, collapse = " ")
  row.order <- unique(row.order$merged)
  
  cols <- strtrim(form[[1]][2])
  col.names <- strtrim(unlist(strsplit(cols, "\\+")))
  col.order <- data[, col.names]
  col.order$merged <- apply(col.order, 1, paste, collapse = " ")
  col.order <- unique(col.order$merged)
  
  # ------
  # Body
  # ------
  cdata <- reshape2::dcast(data, Level1.var + Level1.quest ~ var + quest)
  
  if(!is.data.frame(cdata)) stop("Cast object has to be a data frame", call. = FALSE)
  
  if(!isTRUE(reorder)){
    cdata <- reorder_df(df = cdata,
                        row.names = row.names, 
                        row.order = row.order,
                        col.order = col.order)
    }
  
  cdata <- data.frame(lapply(cdata, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  
  # ------
  # Rows
  # ------
  row.vals <- as.list(cdata[, row.names, drop = FALSE])
  rows <- list(names = row.names, vals = row.vals)
  
  # ------
  # Cols
  # ------
  str <- colnames(cdata)[-(1:length(rows$vals))]
  col.vals <- split_str(str)
  cols <- list(names = col.names, vals = col.vals)

  # ----------
  # Create tisch object
  # ----------
  struc_list <- list(rows = rows, cols = cols, body = cdata, header = colnames(cdata))
  annotations_list <- list(caption = NULL, footnote = NULL, label = NULL)
  theme_list <- theme_base()

  tisch_base <- new("tisch", 
                    struc = struc_list,
                    annotations = annotations_list,
                    theme = theme_list)
  
  invisible(tisch_base)
}


#' Split vars
split_str <- function(str){
  splitted <- strsplit(str, "_")
  cols <- max(lengths(splitted))
  mat <- do.call(rbind, lapply(splitted, "length<-", cols))
  
  lapply(1:cols, function(i){
    list_get(splitted, i)
  })
}


#' Reorder the df
#' @export
#' @import stringr
reorder_df <- function(df, row.names, row.order, col.order){
  
  # Ordering rows
  df$merged_var <- apply(df[, row.names], 1, paste, collapse = " ")
  df$merged_var <- ordered(df$merged_var, levels = row.order)
  df <- df[order(df$merged_var), ]
  df$merged_var <- NULL
  
  # Order columns
  merge_df <- data.frame(merge.var = gsub("_", " ", colnames(df)),
                         colname = colnames(df), stringsAsFactors = FALSE)
  col.order <- c(row.names, col.order)
  merge_df <- merge_df[match(col.order, merge_df$merge.var),]
  
  df <- df[, merge_df$colname]
  
  return(df)
}


#cols.value <- as.list(unique(data[,cols.name])) #lapply(cols.name, function(str) unique(data[,str]))

# cols.str <- colnames(cdata)[-1]


#   ddata <- reshape2::dcast(data,  fach  ~ type + variable)[,-1]
#   cols.str <- list(colnames(ddata))  
#   x <- cols.str
#   
#   rapply(cols.str, function(x) {
#     cols.list <- lapply(x, function(yo) str_split(yo, "_"))
#     cols.split <- list_get(cols.list, i)
#     i <<- i + 1
#     split(x, factor(cols.split, levels = unique(cols.split)))
#   }, how = "replace")
