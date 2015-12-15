#' Full row
#' @export
full <- function(){
  
  function(struc){
    
    # --------
    # Construct body
    # --------
    df <- struc$body
    body_str <- lapply(1:nrow(df), function(i) paste(as.character(df[i,]), collapse = " & "))
    Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = " \\\\\\\\\\ \n\t")
    df <- Reduce(Reduce_paste, body_str)
    
    struc$body <- df
    struc$header$row.length <- length(struc$rows$names)
    
    return(struc)
  }
}



#' Loose row
#' @export
loose <- function(){
  
  function(struc){
    
    body <- struc$body
    #rownames <- head(struc$rows$names, -1)
    rownames <- head(struc$rows$names)
    
    for(i in 1:length(rownames)){
      
      rljj <- rle(body[, rownames[i]])
      
      cumsums <- cumsum(rljj$lengths)
      cumsums <- cumsums[-length(cumsums)]
      positions <- c(1, 1+cumsums)
      
      body[-positions, rownames[i]] <- ""
    }
    
    # --------
    # Construct body
    # --------
    df <- body
    body_str <- lapply(1:nrow(df), function(i) paste(as.character(df[i,]), collapse = " & "))
    Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = " \\\\\\\\\\ \n\t")
    df <- Reduce(Reduce_paste, body_str)
    
    # -----------
    # Construct struc
    struc$body <- df
    struc$header$row.length <- length(struc$rows$names)
    
    return(struc)
  }
  
}

#' Creates a ragged row style
#' @param indent the indention level
#' @export
ragged <- function(indent = 0.25){
  
  if(!is.numeric(indent)) stop("Indention value must be numeric", call. = FALSE)
  if(indent<0) stop("Indention value must be >= 0", call. = FALSE)
  
  function(struc, indent_ = indent){
    
    body <- struc$body
    rownames <- head(struc$rows$names, -1)
    
    if(length(rownames) < 1){
      stop("To apply ragged row style need at least two row variables.", call. = FALSE)
    }
    
    body <- lapply(seq_along(rownames), function(i){
      unique.vals <- unique(body[, rownames[i]])
      split_df <- split(body, ordered(body[, rownames[i]], levels = unique.vals))
      
      each_row_df <- lapply(seq_along(split_df), function(ii){
        df <- split_df[[ii]]
        df <- df[, !(colnames(df) %in% rownames[i])]
        indentstr <- sprintf("\\\\hspace*{%.2fcm}", i*indent_)
        df[,1] <- paste(indentstr, df[,1])
        df <- rbind("", df)
        df[1,1] <- names(split_df)[ii]
        
        return(df)
        })
      
      do.call("rbind", each_row_df)
      })
    
    df <- do.call("rbind", body)
    
    # --------
    # Construct body
    # --------
    body_str <- lapply(1:nrow(df), function(i) paste(as.character(df[i,]), collapse = " & "))
    Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = " \\\\\\\\\\ \n\t")
    df <- Reduce(Reduce_paste, body_str)
    
    # -----------
    # Construct struc
    struc$body <- df
    struc$header$row.length <- 1
    
    return(struc)
    }
}


#' Create a spanend row style
#' @param which  asd
#' @param ... additional arguments passed to nowehere
#' @export
spanned <- function(which = 1, ...){
  
  dots <- list(...)
  
  function(struc){
    
    body <- struc$body
    rownames <- head(struc$rows$names, -1)
    
    body <- lapply(seq_along(rownames), function(i){
      unique.vals <- unique(body[, rownames[i]])
      hoho <- split(body, ordered(body[, rownames[i]], levels = unique.vals))
      
      sos <- lapply(seq_along(hoho), function(ii){
        df <- hoho[[ii]]
        df <- df[, !(colnames(df) %in% rownames[i])]
        span.length <- ncol(df)
        span.str <- sprintf("\\\\rule{0pt}{14pt}\\\\multicolumn{%s}{c}{%s}", span.length, names(hoho)[ii])
        
        # --------
        # Construct body
        # --------
        body_str <- lapply(1:nrow(df), function(i) paste(as.character(df[i,]), collapse = " & "))
        body_str  <- c(list(span.str), body_str)
        Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = " \\\\\\\\\\ \n\t")
        df <- Reduce(Reduce_paste, body_str)
        
        # -----------
        # Construct struc
        struc$body <- df
        struc$header$row.length <- 0
        
        return(struc)
      })
      
      paste(unlist(sos), collapse = "\\\\\\\\\\ \n\t")
    })
    
    return(body[[1]])
  }
  
}


#' Hierarchical column style
#' @export
hierarchical <- function(){
  
  function(obj_ev){
    
    offset.left <- obj_ev@struc$header$row.length
    tex_offset <- paste(rep("& ", offset.left), collapse = "")
    
    col.vars <- obj_ev@struc$cols$vals
    row.vars <- obj_ev@struc$rows$vals
    length.vars <- sapply(col.vars, length)
    body.col.length <- length.vars[1]
    
    these <- which(col.vars[[2]] == "NA")
    col.vars[[2]][these] <- col.vars[[1]][these]
    col.vars[[1]][these] <- ""
    
    header1 <- paste(rev(col.vars)[[1]], collapse = " & ")
    header1 <- paste(tex_offset, header1, " \\\\\\\\\\")
    
    # Higher level vars
    var2.vars <- col.vars[-length(col.vars)]
    
    if(length(var2.vars) > 0){
      header2 <- multicol_header(col.vars = var2.vars, offset.left)
      } else {
        header2 <- ""
        }
    
    obj_ev@struc$header$header_str <- paste(header2, header1, sep = " ")
    
    # -----------
    # Construct struc
    obj_ev@struc$header$col.length <- 1
    
    return(obj_ev@struc)
    }
  
}

#' Pasted column style
#' @param sep character, separater between level values
#' @export
pasted <- function(sep = ":"){
  
  function(struc, sep_ = sep){
    
    offset.left <- struc$header$row.length
    tex_offset <- paste(rep("& ", offset.left), collapse = "")
    
    col.vars <- struc$cols$vals
    row.vars <- struc$rows$vals
    length.vars <- sapply(col.vars, length)
    body.col.length <- length.vars[1]
    
    Reduce_paste <- function(vec1, vec2) paste(vec1, vec2, sep = sep_)
    header_str <- Reduce(Reduce_paste, col.vars)
    header_str <- paste(header_str, collapse = " & ")
    struc$header$header_str <- paste(tex_offset, header_str, "\\\\\\\\")
    
    # Colnames
    header.length <- length(rev(struc$cols$vals)[[1]])
    theme$column_width
    header_dim_str <- paste(rep("c", header.length), collapse = "")
    row_dim_str <- paste(rep("l", length(struc$rows$vals)), collapse = "")
    struc$header$dim_str <- paste(row_dim_str, header_dim_str, sep = "")
    
    return(struc)
  }
  
}


#' Column Width
#' @param sep character, separater between level values
#' @export
fixed_width <- function(cm = NULL, pt = NULL){
  
  function(centimeters = cm, points = pt){
    
    if(!is.null(centimeters)){
      string <- sprintf("%scm", centimeters)
    } else {
      if(!is.null(points)){
        string <- sprintf("%spt", points)
      }
    }
    
    string <- sprintf("p{%s}", string)
    return(string)
  }
}

