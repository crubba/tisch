#' @export
loose_rows <- function(struc, row.vars){
  
  body <- struc$body
  nest.vars <- row.vars[-length(row.vars)]
  nest.vars <- lapply(nest.vars, function(vec){
    
    rljj <- rle(vec)
    
    unlist(lapply(seq_along(rljj$values), function(i){
    c(rljj$values[i], rep("", rljj$lengths[i]-1))
      })
    )
    })
  
  for(i in 1:length(nest.vars)){
    body[,i] <- nest.vars[[i]]
  }
  
}

#' Create a stepped row styel
#' @param indent the indention level
#' @export
steprow <- function(indent){
  
  force(indent)
  
  if(!is.numeric(indent)) stop("Indention value must be numeric", call. = FALSE)
  
  function(struc, indent_ = indent){
    
    body <- struc$body
    rownames <- head(struc$rows$names, -1)
    
    body <- lapply(seq_along(rownames), function(i){
      unique.vals <- unique(body[, rownames[i]])
      hoho <- split(body, ordered(body[, rownames[i]], levels = unique.vals))
      
      sos <- lapply(seq_along(hoho), function(ii){
        df <- hoho[[ii]]
        df <- df[, !(colnames(df) %in% rownames[i])]
        indentstr <- sprintf("\\\\hspace*{%.2fcm}", i*indent_)
        df[,1] <- paste(indentstr, df[,1])
        df <- rbind("", df)
        df[1,1] <- names(hoho)[ii]
        
        return(df)
        })
      
      do.call("rbind", sos)
      })
    
    body <- do.call("rbind", body)
    
    return(body)
  }
}

