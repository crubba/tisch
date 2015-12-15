setGeneric("+")

setClass("tisch",
         slots = list(struc = "list", annotations = "list", theme = "list"))

setClass("tisch_annotation",
         slots = list(annotations = "list"))

setClass("tisch_theme",
         slots = list(theme = "list"))

setClass("tisch_body",
         slots = list(body = "list"))

setMethod("+", signature(e1 = "tisch", e2 = "tisch_annotation"),
          function (e1, e2){
            
            common_vars <- intersect(names(slot(e1 , "annotations")), names(slot(e2 , "annotations")))
            new_annon <- replace(slot(e1 , "annotations"), 
                                 common_vars, 
                                 slot(e2 , "annotations"))
            e1@annotations <- new_annon
            return(e1)
          }
)

setMethod("+", signature(e1 = "tisch", e2 = "tisch_theme"),
          function (e1, e2){
            
            common_vars <- intersect(names(slot(e1 , "theme")), names(slot(e2 , "theme")))
            new_theme <- replace(slot(e1 , "theme"), 
                                 common_vars, 
                                 slot(e2 , "theme"))
            
            e1@theme <- new_theme
            return(e1)
          }
)


setMethod("+", signature(e1 = "tisch_theme", e2 = "tisch_theme"),
          function (e1, e2){
            
            common_vars <- intersect(names(slot(e1 , "theme")), names(slot(e2 , "theme")))
            new_theme <- replace(slot(e1 , "theme"), 
                                 common_vars, 
                                 slot(e2 , "theme"))
            
            e1@theme <- new_theme
            return(e1)
          }
)


setMethod("+", signature(e1 = "tisch_theme", e2 = "tisch_annotation"),
          function (e1, e2){
            
            common_vars <- intersect(names(slot(e1 , "annotations")), names(slot(e2 , "annotations")))
            new_annon <- replace(slot(e1 , "annotations"), 
                                 common_vars, 
                                 slot(e2 , "annotations"))
            e1@annotations <- new_annon
            return(e1)
          }
)