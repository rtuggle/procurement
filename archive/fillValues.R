fillValues <- function(retrieve, nam){
    mat <- matrix("NULL", nrow=1, ncol=length(nam))
    colnames(mat) <- nam
    #output XML values to list
    var <- xmlApply(idv[[retrieve]], xmlValue)
    #cycle through names updating data if found
    for(j in 1:length(nam)){
        if(nam[j] %in% names(var)) {
            tryCatch(mat[1, nam[j]] <- var[[nam[j]]]
                     , error = function(e){ 
                         stop(paste(retrieve, "breaks first at line",j))
                         }
                     )
        }
    }
    return(mat)
}
