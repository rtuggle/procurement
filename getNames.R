getNames <- function(node) {
    n <- c()
    for(i in 2:len){
        x <- children[[i]]
        n <- c(n, names(xmlSApply(x[[node]], xmlValue)))
    } 
    return(unique(n))
}