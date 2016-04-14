getNames_vendor <- function(node) {
    n <- c()
    for(i in 2:len){
        x <- children[[i]]
        n <- c(n, names(xmlSApply(
            x[['vendor']][['vendorSiteDetails']]
            [[node]], xmlValue)))
    } 
    return(unique(n))
}