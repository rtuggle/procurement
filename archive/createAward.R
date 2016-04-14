createAward <- function(file) {
    data <- xmlTreeParse(file)
    root <- xmlRoot(data)
    children <- xmlChildren(root)
    #retrieve the number of nodes in the file
    #note: the first node is a count of entries, the rest are awards
    len <- length(xmlApply(root, xmlSize))
    #initialize matrices to hold the data elements
    matAwardID <- matrix("awardID", nrow = len - 1, ncol = 4)
    matDate <- matrix("contractDate", nrow = len - 1, ncol = 4)
    matDollar <- matrix("dollarValue", nrow = len - 1, ncol = 3)
    matPurchaser <- matrix("purchaserInfo", nrow = len -1, ncol = 5)
    matContract <- matrix("contractInfo", nrow = len -1, ncol = 16)
    #loop through every award, note: awards begin at node 2
    for(i in 2:len) {
        award <- children[[i]]
        ##AWARDID
        #retrieve nested elements of the award
        awardID <- award[[1]][[1]]
        #write values to character vector
        vecAwardID <- xmlSApply(awardID, xmlValue)
        #overwrite matrix row with vector 
        matAwardID[i - 1, ] <- vecAwardID 
        ##DATES
        vecDate <- xmlSApply(award[[2]], xmlValue)
        matDate[i - 1, ] <- vecDate
        ##DOLLARS
        vecDollar <- xmlSApply(award[[3]], xmlValue)
        matDollar[i - 1, ] <- vecDollar
        ##PURCHASER
        vecPurchaser <- xmlSApply(award[[4]], xmlValue)
        matPurchaser[i - 1, ] <- vecPurchaser
        ##CONTRACT
        #remove the list of treasury account elements from the vector [-14]
        #vecContract <- xmlSApply(award[[6]], xmlValue)[-14]
        #if(length(vecContract) == 16) {
         #   matContract[i - 1, ] <- vecContract 
        #} 
        #else {
         #   matContract[i - 1, ] <- rep("ERROR", times = 16)
        #}     
    }
    #add names to matrices
    colnames(matAwardID) <- names(vecAwardID)
    colnames(matDate) <- names(vecDate)
    colnames(matDollar) <- names(vecDollar)
    colnames(matPurchaser) <- names(vecPurchaser)
    #colnames(matContract) <- names(vecContract)
    data.frame(matAwardID, matDate, matDollar, matPurchaser)
}