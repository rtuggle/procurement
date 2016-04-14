createIdv <- function(file) {
    data <- xmlTreeParse(file)
    root <- xmlRoot(data)
    children <- xmlChildren(root)
    #retrieve the number of nodes in the file
    #note: the first node is a count of entries, the rest are awards
    len <- length(xmlApply(root, xmlSize))
    #initialize matrices to hold the data elements
    matAwardID <- matrix("awardID", nrow = len - 1, ncol = 3)
    matDate <- matrix("contractDate", nrow = len - 1, ncol = 3)
    matDollar <- matrix("dollarValue", nrow = len - 1, ncol = 2)
    matPurchaser <- matrix("purchaserInfo", nrow = len -1, ncol = 5)
    matContract <- matrix("contractInfo", nrow = len -1, ncol = 15)
    #loop through every award, note: awards begin at node 2
    for(i in 2:len) {
        idv <- children[[i]]
        ##AWARDID
        #retrieve nested elements of the award
        awardID <- idv[[1]][[1]]
        #write values to character vector
        vecAwardID <- xmlSApply(awardID, xmlValue)
        #overwrite matrix row with vector 
        matAwardID[i - 1, ] <- vecAwardID 
        ##DATES
        vecDate <- xmlSApply(idv[[2]], xmlValue)
        matDate[i - 1, ] <- vecDate
        ##DOLLARS
        vecDollar <- xmlSApply(idv[[3]], xmlValue)
        matDollar[i - 1, ] <- vecDollar
        ##PURCHASER
        vecPurchaser <- xmlSApply(idv[[4]], xmlValue)
        matPurchaser[i - 1, ] <- vecPurchaser
        ##CONTRACT
        vecContract <- xmlSApply(idv[['contractData']], xmlValue)
        c.names <- names(vecContract)
        #remove the list of Treasury Accounts
        c.names <- c.names[c.names!='listOfTreasuryAccounts']
        #add names to matrix first time
        if(i == 2) {
            colnames(matContract) <- c.names
        }
        #cycle through names
        for(j in 1:length(c.names)){
            try(matContract[i -1, c.names[j]] <- vecContract[c.names[j]])
        }
    }
    #add names to matrices
    colnames(matAwardID) <- names(vecAwardID)
    colnames(matDate) <- names(vecDate)
    colnames(matDollar) <- names(vecDollar)
    colnames(matPurchaser) <- names(vecPurchaser)
    #create data frame
    data.frame(matAwardID, matDate, matDollar, matPurchaser, matContract)
}