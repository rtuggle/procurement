createIdv <- function(file) {
    data <- xmlTreeParse(file)
    root <- xmlRoot(data)
    children <- xmlChildren(root)
    #retrieve the number of nodes in the file
    #note: the first node is a count of entries, the rest are awards
    len <- length(xmlApply(root, xmlSize))
    #source getNames function
    source('~/Repositories/procurement/getNames.R', local=TRUE)
    #Awards Matrices
    a.names <- c('agencyID', 'PIID', 'modNumber')
    matPIID <- matrix('NULL', nrow = len - 1, ncol = 3)
    colnames(matPIID) <- a.names
    matRefPIID <- matrix('NULL', nrow = len - 1, ncol = 3)
    colnames(matRefPIID) <- a.names
    #Dates Matrix
    d.names <- getNames('relevantContractDates')
    matDate <- matrix('NULL', nrow = len -1, ncol = length(d.names))
    colnames(matDate) <- d.names
    #Dollar Matrix
    do.names <- getNames('dollarValues')
    matDollar <- matrix('NULL', nrow = len -1, ncol = length(do.names))
    colnames(matDollar) <- do.names
    #Purchaser Matrix
    p.names <- getNames('purchaserInformation')
    matPurchaser <- matrix('NULL', nrow = len -1, ncol = length(p.names))
    colnames(matPurchaser) <- p.names
    #Marketing Matrix
    m.names <- getNames('contractMarketingData')
    matMarket <- matrix('NULL', nrow = len -1, ncol = length(m.names))
    colnames(matMarket) <- m.names
    #Contract Matrix
    c.names <- getNames('contractData')
    #remove the list of Treasury Accounts
    c.names <- c.names[c.names!='listOfTreasuryAccounts']
    matContract <- matrix('NULL', nrow = len -1, ncol = length(c.names))
    colnames(matContract) <- c.names
    #productOrServiceInformation Matrix
    pr.names <- getNames('productOrServiceInformation')
    matProduct <- matrix('NULL', nrow = len -1, ncol = length(pr.names))
    colnames(matProduct) <- pr.names
    #competition
    cp.names <- getNames('competition')
    matCompetition <- matrix('NULL', nrow = len -1, ncol = length(cp.names))
    colnames(matCompetition) <- cp.names
    #vendor header
    v.names <- 'vendorName'
    matVendor <- matrix('NULL', nrow = len -1, ncol = 1)
    colnames(matVendor) <- v.names
    #vendor type
    vt.names <- c("isAlaskanNativeOwnedCorporationOrFirm"       
                  , "isAmericanIndianOwned" 
                  , "isIndianTribe"
                  , "isNativeHawaiianOwnedOrganizationOrFirm"
                  , "isTriballyOwnedFirm" 
                  , "isVeteranOwned" 
                  , "isServiceRelatedDisabledVeteranOwnedBusiness"
                  , "isWomenOwned"
                  , "isVerySmallBusiness" )
    matSocio <- matrix('NULL', nrow = len -1, ncol = 9)
    colnames(matSocio) <- vt.names
    #vendor minority
    vm.names <- c("isMinorityOwned"                         
                  , "isSubContinentAsianAmericanOwnedBusiness"
                  , "isAsianPacificAmericanOwnedBusiness"     
                  , "isBlackAmericanOwnedBusiness"            
                  , "isHispanicAmericanOwnedBusiness"         
                  , "isNativeAmericanOwnedBusiness"           
                  , "isOtherMinorityOwned"    )
    matMinority <- matrix('NULL', nrow = len -1, ncol = 7)
    colnames(matMinority) <- vm.names
    #vendor certifications
    vc.names <- c( "isDOTCertifiedDisadvantagedBusinessEnterprise"
                   , "isSelfCertifiedSmallDisadvantagedBusiness"    
                   , "isSBACertifiedSmallDisadvantagedBusiness"     
                   , "isSBACertified8AProgramParticipant"           
                   , "isSBACertifiedHUBZone"                        
                   , "isSBACertified8AJointVenture"     )
    matCertification <- matrix('NULL', nrow = len -1, ncol = 6)
    colnames(matCertification) <- vc.names
    #vendor location
    vl.names <- c( "streetAddress"
                   , "city", "state", "ZIPCode"
                   , "countryCode", "phoneNo"                  
                   , "faxNo", "congressionalDistrictCode"
                   , "streetAddress2"    )
    matLocation <- matrix('NULL', nrow = len -1, ncol = 9)
    colnames(matLocation) <- vl.names
    #vendor site code
    vs.names <- c( "vendorSiteCode"
                   , "vendorAlternateSiteCode" )
    matSite <- matrix('NULL', nrow = len -1, ncol = 2)
    colnames(matSite) <- vs.names
    #vendor DUNS
    vd.names <- c( "DUNSNumber")
    matDUNS <- matrix('NULL', nrow = len -1, ncol = 1)
    colnames(matDUNS) <- vd.names
    
    #loop through every award, note: awards begin at node 2
    for(i in 2:len) {
        idv <- children[[i]]
        ##PIID
        var <- xmlApply(idv[['contractID']][['IDVID']], xmlValue)
        #cycle through names of PIID columns updating data if found
        for(j in 1:length(a.names)){
            if(a.names[j] %in% names(var)) {
                tryCatch(matPIID[i-1, a.names[j]] <- var[[a.names[j]]]
                         , error = function(e){ 
                             stop(paste("PIID breaks first at line",i))
                         }
                )
            }
        }
        # note about repitition: repeats because I was unable to design 
        # function that updates matrices with the tryCatch
        ##refPIID
        #many records do not include referencedIDV so use try 
        var <- try(xmlApply(idv[['contractID']][['referencedIDVID']], xmlValue), silent = TRUE)
        if(!"try-error" %in% class(var)){
            for(j in 1:length(a.names)){
                if(a.names[j] %in% names(var)) {
                    tryCatch(matRefPIID[i-1, a.names[j]] <- var[[a.names[j]]]
                         , error = function(e){ 
                             stop(paste("referenced PIID breaks first at line",i))
                            }
                    )
                }
            }
        }
        ##DATE
        var <- xmlApply(idv[['relevantContractDates']], xmlValue)
        for(j in 1:length(d.names)){
            if(d.names[j] %in% names(var)) {
                tryCatch(matDate[i-1, d.names[j]] <- var[[d.names[j]]]
                         , error = function(e){ 
                             stop(paste("dates breaks first at line",i))
                         }
                )
            }
        }
        ##DOLLAR
        var <- xmlApply(idv[['dollarValues']], xmlValue)
        #cycle through names updating data if found
        for(j in 1:length(do.names)){
            if(do.names[j] %in% names(var)) {
                tryCatch(matDollar[i-1, do.names[j]] <- var[[do.names[j]]]
                         , error = function(e){ 
                             stop(paste("dollar breaks first at line",i))
                         }
                )
            }
        }
        ##PURCHASER
        var <- xmlApply(idv[['purchaserInformation']], xmlValue)
        for(j in 1:length(p.names)){
            if(p.names[j] %in% names(var)) {
                tryCatch(matPurchaser[i-1, p.names[j]] <- var[[p.names[j]]]
                         , error = function(e){ 
                             stop(paste("purchaser breaks first at line",i))
                         }
                )
            }
        }
        ##Marketing
        var <- xmlApply(idv[['contractMarketingData']], xmlValue)
        for(j in 1:length(m.names)){
            if(m.names[j] %in% names(var)) {
                tryCatch(matMarket[i-1, m.names[j]] <- var[[m.names[j]]]
                         , error = function(e){ 
                             stop(paste("marketing breaks first at line",i))
                         }
                )
            }
        }
        ##CONTRACT
        var <- xmlApply(idv[['contractData']], xmlValue)
        for(j in 1:length(c.names)){
            if(c.names[j] %in% names(var)) {
                tryCatch(matContract[i-1, c.names[j]] <- var[[c.names[j]]]
                         , error = function(e){ stop(paste("contract breaks first at line",i))})
            }
        }
        ##PRODUCT 
        var <- xmlApply(idv[['productOrServiceInformation']], xmlValue)
        for(j in 1:length(pr.names)){
            if(pr.names[j] %in% names(var)) {
                tryCatch(matProduct[i-1, pr.names[j]] <- var[[pr.names[j]]]
                         , error = function(e){ stop(paste("product breaks first at line",i))})
            }
        }
        ##COMPETITION 
        var <- xmlApply(idv[['competition']], xmlValue)
        for(j in 1:length(cp.names)){
            if(cp.names[j] %in% names(var)) {
                tryCatch(matCompetition[i-1, cp.names[j]] <- var[[cp.names[j]]]
                         , error = function(e){ stop(paste("competition breaks first at line",i))})
            }
        }
        ##VENDOR
        var <- xmlApply(idv[['vendor']][['vendorHeader']], xmlValue)
        for(j in 1:length(v.names)){
            if(v.names[j] %in% names(var)) {
                tryCatch(matVendor[i-1, v.names[j]] <- var[[v.names[j]]]
                         , error = function(e){ stop(paste("vendor breaks first at line",i))})
            }
        }
        ###Socio
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']][['vendorSocioEconomicIndicators']], xmlValue)
            for(j in 1:length(vt.names)){
                if(vt.names[j] %in% names(var)) {
                    tryCatch(matSocio[i-1, vt.names[j]] <- var[[vt.names[j]]]
                         , error = function(e){ stop(paste("vendor type (socio) breaks first at line",i))})
            }
        }
        ###Minority
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']][['vendorSocioEconomicIndicators']][['minorityOwned']], xmlValue)
        for(j in 1:length(vm.names)){
            if(vm.names[j] %in% names(var)) {
                tryCatch(matMinority[i-1, vm.names[j]] <- var[[vm.names[j]]]
                         , error = function(e){ stop(paste("vendor type (minority) breaks first at line",i))})
            }
        }
        ###Certifications
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']][['vendorCertifications']], xmlValue)
        for(j in 1:length(vc.names)){
            if(vc.names[j] %in% names(var)) {
                tryCatch(matCertification[i-1, vc.names[j]] <- var[[vc.names[j]]]
                         , error = function(e){ stop(paste("vendor type (certification) breaks first at line",i))})
            }
        }
        ###Location
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']][['vendorLocation']], xmlValue)
        for(j in 1:length(vl.names)){
            if(vl.names[j] %in% names(var)) {
                tryCatch(matLocation[i-1, vl.names[j]] <- var[[vl.names[j]]]
                         , error = function(e){ stop(paste("vendor type (location) breaks first at line",i))})
            }
        }
        ###SiteCode
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']], xmlValue)
        for(j in 1:length(vs.names)){
            if(vs.names[j] %in% names(var)) {
                tryCatch(matSite[i-1, vs.names[j]] <- var[[vs.names[j]]]
                         , error = function(e){ stop(paste("vendor site code breaks first at line",i))})
            }
        }
        ###DUNS
        var <- xmlApply(idv[['vendor']][['vendorSiteDetails']][['vendorDUNSInformation']], xmlValue)
        for(j in 1:length(vd.names)){
            if(vd.names[j] %in% names(var)) {
                tryCatch(matDUNS[i-1, vd.names[j]] <- var[[vd.names[j]]]
                         , error = function(e){ stop(paste("vendor DUNS breaks first at line",i))})
            }
        }
    }
    #rename the reference PIIDs
    colnames(matRefPIID) <- c('refAgencyID', 'refPIID', 'refModNumber')
    #create data frame
    data.frame(matPIID, matRefPIID, matDate, matDollar
               , matPurchaser, matMarket, matContract
               , matProduct, matCompetition, matVendor
               , matSocio, matMinority, matLocation
               , matSite, matDUNS)
}