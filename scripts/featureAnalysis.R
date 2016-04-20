
#retrieve variables needed to create features
base.frame <- fpds %>%
    select(Contracting.Group.ID, Fiscal.Year,
           uniqueId, vendorId, congressId, NAICS.Code, 
           Action.Obligation, Number.of.Offers.Received, 
           Extent.Competed, Reason.Not.Awarded.To..Small.Business, 
           catAward, compCat, naicsTwo, naicsThree, naicsFour, 
           Award.or.IDV.Type, Type.of.Contract, Type.of.Set.Aside,
           Fair.Opportunity.Limited.Sources, Other.Than.Full.and.Open.Competition,
           Effective.Date, Completion.Date,
           Contracting.Office.Name, Contracting.Agency.ID, Contracting.Agency.Name,
           Funding.Department.ID, Funding.Department.Name,
           Funding.Agency.ID, Funding.Agency.Name, 
           Funding.Office.ID, Funding.Office.Name,
           PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID, 
           Referenced..IDV.PIID, Modification.Number, 
           Principal.Place.of.Performance.State.Code,
           Congressional.District.Place.of..Performance, histDisAdv
    ) 

##build features

#get the number of offers by office
offers <- base.frame %>%
    mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                               Number.of.Offers.Received)) %>%
    select(Contracting.Group.ID, catAward, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, catAward,  uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation),
              numberAwards = n_distinct(uniqueId),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Group.ID, catAward) %>%
    summarize(sumActions = sum(numberActions), sumAwards = sum(numberAwards),
              sumDollars = sum(amountDollars), sumOffers = sum(numberOffers), 
              meanOffers = mean(numberOffers), medianOffers = median(numberOffers), 
              maxOffers = max(numberOffers),
              wtavgOffers = sum(weightedOffers) / sum(amountDollars))

#get the percent with no set asides
setaside <- base.frame %>%
    mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                      is.na(Type.of.Set.Aside), 1, 0)) %>%
    mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
    select(Contracting.Group.ID, catAward,indNoSetAside, weightNoSetAside,
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, catAward) %>%
    summarize(propNoSetAside = sum(indNoSetAside) / n(), 
              wtpropNoSetAside = sum(weightNoSetAside) / sum(Action.Obligation))


#get the percent with firm fixed price 
firmfixed <- base.frame %>%
    mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
    mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
    select(Contracting.Group.ID, catAward, 
           FirmFixedPrice, weightFirmFixed, Action.Obligation) %>%
    group_by(Contracting.Group.ID, catAward) %>%
    summarize(propFirmFixed = sum(FirmFixedPrice) / n(), 
              wtpropFirmFixed = sum(weightFirmFixed) / sum(Action.Obligation))


#get the percent from historically disadvantaged vendors
historical.disadv <- base.frame %>%
    mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
    select(Contracting.Group.ID, catAward, 
           histDisAdv, weightHistDis, Action.Obligation) %>%
    group_by(Contracting.Group.ID, catAward) %>%
    summarize(propHistDis = sum(histDisAdv) / n(), 
              wtpropHistDis = sum(weightHistDis) / sum(Action.Obligation))

#the percent competed (i.e. competition rate)
competed <- base.frame %>%
    mutate(binCompeted = ifelse(!grepl("Not", compCat), 1, 0)) %>%
    mutate(weightCompeted = binCompeted * Action.Obligation) %>%
    select(Contracting.Group.ID, catAward,binCompeted, weightCompeted,
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, catAward) %>%
    summarize(propCompete = sum(binCompeted) / n(), 
              wtpropCompeted = sum(weightCompeted) / sum(Action.Obligation))
#....
#prop of actions that are firm fixed price (Type.of.Contract) include the weighted val
#prop hDisadvantaged, check the git hub for list of variables recommend paste together
#and grepl for "YES" (double check), include the weighted val
#concentration values for offices for vendor (vendorId), geography (congressId)
#see below for example of start, need to add the other dimensions

vendorConc <- base.frame %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Group.ID, catAward, vendorId) %>%
    summarize(dollars = sum(Action.Obligation)) %>%
    arrange(Contracting.Group.ID, catAward,
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID, catAward) %>% 
    mutate(rank = row_number(), propGroup = cumsum(dollars) / sum(dollars),
           propVendor = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(propGroup)) %>%
    summarize(vendorConc = MESS::auc(propVendor,propGroup,type = 'spline')) 

#concentration values for offices for geography (congressId)
congressConc <- base.frame %>%
    filter(!grepl("NA", congressId)) %>%
    group_by(Contracting.Group.ID, catAward, congressId) %>%
    summarize(dollars = sum(Action.Obligation)) %>%
    arrange(Contracting.Group.ID, catAward,
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID, catAward) %>% 
    mutate(rank = row_number(), propGroup = cumsum(dollars) / sum(dollars),
           propCongress = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(propGroup)) %>%
    summarize(congressConc = MESS::auc(propCongress,propGroup,type = 'spline')) 

#put them togeher
featureAnalysis <- offers %>%
    full_join(setaside, by = c('Contracting.Group.ID', 'catAward')) %>%
    full_join(firmfixed,by = c('Contracting.Group.ID', 'catAward')) %>%
    full_join(historical.disadv,by = c('Contracting.Group.ID', 'catAward')) %>%
    full_join(competed,by = c('Contracting.Group.ID', 'catAward')) %>%
    full_join(vendorConc,by = c('Contracting.Group.ID', 'catAward')) %>%
    full_join(congressConc,by = c('Contracting.Group.ID', 'catAward'))
    
chart.feature <- featureAnalysis %>%
    select(Contracting.Group.ID, sumAwards, sumDollars, 
           wtpropCompeted, wtavgOffers, wtpropNoSetAside) %>%
    mutate(competedRate = wtpropCompeted, offerRate = wtavgOffers, 
           setAsideRate = wtpropNoSetAside)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = (cex.cor * r)+1)
}

## Write out scatter plot matrix to png

png("fig/5_scatterMetrics.png",height=1000,width=1400)
pairs(~sumAwards + sumDollars + competedRate + offerRate + setAsideRate, 
      data=na.omit(chart.feature), 
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      cex.labels=2,
      pch = 20, main='Competition Metrics Matrix\n Comparison of Regional Contracting Agencies') +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=13,face="bold"),
        title=element_text(size=14,face="bold"))

dev.off()




require()
plotmatrix(with(chart, data.frame(sumAwards, sumDollars, 
                                  competedRate, offerRate, setAsideRate)))



c("sumAwards", "sumDollars", "wtavgOffers", "wtavgOffers")) 
    
    
    gather("measure", "value", 3:20) %>%
    filter(grepl("wtavg", measure) | grepl("wtprop", measure) | grepl("Conc", measure) |
               (grepl("sum", measure) & !grepl("Offer", measure))) %>%
    filter(catAward == "Award")

#Create combination of every measure
prep$measure <- as.factor(prep$measure)
combis <- expand.grid(levels(prep$measure),levels(prep$measure))

#Generate list of vectors with combinations of measures
listVal <- lapply(seq_len(nrow(combis)),function(i) cbind(prep[prep$measure==combis[i,1], c("measure", "value")],
                                                          prep[prep$measure==combis[i,2], c("measure","value")]))
#Combine list to frame and rename columns
chart <- do.call(rbind, listVal)
names(chart) <- c("var.x","x","var.y","y")

#Calculate correlations
##couldn't figure out how to get this to work 
library(plyr)
cors <- ddply(chart,.(var.x,var.y),summarize,
              cor=format(signif(cor(x,y, use="complete"),2),scientific=-2))

ggplot(chart, aes(x=x,y=y)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    #geom_text(data=cors,aes(label=paste("r =",cor))) +
    facet_wrap(~var.y*var.x,ncol=4) 

cors <- chart %>%
    group_by(var.x,var.y) %>%
    summarize(corr=cor(x,y, use="complete"))

cors$x <- 2.2
cors$y <- 2.5