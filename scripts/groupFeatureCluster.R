require(dplyr)
require(ggplot2)
require(pvclust)
require(MESS)


## Which features will be used for distance measurement

group.frame <- fpds %>%
  select(Contracting.Group.ID, Fiscal.Year,
         uniqueId, vendorId, congressId, NAICS.Code, 
         Action.Obligation, Number.of.Offers.Received, 
         Extent.Competed, Reason.Not.Awarded.To..Small.Business, 
         catAward, compCat, naicsTwo, naicsThree, naicsFour, 
         Award.or.IDV.Type, Type.of.Contract, Type.of.Set.Aside,
         Fair.Opportunity.Limited.Sources, Other.Than.Full.and.Open.Competition,
         Effective.Date, Completion.Date,
         Contracting.Office.Name, Contracting.Agency.ID, Contracting.Agency.Name,
         Referenced..IDV.PIID, Modification.Number, 
         Principal.Place.of.Performance.State.Code,
         Congressional.District.Place.of..Performance, histDisAdv
  )

make.feature.table <- function(data){
  ## function to create a list of features by contracting group and combine them into a table
  
  
  #get the number of offers by group
  offers.compare <- data %>%
    mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                               Number.of.Offers.Received)) %>%
    select(Contracting.Group.ID, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation,na.rm = T),
              numberAwards = n_distinct(uniqueId,na_rm = T),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(sumActions = sum(numberActions,na.rm = T), sumAwards = sum(numberAwards,na.rm = T),
              sumDollars = sum(amountDollars,na.rm = T), sumOffers = sum(numberOffers,na.rm = T), 
              meanOffers = mean(numberOffers,na.rm = T), medianOffers = median(numberOffers,na.rm = T), 
              maxOffers = max(numberOffers,na.rm = T),
              wtavgOffers = sum(weightedOffers,na.rm = T) / sum(amountDollars,na.rm = T))
  
  #get the percent with no set asides
  setaside.compare <- data %>%
    mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                    is.na(Type.of.Set.Aside), 1, 0)) %>%
    mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
    select(Contracting.Group.ID, indNoSetAside, weightNoSetAside,
           Action.Obligation) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(propNoSetAside = sum(indNoSetAside,na.rm = T) / n(), 
              wtpropNoSetAside = sum(weightNoSetAside,na.rm = T) / sum(Action.Obligation,na.rm = T))
  
  
  #get the percent with firm fixed price 
  firmfixed.compare <- data %>%
    mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
    mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
    select(Contracting.Group.ID, FirmFixedPrice, weightFirmFixed, Action.Obligation) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(propFirmFixed = sum(FirmFixedPrice,na.rm = T) / n(), 
              wtpropFirmFixed = sum(weightFirmFixed,na.rm = T) / sum(Action.Obligation,na.rm = T))
  
  
  #get the percent from historically disadvantaged vendors
  
  historical.disadv.compare <- data %>%
    mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
    select(Contracting.Group.ID, 
           histDisAdv, weightHistDis, Action.Obligation) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(propHistDis = sum(histDisAdv) / n(), 
              wtpropHistDis = sum(weightHistDis,na.rm = T) / sum(Action.Obligation,na.rm = T))
  
  #the percent competed (i.e. competition rate)
  
  competed.compare <- data %>%
    mutate(binCompeted = ifelse(!grepl("Not", compCat), 1, 0)) %>%
    mutate(weightCompeted = binCompeted * Action.Obligation) %>%
    select(Contracting.Group.ID,  binCompeted, weightCompeted,
           Action.Obligation) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(propCompete = sum(binCompeted,na.rm = T) / n(), 
              wtpropCompeted = sum(weightCompeted,na.rm = T) / sum(Action.Obligation,na.rm = T))
  #....
  #prop of actions that are firm fixed price (Type.of.Contract) include the weighted val
  #prop hDisadvantaged, check the git hub for list of variables recommend paste together
  #and grepl for "YES" (double check), include the weighted val
  #concentration values for offices for vendor (vendorId), geography (congressId)
  #see below for example of start, need to add the other dimensions
  
  vendorConc.compare <- data %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Group.ID,  vendorId) %>%
    summarize(dollars = sum(Action.Obligation,na.rm = T)) %>%
    arrange(Contracting.Group.ID,
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID) %>% 
    mutate(rank = row_number(), propGroup = cumsum(dollars) / sum(dollars,na.rm = T),
           propVendor = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(propGroup)) %>%
    summarize(vendorConc = MESS::auc(propVendor,propGroup,type = 'spline')) 
  
  #concentration values for offices for geography (congressId)
  
  congressConc.compare <- data %>%
    filter(!grepl("NA", congressId)) %>%
    group_by(Contracting.Group.ID,  congressId) %>%
    summarize(dollars = sum(Action.Obligation,na.rm = T)) %>%
    arrange(Contracting.Group.ID, 
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID) %>% 
    mutate(rank = row_number(), propGroup = cumsum(dollars) / sum(dollars,na.rm = T),
           propCongress = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(propGroup)) %>%
    summarize(congressConc = MESS::auc(propCongress,propGroup,type = 'spline')) 
  
  
  
  #put them together
  featureGroup.compare <- offers.compare %>%
    full_join(setaside.compare, by = c('Contracting.Group.ID')) %>%
    full_join(firmfixed.compare,by = c('Contracting.Group.ID')) %>%
    full_join(historical.disadv.compare,by = c('Contracting.Group.ID')) %>%
    full_join(competed.compare,by = c('Contracting.Group.ID')) %>%
    full_join(vendorConc.compare,by = c('Contracting.Group.ID'))  %>%
    full_join(congressConc.compare,by = c('Contracting.Group.ID'))
  
  group.features <- featureGroup.compare %>% select(Contracting.Group.ID,wtpropCompeted,
                                                    wtavgOffers, wtpropNoSetAside)
  #select(Contracting.Group.ID,propNoSetAside, propCompete,maxOffers, propHistDis,congressConc)
  
  return(group.features)
}


make.graphable <- function(data, scaled=T){
  ## Transform data into numeric matrix for clustering and graphing
  data.out <- data
  rnames <- data.out$Contracting.Group.ID
  data.out <- data.out %>% select(-Contracting.Group.ID)
  data.out <- as.matrix(data.out)
  rownames(data.out) <- rnames

  if (scaled){
    data.out <- scale(data.out) # standardize variables 
  }
  return(na.omit(data.out))
}


kmeans.cluster.count <- function(data){

  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(data,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}





## make feature table for both awards and vehicles 
comparison.both <- make.feature.table(group.frame)
both.graphable <- make.graphable(comparison.both,scaled=F)
both.graphable.scaled <- make.graphable(comparison.both,scaled=T)


## count clusters
kmeans.cluster.count(both.graphable)
kmeans.cluster.count(both.graphable.scaled)



# K-Means Cluster Analysis
fit <- kmeans(both.graphable.scaled, 5) # 5 cluster solution
plot(fit$cluster)
# get cluster means
aggregate(both.graphable.scaled,by=list(fit$cluster),FUN=mean)
# append cluster assignment
both.out <- data.frame(both.graphable.scaled, fit$cluster) 



## calculate distances
d <- dist(both.graphable.scaled, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D",)
plot(fit)
groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")


thinger <- cluster::agnes(both.graphable.scaled)
plot(thinger)

thinger <- cluster::clara(both.graphable.scaled,k=6)
plot(thinger)


## make feature table for awards only
comparison.awards <- filter(group.frame, catAward == "Award") %>% make.feature.table(.)
awards.graphable <- make.graphable(comparison.awards,scaled=F)
awards.graphable.scaled <- make.graphable(comparison.awards,scaled=T)

## count clusters
kmeans.cluster.count(awards.graphable)
kmeans.cluster.count(awards.graphable.scaled)


# Ward Hierarchical Clustering with Bootstrapped p values  -- pvclust packages

fit <- pvclust::pvclust(awards.graphable.scaled, method.hclust="ward.D",
                        method.dist ="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 


## calculate distances
awards.dist <- dist(awards.graphable.scaled, method = "euclidean") # distance matrix
awards.fit <- hclust(awards.dist, method="ward.D")
plot(awards.fit)
awards.clusters <- cutree(awards.fit, k=5)
rect.hclust(awards.fit, k=5, border="red")

awards.compare <- as.data.frame(awards.clusters) %>% mutate(Contract.Group.ID=rownames(.)) %>%
  full_join(awards.graphable.scaled,by="Contract.Group.ID")

groups %>% pairs(color=)

## Base R implementation

d <- dist(mydata.scaled, method = "euclidean",) # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")


# Ward Hierarchical Clustering with Bootstrapped p values  -- pvclust packages

fit <- pvclust::pvclust(mydata.scaled, method.hclust="ward.D",
               method.dist ="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95) 

## Twins Agglomerative Nesting (Agnes) --  cluster
thinger <- cluster::agnes(t(mydata.scaled))
plot(thinger)

thinger <- cluster::diana(mydata.scaled,)
plot(thinger)


thinger <- cluster::clara(mydata.scaled,k=4)

thinger <- cluster::clara(mydata.scaled,k=8)


thinger <- cluster::fanny(mydata.scaled,k=4)
plot(thinger)


thinger <- cluster::pam(mydata.scaled,k=4)
plot(thinger)

### KMEANS



wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


wss <- (nrow(mydata.scaled)-1)*sum(apply(mydata.scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata.scaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata.scaled, 4) # 5 cluster solution
# get cluster means
aggregate(mydata.scaled,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata.out <- data.frame(mydata.scaled, fit$cluster) 
plot(mydata.out)
