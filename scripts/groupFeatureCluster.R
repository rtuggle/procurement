require(dplyr)
require(ggplot2)
require(pvclust)
require(MESS)
require

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


#get the number of offers by group
offers.compare <- group.frame %>%
  mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                             Number.of.Offers.Received)) %>%
  select(Contracting.Group.ID, catAward, uniqueId, holdOffers, 
         Action.Obligation) %>%
  group_by(Contracting.Group.ID, catAward, uniqueId) %>%
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
setaside.compare <- group.frame %>%
  mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                  is.na(Type.of.Set.Aside), 1, 0)) %>%
  mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
  select(Contracting.Group.ID, catAward, indNoSetAside, weightNoSetAside,
         Action.Obligation) %>%
  group_by(Contracting.Group.ID, catAward) %>%
  summarize(propNoSetAside = sum(indNoSetAside) / n(), 
            wtpropNoSetAside = sum(weightNoSetAside) / sum(Action.Obligation))


#get the percent with firm fixed price 
firmfixed.compare <- group.frame %>%
  mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
  mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
  select(Contracting.Group.ID, catAward, FirmFixedPrice, weightFirmFixed, Action.Obligation) %>%
  group_by(Contracting.Group.ID, catAward) %>%
  summarize(propFirmFixed = sum(FirmFixedPrice) / n(), 
            wtpropFirmFixed = sum(weightFirmFixed) / sum(Action.Obligation))


#get the percent from historically disadvantaged vendors

historical.disadv.compare <- group.frame %>%
  mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
  select(Contracting.Group.ID, catAward, 
         histDisAdv, weightHistDis, Action.Obligation) %>%
  group_by(Contracting.Group.ID, catAward) %>%
  summarize(propHistDis = sum(histDisAdv) / n(), 
            wtpropHistDis = sum(weightHistDis) / sum(Action.Obligation))

#the percent competed (i.e. competition rate)

competed.compare <- group.frame %>%
  mutate(binCompeted = ifelse(!grepl("Not", compCat), 1, 0)) %>%
  mutate(weightCompeted = binCompeted * Action.Obligation) %>%
  select(Contracting.Group.ID, catAward,  binCompeted, weightCompeted,
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

vendorConc.compare <- group.frame %>%
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

congressConc.compare <- group.frame %>%
  filter(!grepl("NA", congressId)) %>%
  group_by(Contracting.Group.ID, catAward,  congressId) %>%
  summarize(dollars = sum(Action.Obligation)) %>%
  arrange(Contracting.Group.ID, catAward,
          desc(dollars)) %>%
  ungroup() %>% group_by(Contracting.Group.ID, catAward) %>% 
  mutate(rank = row_number(), propGroup = cumsum(dollars) / sum(dollars),
         propCongress = cumsum(rank) / sum(rank)) %>%
  filter(!is.na(propGroup)) %>%
  summarize(congressConc = MESS::auc(propCongress,propGroup,type = 'spline')) 



#put them together
featureGroup.compare <- offers.compare %>%
  full_join(setaside.compare, by = c('Contracting.Group.ID','catAward')) %>%
  full_join(firmfixed.compare,by = c('Contracting.Group.ID','catAward')) %>%
  full_join(historical.disadv.compare,by = c('Contracting.Group.ID','catAward')) %>%
  full_join(competed.compare,by = c('Contracting.Group.ID','catAward')) %>%
  full_join(vendorConc.compare,by = c('Contracting.Group.ID','catAward'))  %>%
  full_join(congressConc.compare,by = c('Contracting.Group.ID','catAward'))

group.features <- featureGroup.compare %>% select(Contracting.Group.ID,catAward,
                                                   propNoSetAside,propCompete, maxOffers) %>%
filter(catAward == "Award")

rnames <-group.features$Contracting.Group.ID
group.features <- group.features %>% select(- Contracting.Group.ID)
group.features <- as.matrix(group.features)
rownames(group.features) <- rnames

group.features <- na.omit(group.features) # listwise deletion of missing
mydata <- as.numeric(group.features[,-(1:2)])
mydata.scaled <- scale(mydata) # standardize variables 

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
