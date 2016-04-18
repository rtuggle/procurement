## Load Packages
require(MESS)

##Build feature set for contracting offices

office.frame <- fpds %>%
    select(Contracting.Office.ID, Contracting.Group.ID,Fiscal.Year, 
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
           Congressional.District.Place.of..Performance,histDisAdv
) 


#get the number of offers by office
offers <- office.frame %>%
    mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                                 Number.of.Offers.Received)) %>%
    select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation),
              numberAwards = n_distinct(uniqueId),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
    summarize(sumActions = sum(numberActions), sumAwards = sum(numberAwards),
              sumDollars = sum(amountDollars), sumOffers = sum(numberOffers), 
              meanOffers = mean(numberOffers), medianOffers = median(numberOffers), 
              maxOffers = max(numberOffers),
              wtavgOffers = sum(weightedOffers) / sum(amountDollars))

#get the percent with no set asides
setaside <- office.frame %>%
    mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                is.na(Type.of.Set.Aside), 1, 0)) %>%
    mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
    select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
           indNoSetAside, weightNoSetAside, Action.Obligation) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
    summarize(propNoSetAside = sum(indNoSetAside) / n(), 
              wtpropNoSetAside = sum(weightNoSetAside) / sum(Action.Obligation))


#get the percent with firm fixed price 
firmfixed <- office.frame %>%
  mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
  mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
         FirmFixedPrice, weightFirmFixed,Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(propFirmFixed = sum(FirmFixedPrice) / n(), 
            wtpropFirmFixed = sum(weightFirmFixed) / sum(Action.Obligation))


#get the percent from historically disadvantaged vendors

historical.disadv <- office.frame %>%
  mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
         histDisAdv, weightHistDis, Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(propHistDis = sum(histDisAdv) / n(), 
            wtpropHistDis = sum(weightHistDis) / sum(Action.Obligation))


#the percent competed (i.e. competition rate)

competed <- office.frame %>%
  mutate(binCompeted = ifelse(!grepl("Not", compCat), 1, 0)) %>%
  mutate(weightCompeted = binCompeted * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,binCompeted, weightCompeted,
         Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(propCompete = sum(binCompeted) / n(), 
            wtpropCompeted = sum(weightCompeted) / sum(Action.Obligation))
#....
#prop of actions that are firm fixed price (Type.of.Contract) include the weighted val
#prop hDisadvantaged, check the git hub for list of variables recommend paste together
#and grepl for "YES" (double check), include the weighted val
#concentration values for offices for vendor (vendorId), geography (congressId)
#see below for example of start, need to add the other dimensions

#concentration values for offices for vendor (vendorId)
vendorConc <- office.frame %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, vendorId) %>%
    summarize(dollars = sum(Action.Obligation)) %>%
    arrange(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>% 
    mutate(rank = row_number(), propOffice = cumsum(dollars) / sum(dollars),
           propVendor = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(propOffice)) %>%
    summarize(vendorConc = MESS::auc(propVendor,propOffice,type = 'spline')) 

#concentration values for offices for geography (congressId)

congressConc <- office.frame %>%
  filter(!grepl("NA", congressId)) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, congressId) %>%
  summarize(dollars = sum(Action.Obligation)) %>%
  arrange(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
          desc(dollars)) %>%
  ungroup() %>% group_by(Contracting.Group.ID, Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>% 
  mutate(rank = row_number(), propOffice = cumsum(dollars) / sum(dollars),
         propCongress = cumsum(rank) / sum(rank)) %>%
  filter(!is.na(propOffice)) %>%
  summarize(congressConc = MESS::auc(propCongress,propOffice,type = 'spline')) 


#put them togeher
featureOffice <- offers %>%
    full_join(setaside, by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(firmfixed,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(historical.disadv,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(competed,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(vendorConc,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(congressConc,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward'))

write.csv(featureOffice, "Contracting_Office_Features_20160418.csv",na="",row.names = FALSE)
  


