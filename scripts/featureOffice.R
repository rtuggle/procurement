## Load Packages
require(MESS)
require(dplyr)

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
    select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat, uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation,na.rm = T),
              numberAwards = n_distinct(uniqueId,na_rm = T),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,compCat) %>%
    summarize(sumActions = sum(numberActions,na.rm = T), sumAwards = sum(numberAwards,na.rm = T),
              sumDollars = sum(amountDollars,na.rm = T), sumOffers = sum(numberOffers,na.rm = T), 
              meanOffers = mean(numberOffers,na.rm = T), medianOffers = median(numberOffers,na.rm = T), 
              maxOffers = max(numberOffers,na.rm = T),
              wtavgOffers = sum(weightedOffers,na.rm = T) / sum(amountDollars,na.rm = T))

#get the percent with no set asides
setaside <- office.frame %>%
    mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                is.na(Type.of.Set.Aside), 1, 0)) %>%
    mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
    select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,
           indNoSetAside, weightNoSetAside, Action.Obligation) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>%
    summarize(propNoSetAside = sum(indNoSetAside,na.rm = T) / n(), 
              wtpropNoSetAside = sum(weightNoSetAside,na.rm = T) / sum(Action.Obligation,na.rm = T))


#get the percent with firm fixed price 
firmfixed <- office.frame %>%
  mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
  mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,
         FirmFixedPrice, weightFirmFixed,Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>%
  summarize(propFirmFixed = sum(FirmFixedPrice,na.rm = T) / n(), 
            wtpropFirmFixed = sum(weightFirmFixed,na.rm = T) / sum(Action.Obligation,na.rm = T))


#get the percent from historically disadvantaged vendors

historical.disadv <- office.frame %>%
  mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,
         histDisAdv, weightHistDis, Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>%
  summarize(propHistDis = sum(histDisAdv,na.rm = T) / n(), 
            wtpropHistDis = sum(weightHistDis,na.rm = T) / sum(Action.Obligation,na.rm = T))


#the percent competed (i.e. competition rate)

competed <- office.frame %>%
  mutate(binCompeted = ifelse(!grepl("Not", compCat), 1, 0)) %>%
  mutate(weightCompeted = binCompeted * Action.Obligation) %>%
  select(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,binCompeted, weightCompeted,
         Action.Obligation) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>%
  summarize(propCompete = sum(binCompeted,na.rm = T) / n(), 
            wtpropCompeted = sum(weightCompeted,na.rm = T) / sum(Action.Obligation,na.rm = T))
#....
#prop of actions that are firm fixed price (Type.of.Contract) include the weighted val
#prop hDisadvantaged, check the git hub for list of variables recommend paste together
#and grepl for "YES" (double check), include the weighted val
#concentration values for offices for vendor (vendorId), geography (congressId)
#see below for example of start, need to add the other dimensions

#concentration values for offices for vendor (vendorId)

vendorConc <- office.frame %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat, vendorId) %>%
    summarize(dollars = sum(Action.Obligation,na.rm = T)) %>%
    arrange(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,
            desc(dollars)) %>%
    ungroup() %>% group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>% 
    mutate(rank = row_number(), propOffice = cumsum(dollars) / sum(dollars),
           propVendor = cumsum(rank) / sum(rank))
#%>%
#    filter(!is.na(propOffice) & !is.na(propVendor)) %>% na.omit() %>% 
#    summarize(vendorConc = MESS::auc(propVendor,propOffice,type = 'spline'))

#concentration values for offices for geography (congressId)

congressConc <- office.frame %>%
  filter(!grepl("NA", congressId)) %>%
  group_by(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat, congressId) %>%
  summarize(dollars = sum(Action.Obligation,na.rm = T)) %>%
  arrange(Contracting.Group.ID,Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat,
          desc(dollars)) %>%
  ungroup() %>% group_by(Contracting.Group.ID, Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, compCat) %>% 
  mutate(rank = row_number(), propOffice = cumsum(dollars) / sum(dollars,na.rm = T),
         propCongress = cumsum(rank) / sum(rank)) %>%
  filter(!is.na(propOffice)) %>% filter(!is.na(propCongress)) %>%
  summarize(congressConc = MESS::auc(propCongress,propOffice,type = 'spline')) 


#put them togeher
featureOffice <- offers %>%
    full_join(setaside, by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat')) %>%
  full_join(firmfixed,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat')) %>%
  full_join(historical.disadv,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat')) %>%
  full_join(competed,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat')) %>%
  full_join(vendorConc,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat')) %>%
  full_join(congressConc,by = c('Contracting.Group.ID','Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year','catAward','compCat'))

write.csv(featureOffice, "Contracting_Office_Features_20160420_v2.csv",na="",row.names = FALSE)
