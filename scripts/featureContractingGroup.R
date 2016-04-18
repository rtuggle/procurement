## Load Packages
require(MESS)


##Build feature set for contracting offices

group.frame <- fpds %>%
    select(Contracting.Group.ID, Fiscal.Year,
           uniqueId, vendorId, congressId, NAICS.Code, 
           Action.Obligation, Number.of.Offers.Received, 
           Extent.Competed, Reason.Not.Awarded.To..Small.Business, 
           catAward, naicsTwo, naicsThree, naicsFour, 
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
           Congressional.District.Place.of..Performance
) 


#get the number of offers by office
offers <- group.frame %>%
    mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                                 Number.of.Offers.Received)) %>%
    select(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward, uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation),
              numberAwards = n_distinct(uniqueId),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward) %>%
    summarize(sumActions = sum(numberActions), sumAwards = sum(numberAwards),
              sumDollars = sum(amountDollars), sumOffers = sum(numberOffers), 
              meanOffers = mean(numberOffers), medianOffers = median(numberOffers), 
              maxOffers = max(numberOffers),
              wtavgOffers = sum(weightedOffers) / sum(amountDollars))

#get the percent with no set asides
setaside <- group.frame %>%
    mutate(indNoSetAside = ifelse(grepl("NO SET", Type.of.Set.Aside) | 
                                is.na(Type.of.Set.Aside), 1, 0)) %>%
    mutate(weightNoSetAside = indNoSetAside * Action.Obligation) %>%
    select(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward,
           indNoSetAside, weightNoSetAside,
           Action.Obligation) %>%
    group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward) %>%
    summarize(pctNoSetAside = sum(indNoSetAside) / n(), 
              wtpctNoSetAside = sum(weightNoSetAside) / sum(Action.Obligation))


#get the percent with firm fixed price 
firmfixed <- group.frame %>%
  mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
  mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
  select(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward,
         FirmFixedPrice, weightFirmFixed,
         Action.Obligation) %>%
  group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(pctFirmFixed = sum(FirmFixedPrice) / n(), 
            wtpctFirmFixed = sum(weightFirmFixed) / sum(Action.Obligation))


#get the percent from historically disadvantaged vendors

historical.disadvantage <- group.frame %>%
  mutate(weightHistDis = histDisAdv * Action.Obligation) %>%
  select(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward,
         histDisAdv, weightHistDis,
         Action.Obligation) %>%
  group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(pctHistDis = sum(histDisAdv) / n(), 
            wtpctHistDis = sum(weightHistDis) / sum(Action.Obligation))
#....
#pct of actions that are firm fixed price (Type.of.Contract) include the weighted val
#pct hDisadvantaged, check the git hub for list of variables recommend paste together
#and grepl for "YES" (double check), include the weighted val
#concentration values for offices for vendor (vendorId), geography (congressId)
#see below for example of start, need to add the other dimensions

vendorConc <- office.frame %>%
  filter(!grepl("NA", vendorId)) %>%
  group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward, vendorId) %>%
  summarize(dollars = sum(Action.Obligation)) %>%
  arrange(Contracting.Group.ID, Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
          desc(dollars)) %>%
  ungroup() %>% group_by(Contracting.Group.ID, naicsTwo, Fiscal.Year, catAward) %>% 
  mutate(rank = row_number(), pctGroup = cumsum(dollars) / sum(dollars),
         pctVendor = cumsum(rank) / sum(rank)) %>%
  filter(!is.na(pctGroup)) %>%
  summarize(vendorConc = MESS::auc(pctVendor,pctGroup,type = 'spline')) 

#put them togeher
featureGroup <- offers %>%
    full_join(setaside, by = c('Contracting.Group.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(firmfixed,by = c('Contracting.Group.ID', 'naicsTwo', 'Fiscal.Year','catAward')) %>%
  full_join(historical.disadvantage,by = c('Contracting.Group.ID', 'naicsTwo', 'Fiscal.Year','catAward'))

write.csv(featureGroup, "Contracting_Group_Features_20160418.csv",na="",row.names = FALSE)
  


