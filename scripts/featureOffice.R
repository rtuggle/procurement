## Load Packages
require(MESS)

# read in list of Historically Disadvantaged Categories
hdis <- read.csv('data/listDisadvantaged.csv',stringsAsFactors = FALSE) %>% filter( Priviliged == "Yes")
hdis.list <- trimws(hdis$Vars)


##Build feature set for contracting offices

office.frame <- fpds %>%
    select(Contracting.Office.ID, Fiscal.Year,
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
           Congressional.District.Place.of..Performance, 
           Is.Vendor...Emerging.Small.Business,
           Is.Vendor.Business.Type...Economically.Disadvantaged.Women.Owned.Small.Business,
           Is.Vendor.Business.Type...Joint.Venture.Economically.Disadvantaged.Women.Owned.Small.Business,
           Is.Vendor.Business.Type...Joint.Venture.Women.Owned.Small.Business, 
           Is.Vendor.Business.Type...Women.Owned.Small.Business,
           Is.Vendor...Alaskan.Native.Corporation.Owned.Firm,
           Is.Vendor...American.Indian.Owned,
           Is.Vendor...Black.American.Owned,
           Is.Vendor...Domestic.Shelter,
           Is.Vendor...DoT.Certified.Disadvantaged.Business.Enterprise,
           Is.Vendor...Emerging.Small.Business,
           Is.Vendor...Hispanic.American.Owned,
           Is.Vendor...Indian.Tribe,
           Is.Vendor...Minority.Owned.Business,
           Is.Vendor...Native.American.Owned,
           Is.Vendor...Native.Hawaiian.Organization.Owned.Firm,
           Is.Vendor...Other.Minority.Owned,
           Is.Vendor...SBA.Certified.8..a..Joint.Venture,
           Is.Vendor...SBA.Certified.8.a..Program.Participant,
           Is.Vendor...SBA.Certified.Hub.Zone.firm,
           Is.Vendor...SBA.Certified.Small.Disadvantaged.Business,
           Is.Vendor...Self.Certifed.Small.Disadvantaged.Business,
           Is.Vendor...Service.Disabled.Veteran.Owned.Business,
           Is.Vendor...Subcontinent.Asian..Asian.Indian..American.Owned,
           Is.Vendor...Tribally.Owned,
           Is.Vendor...Veteran.Owned.Business,
           Is.Vendor...Woman.Owned.Business,
           Is.Vendor.Business.Type...Economically.Disadvantaged.Women.Owned.Small.Business,
           Is.Vendor.Business.Type...Women.Owned.Small.Business) 

# Add indicator of whether Vendor is member of historically disadvantaged group
office.frame$HistDisAdv <- 0
for (pop in hdis.list){
  office.frame$HistDisAdv[office.frame[[pop]] == "YES"] <- 1
}


#get the number of offers by office
offers <- office.frame %>%
    mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA, 
                                 Number.of.Offers.Received)) %>%
    select(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, uniqueId, holdOffers, 
           Action.Obligation) %>%
    group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, uniqueId) %>%
    summarize(numberOffers = max(holdOffers, na.rm = TRUE), 
              amountDollars = sum(Action.Obligation),
              numberAwards = n_distinct(uniqueId),
              numberActions = n()) %>%
    ungroup() %>%
    mutate(weightedOffers = numberOffers * amountDollars) %>%
    group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
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
    select(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
           indNoSetAside, weightNoSetAside,
           Action.Obligation) %>%
    group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
    summarize(pctNoSetAside = sum(indNoSetAside) / n(), 
              wtpctNoSetAside = sum(weightNoSetAside) / sum(Action.Obligation))


#get the percent with firm fixed price 
firmfixed <- office.frame %>%
  mutate(FirmFixedPrice = ifelse(grepl("FIRM FIXED PRICE", Type.of.Contract), 1, 0)) %>%
  mutate(weightFirmFixed = FirmFixedPrice * Action.Obligation) %>%
  select(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
         FirmFixedPrice, weightFirmFixed,
         Action.Obligation) %>%
  group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(pctFirmFixed = sum(FirmFixedPrice) / n(), 
            wtpctFirmFixed = sum(weightFirmFixed) / sum(Action.Obligation))


#get the percent from historically disadvantaged vendors

historical.disadvantage <- office.frame %>%
  mutate(weightHistDis = HistDisAdv * Action.Obligation) %>%
  select(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward,
         HistDisAdv, weightHistDis,
         Action.Obligation) %>%
  group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward) %>%
  summarize(pctHistDis = sum(HistDisAdv) / n(), 
            wtpctHistDis = sum(weightHistDis) / sum(Action.Obligation))
#....
#pct of actions that are firm fixed price (Type.of.Contract) include the weighted val
#pct hDisadvantaged, check the git hub for list of variables recommend paste together
#and grepl for "YES" (double check), include the weighted val
#concentration values for offices for vendor (vendorId), geography (congressId)
#see below for example of start, need to add the other dimensions

vendorConc <- office.frame %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Office.ID, naicsTwo, Fiscal.Year, catAward, vendorId) %>%
    summarize(dollars = sum(Action.Obligation)) %>%
    ungroup() %>%
    arrange(Contracting.Office.ID, desc(dollars)) %>%
    group_by(Contracting.Office.ID) %>%
    mutate(rank = row_number(), pctOffice = cumsum(dollars) / sum(dollars),
           pctVendor = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(pctOffice)) %>%
    summarize(vendorConc = MESS::auc(pctVendor,pctOffice,type = 'spline')) 

#put them togeher
featureOffice <- offers %>%
    full_join(setaside, by = c('Contracting.Office.ID', 'naicsTwo', 'Fiscal.Year',
                               'catAward'))


