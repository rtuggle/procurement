##Build feature set for contracting offices

df <- fpds %>%
    select(Contracting.Office.ID, uniqueId, vendorId, congressId, NAICS.Code, 
           Action.Obligation, Number.of.Offers.Received, 
           Extent.Competed, Reason.Not.Awarded.To..Small.Business, 
           catAward, NAICS2, industrySector, 
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
           Is.Vendor.Business.Type...Women.Owned.Small.Business) 

add <- df %>%
    mutate(competed = ifelse(Other.Than.Full.and.Open.Competition == "", 1, 0)) %>%
    select(Extent.Competed, Fair.Opportunity.Limited.Sources, competed) %>%
    filter(grepl("FOLLOW", Fair.Opportunity.Limited.Sources) & competed == 0)

require(MESS)
vendor.funding <- df %>%
    filter(!grepl("NA", vendorId)) %>%
    group_by(Contracting.Office.ID, vendorId) %>%
    summarize(dollars = sum(Action.Obligation)) %>%
    ungroup() %>%
    arrange(Contracting.Office.ID, desc(dollars)) %>%
    group_by(Contracting.Office.ID) %>%
    mutate(rank = row_number(), pctFund = cumsum(dollars) / sum(dollars),
           pctVendor = cumsum(rank) / sum(rank)) %>%
    filter(!is.na(pctFund)) %>%
    summarize(concVendor = MESS::auc(pctVendor,pctFund,type = 'spline')) 



