#run some box plots

#create a unique key for awards and districts, select needed variables
df <- fpds %>%
    select(uniqueId, vendorId, Action.Obligation, Number.of.Offers.Received,
           Award.or.IDV.Type, Multiple.or.Single.Award.IDV,
           Funding.Department.Name, Funding.Department.ID,
           PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID,
           Referenced..IDV.PIID, Modification.Number,
           Vendor.Name, Vendor.State, Vendor.Country,
           Contracting.Office.ID,Contracting.Office.Name, Contracting.Office.Region,
           Contracting.Agency.ID, Contracting.Agency.Name,
           Contracting.Department.ID,Contracting.Department.Name) 

chart <- df %>%
    group_by(uniqueId, Award.or.IDV.Type) %>%
    summarize(dollars = sum(Action.Obligation), offers = max(Number.of.Offers.Received)) %>%
    ungroup() %>%
    filter(dollars != 0 & offers < 100) %>%
    arrange(desc(offers))

ggplot(chart, 
       aes(x = Award.or.IDV.Type, y = offers)) +
    geom_boxplot()