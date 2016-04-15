##prep data for vendor analysis
require(dplyr)
require(ggplot2)

#create a unique key for awards and districts, select needed variables
vendor.frame <- fpds %>%
  mutate(uniqueId = paste(PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID,
                          Referenced..IDV.PIID, sep='-'),
         vendorId = paste(DUNS.Number,Global.DUNS.Number,
                          sep='-')) %>%
  select(uniqueId, vendorId, Action.Obligation,
         Funding.Department.Name, Funding.Department.ID,
         PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID,
         Referenced..IDV.PIID, Modification.Number,
         Vendor.Name, Vendor.State, Vendor.Country,
         Contracting.Office.ID,Contracting.Office.Name, Contracting.Office.Region,
         Contracting.Agency.ID, Contracting.Agency.Name,
         Contracting.Department.ID,Contracting.Department.Name) 

#for reference count how many distinct unique ids
cntVendors <- n_distinct(vendor.frame$uniqueId)
paste('the number of distinct vendors in dataset is:', 
      prettyNum(cntVendors, big.mark = ',', scientific = FALSE))

#remove records with NA in CongressId
noNa <- vendor.frame %>%
    filter(!grepl("NA", vendorId))

#compare the full and noNa datasets (TASK: bind together to make table)
summarize(vendor.frame, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))
summarize(noNa, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))

#look for remaining records with more than one district
#this is not meant to happen according to data model
test <- noNa %>%
    group_by(uniqueId) %>%
    summarize(districts = n_distinct(vendorId),
              list = paste(vendorId, 
                           collapse = "|")) %>%
    filter(districts > 1) %>%
    arrange(desc(districts))

#write the errors out for examination
write.csv(test, file = "../data/exceptionsVendors.csv", row.names = FALSE)

#get the total spending by Funding Department, rank, and get percentages
chart <- noNa %>%
    group_by(Funding.Department.Name, congressId) %>%
    summarize(total = sum(Action.Obligation)) %>%
    arrange(Funding.Department.Name, desc(total)) %>%
    group_by(Funding.Department.Name) %>%
    mutate(rank = row_number(), pctFund = (cumsum(total) / sum(total)) * 100, 
           #pctDistrict = cumsum(rank) / sum(rank))
        pctDistrict = cumsum(rank) / 435)

#chart the results
ggplot(data = filter(chart, grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctDistrict, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line() +
    scale_x_log10()


ggplot(data = filter(chart, !grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctDistrict, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line()


#output data for Tableau
write.csv(chart, file = "~/Repositories/data/gsaFundCongress_v15APR16.csv", na = "", row.names = FALSE)
