##prep data for vendor analysis
require(dplyr)
require(ggplot2)
require(gridExtra)

#create a unique key for awards and districts, select needed variables
vendor.frame <- fpds %>%
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
vendNoNa <- vendor.frame %>%
    filter(!grepl("NA", vendorId))

#compare the full and vendNoNa datasets (TASK: bind together to make table)
summarize(vendor.frame, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))
summarize(vendNoNa, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))

#look for remaining records with more than one DUNS number
test <- vendNoNa %>%
    group_by(uniqueId) %>%
    summarize(mods = n(),
              duns = n_distinct(vendorId),
              list = paste(vendorId, 
                           collapse = "|")) %>%
    filter(duns > 1) %>%
    arrange(desc(duns))

#write the errors out for examination
write.csv(test, file = "../data/exceptionsVendors.csv", row.names = FALSE)

#get the total spending by Funding Department, rank, and get percentages
vendor.funding <- vendNoNa %>%
    group_by(Funding.Department.Name, vendorId) %>%
    summarize(total = sum(Action.Obligation)) %>%
    ungroup() %>%
    arrange(Funding.Department.Name, desc(total)) %>%
    group_by(Funding.Department.Name) %>%
  mutate(rank = row_number(),pctFund = (cumsum(total) / sum(total)) * 100,
         pctVendor = (cumsum(rank) / sum(rank) )*100)
        #pctVendor = cumsum(rank) / 500)

#chart the results
ggplot(data = filter(vendor.funding, grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctVendor, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line() +
    scale_x_log10()



ggplot(data = filter(vendor.funding, !grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctVendor, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line() +
    scale_x_log10()


#output data for Tableau
write.csv(vendor.funding, file = "~/Repositories/data/gsaFundVendor_v15APR16.csv", na = "", row.names = FALSE)
