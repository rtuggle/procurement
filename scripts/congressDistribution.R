##prep data for congressional analysis
require(ggplot2)
require(gridExtra)
require(dplyr)


#create a unique key for awards and districts, select needed variables
congress.frame <- fpds %>%
    select(uniqueId, congressId, Action.Obligation,
           Funding.Department.Name, PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID, 
           Referenced..IDV.PIID, Modification.Number, 
           Principal.Place.of.Performance.State.Code,
           Congressional.District.Place.of..Performance,
           Contracting.Office.Name,Contracting.Office.ID,
           Contracting.Agency.Name,Contracting.Agency.Name) 

#for reference count how many distinct unique ids
cntAwards <- n_distinct(congress.frame$uniqueId)
paste('the number of distinct awards in dataset is:', 
      prettyNum(cntAwards, big.mark = ',', scientific = FALSE))

#remove records with NA in CongressId
congressNoNa <- congress.frame %>%
    filter(!grepl("NA", congressId))

#compare the full and congressNoNa datasets (TASK: bind together to make table)
summarize(congress.frame, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))
summarize(congressNoNa, distinct = n_distinct(uniqueId), dollars = sum(Action.Obligation))

#look for remaining records with more than one district
#this is not meant to happen according to data model
test <- congressNoNa %>%
    group_by(uniqueId) %>%
    summarize(districts = n_distinct(congressId),
              list = paste(congressId, 
                           collapse = "|")) %>%
    filter(districts > 1) %>%
    arrange(desc(districts))

#write the errors out for examination
write.csv(test, file = "~/Repositories/data/exceptionsCongress.csv", row.names = FALSE)

#get the total spending by Funding Department, rank, and get percentages
congress.funding <- congressNoNa %>%
    group_by(Funding.Department.Name, congressId) %>%
    summarize(total = sum(Action.Obligation)) %>%
    arrange(Funding.Department.Name, desc(total)) %>%
    group_by(Funding.Department.Name) %>%
    mutate(rank = row_number(), 
           pctFund = (cumsum(total) / sum(total)) * 100,
           pctDistrict = (cumsum(rank) / sum(rank)))


#graph the results
ggplot(data = filter(congress.funding, grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctDistrict, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line() +
    scale_x_log10()


ggplot(data = filter(congress.funding, !grepl("DEPARTMENT",Funding.Department.Name)),
       aes(x = pctDistrict, y = pctFund, 
           group = Funding.Department.Name,
           colour = Funding.Department.Name)) +
    geom_line()


#output data for Tableau
write.csv(congress.funding, file = "gsaFundCongress_v15APR16.csv", na = "", row.names = FALSE)


########################

#get the total spending by Contracting Office, rank, and get percentages
congress.con.office <- congressNoNa %>%
  group_by(Contracting.Office.Name, congressId) %>%
  summarize(total = sum(Action.Obligation)) %>%
  arrange(Contracting.Office.Name, desc(total)) %>%
  group_by(Contracting.Office.Name) %>%
  mutate(rank = row_number(), 
         pctContract = (cumsum(total) / sum(total)) * 100,
         pctDistrict = (cumsum(rank) / sum(rank)))


#graph the results
ggplot(data = filter(congress.con.office,grepl("OFFICE",Contracting.Office.Name)),
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Office.Name,
           colour = Contracting.Office.Name)) +
  geom_line() +
  scale_x_log10()


ggplot(data = filter(congress.con.office, !grepl("OFFICE",Contracting.Office.Name) & grepl("FAS",Contracting.Office.Name)),
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Office.Name,
           colour = Contracting.Office.Name)) +
  geom_line() + scale_x_log10()

ggplot(data = filter(congress.con.office, 
                     !grepl("OFFICE",Contracting.Office.Name) & !grepl("FAS",Contracting.Office.Name) & grepl("PBS",Contracting.Office.Name)),
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Office.Name,
           colour = Contracting.Office.Name)) +
  geom_line() + scale_x_log10()

ggplot(data = filter(congress.con.office, 
                     !grepl("OFFICE",Contracting.Office.Name) & 
                       !grepl("FAS",Contracting.Office.Name) & 
                       !grepl("PBS",Contracting.Office.Name) &
                       grepl("CENTER",Contracting.Office.Name)),
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Office.Name,
           colour = Contracting.Office.Name)) +
  geom_line() + scale_x_log10()


ggplot(data = filter(congress.con.office, 
                     !grepl("OFFICE",Contracting.Office.Name) & 
                       !grepl("FAS",Contracting.Office.Name) & 
                       !grepl("PBS",Contracting.Office.Name) &
                       !grepl("CENTER",Contracting.Office.Name)),
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Office.Name,
           colour = Contracting.Office.Name)) +
  geom_line() + scale_x_log10()


#output data for Tableau
write.csv(congress.con.office, file = "gsaContractOfficeCongress_v15APR16.csv", na = "", row.names = FALSE)

########################

#get the total spending by Contracting Agency, rank, and get percentages
congress.con.agency <- congressNoNa %>%
  group_by(Contracting.Agency.Name, congressId) %>%
  summarize(total = sum(Action.Obligation)) %>%
  arrange(Contracting.Agency.Name, desc(total)) %>%
  group_by(Contracting.Agency.Name) %>%
  mutate(rank = row_number(), 
         pctContract = (cumsum(total) / sum(total)) * 100,
         pctDistrict = (cumsum(rank) / sum(rank)))


#graph the results
ggplot(data = congress.con.agency ,
       aes(x = pctDistrict, y = pctContract, 
           group = Contracting.Agency.Name,
           colour = Contracting.Agency.Name)) +
  geom_line() +
  scale_x_log10()


#output data for Tableau
write.csv(congress.con.agency, file = "gsaContractAgencyCongress_v15APR16.csv", na = "", row.names = FALSE)



