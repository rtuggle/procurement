##prep data for congressional analysis
require(ggplot2)
require(gridExtra)
require(dplyr)


#create a unique key for awards and districts, select needed variables
geo.frame <- fpds %>%
    select(uniqueId, Contracting.Group.ID,
           Place.of.Performance.Zip.Code, congressId, Action.Obligation,
           Funding.Department.Name, PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID, 
           Referenced..IDV.PIID, Modification.Number, 
           Principal.Place.of.Performance.State.Code,
           Congressional.District.Place.of..Performance,
           Contracting.Office.Name,Contracting.Office.ID,
           Contracting.Agency.Name)

#output funding by office by zipcode for mapping
zip.frame <- geo.frame %>%
    group_by(Contracting.Group.ID, Place.of.Performance.Zip.Code) %>%
    summarize(sumDolars = sum(Action.Obligation))

#write.csv(zip.frame, file = '../data/zipcodeData.csv')

#remove records with NA in CongressId
congressNoNa <- geo.frame %>%
    filter(!grepl("NA", congressId))

#get the total spending by Funding Department, rank, and get percentages

congress.total <- congressNoNa %>%
    select(Contracting.Group.ID, Action.Obligation) %>%
    group_by(Contracting.Group.ID) %>%
    summarize(totalDollars = sum(Action.Obligation))

congress.funding <- congressNoNa %>%
    group_by(Contracting.Group.ID, congressId) %>%
    summarize(total = sum(Action.Obligation)) %>%
    arrange(Contracting.Group.ID, desc(total)) %>%
    ungroup() %>%
    filter(total > 0) %>%
    group_by(Contracting.Group.ID) %>%
    mutate(rank = row_number(), 
           pctDollar = (cumsum(total) / sum(total)),
           pctDistrict = (cumsum(rank) / sum(rank)))

write.csv(congress.funding, file = '../data/geoConcentration.csv', na="", row.names = FALSE)


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



