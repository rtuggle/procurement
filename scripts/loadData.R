require(dplyr)

##script to create data frame of fpds data
#define the directory location of the data files 
datadir <- '../data/FPDS_20160414'

# read in list of Historically Disadvantaged Categories
hdis <- read.csv('data/listDisadvantaged.csv',stringsAsFactors = FALSE) %>% filter( Priviliged == "Yes")
hdis.list <- trimws(hdis$Vars)

#create function that takes the filename and adds it to the dataset
#will use this function in do.call below
read.helper <- function(infile,datadir,...){
    ## Function to read in csv and strip date information from file name
    temp <- read.csv(paste(datadir,infile,sep="/"), stringsAsFactors = F,na.strings = c("NA",""),...)
    temp$period <- gsub("FPDS","",infile) %>% gsub("\\.csv","",.)
    return(temp)
}

#get a list of files in the data directory
files <- list.files(datadir,pattern = "FPDSFULL[0-9_]*.csv")

#rowbind all the files together, with the added filename in the dataset
fpds <- do.call(rbind, lapply(files, function(x) read.helper(x,datadir)))

#strip the dollars from money columns
fpds$Action.Obligation <- gsub("[^0-9.]","",fpds$Action.Obligation) %>% 
    as.numeric()
fpds$Base.and.Exercised.Options.Value <- gsub("[^0-9.]","",fpds$Base.and.Exercised.Options.Value) %>% as.numeric()
fpds$Base.and.All.Options.Value <- gsub("[^0-9.]","",fpds$Base.and.All.Options.Value) %>% as.numeric()

#convert dates to date objects
fpds$Last.Modified.Date <- as.Date(fpds$Last.Modified.Date,"%m/%d/%Y")
fpds$Effective.Date <- as.Date(fpds$Effective.Date,"%m/%d/%Y")
fpds$Completion.Date <- as.Date(fpds$Completion.Date,"%m/%d/%Y")
fpds$Signed.Date <- as.Date(fpds$Signed.Date,"%m/%d/%Y")
fpds$Est..Ultimate.Completion.Date <- as.Date(fpds$Est..Ultimate.Completion.Date,"%m/%d/%Y")

# add duration value
fpds <- fpds %>% mutate(Est.Pd.Perf = as.numeric(difftime(Est..Ultimate.Completion.Date,
                                                          Effective.Date, units="days")))

#add categories for award type
Award.or.IDV.Type <- c('DELIVERY ORDER', 'PURCHASE ORDER', 'BPA CALL', 'DEFINITIVE CONTRACT',
         'IDC', 'FSS', 'BPA', 'GWAC', 'BOA')
catAward <- c(rep('Award', 4), rep('Vehicle', 5))
award <- data.frame(Award.or.IDV.Type, catAward, stringsAsFactors = FALSE)

#add categories for two digit NAICS
naics <- read.csv('data/lookNaics.csv', stringsAsFactors = FALSE)
naics$codeNaics <- as.character(naics$codeNaics)

#add a unique ids per task order and per vendor
fpds <- fpds %>% 
    ## add award class -> vehicle or award
    left_join(award, by = 'Award.or.IDV.Type') %>%
    ## add NAICS levels 2 through 5
    mutate(codeNaics = substr(NAICS.Code, 1, 2)) %>%
    left_join(naics[, c(2,3)], by = 'codeNaics') %>%
    mutate(naicsTwo = titleNaics2) %>%
    mutate(codeNaics = substr(NAICS.Code, 1, 3)) %>%
    left_join(naics[,c(2,4)], by = 'codeNaics') %>%
    mutate(naicsThree = titleNaics3) %>%
    mutate(codeNaics4 = substr(NAICS.Code, 1, 4)) %>%
    left_join(naics[,c(2,5)], by = 'codeNaics') %>%
    mutate(naicsFour = titleNaics4) %>%
    mutate(codeNaics5 = substr(NAICS.Code, 1, 5)) %>%
    left_join(naics[,c(2,6)], by = 'codeNaics') %>%
    mutate(naicsFive = titleNaics5) %>% select(-contains("titleNaics"))


fpds <- fpds %>%
  #add competed class
  mutate(compType = ifelse(
    grepl("NOT", Extent.Competed) | grepl("NON-C",Extent.Competed) |
      grepl("FOLLOW", Extent.Competed), "Not Competed",
    ifelse(!grepl("FAIR", Fair.Opportunity.Limited.Sources) &
             ! is.na(Fair.Opportunity.Limited.Sources), "Not Competed", "Competed"))) %>%
  mutate(bidType = ifelse(
    Number.of.Offers.Received > 1, "Multiple Offers",
    ifelse(Number.of.Offers.Received == 1, "One Offer", Number.of.Offers.Received))) %>%
  mutate(compCat = ifelse(
    compType == "Not Competed", "Not Competed",
    ifelse(bidType == "Multiple Offers", "Effectively Competed",
           ifelse(bidType == "One Offer", "One Bid", NA))))

# Add unique ids for bids, vendors, and congressional districts
fpds <- fpds %>%    mutate(uniqueId = paste(PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID, Referenced..IDV.PIID, sep='-'),
         vendorId = paste(DUNS.Number,Global.DUNS.Number,sep='-'), 
         congressId = paste(Principal.Place.of.Performance.State.Code,
                            Congressional.District.Place.of..Performance, sep='-'))

# Add Keys needed in Tablea for Congressional Districts
fpds <- fpds %>% mutate(CD.Place.Key=paste0("CD",Congressional.District.Place.of..Performance),
                        CD.Contractor.Key=paste0("CD",Congressional.District...Contractor))


## Add Contracting Region Groups
fpds$Contracting.Group.ID <- NA
fpds$Contracting.Group.ID[is.na(fpds$Contracting.Office.Region)] <- paste("NA -",fpds$Contracting.Agency.Name[is.na(fpds$Contracting.Office.Region)])
fpds$Contracting.Group.ID[!is.na(fpds$Contracting.Office.Region)] <- paste0("R",fpds$Contracting.Office.Region[!is.na(fpds$Contracting.Office.Region)],
                                                                           " - ",fpds$Contracting.Agency.Name[!is.na(fpds$Contracting.Office.Region)])

## Indicate whether contract was funded by GSA
fpds <- fpds %>% mutate(GSA.Funded = ifelse(Funding.Department.ID == "4700","Yes","No"))


## Add indicator of whether Vendor is member of historically disadvantaged group
fpds$histDisAdv <- 0
for (pop in hdis.list){
  fpds$histDisAdv[fpds[[pop]] == "YES"] <- 1
}

#write the frame to a file
write.csv(fpds,file="sourceFpds_18APR16_v2.csv",na="",row.names=F)
