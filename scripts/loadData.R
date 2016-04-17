require(dplyr)

##script to create data frame of fpds data
#define the directory location of the data files 
datadir <- '~/Repositories/data/FPDS_20160414'

#create function that takes the filename and adds it to the dataset
#will use this function in do.call below
read.helper <- function(infile,datadir,...){
    ## Function to read in csv and strip date information from file name
    temp <- read.csv(paste(datadir,infile,sep="/"), stringsAsFactors = F,...)
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

#add categories for award type
Award.or.IDV.Type <- c('DELIVERY ORDER', 'PURCHASE ORDER', 'BPA CALL', 'DEFINITIVE CONTRACT',
         'IDC', 'FSS', 'BPA', 'GWAC', 'BOA')
catAward <- c(rep('Award', 4), rep('Vehicle', 5))
award <- data.frame(Award.or.IDV.Type, catAward)

#add categories for two digit NAICS
naics <- read.csv('~/Repositories/data/naics2.csv', stringsAsFactors = FALSE)
naics <- naics %>%
    mutate(NAICS2 = as.character(NAICS2))

#add a unique ids per task order and per vendor
fpds <- fpds %>% 
    left_join(award, by = 'Award.or.IDV.Type') %>%
    mutate(NAICS2 = substr(NAICS.Code, 1, 2)) %>%
    left_join(naics, by = 'NAICS2') %>%
    mutate(uniqueId = paste(PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID,
                                         Referenced..IDV.PIID, sep='-'),
                        vendorId = paste(DUNS.Number,Global.DUNS.Number,
                                         sep='-'), 
                        congressId = paste(Principal.Place.of.Performance.State.Code,
                                           Congressional.District.Place.of..Performance,
                                           sep='-'))


#write the frame to a file
# save(fpds, file = 'gsaSource_15APR16.Rda')
