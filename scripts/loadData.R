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

#write the frame to a file
save(fpds, file = 'gsaSource_15APR16.Rda')
