##script to create data frame of fpds data
#define the directory location of the data files 
datadir <- '~/Repositories/data/FPDS_20160414'

#create function that takes the filename and adds it to the dataset
#will use this function in do.call below
require(dplyr)
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

#strip the dollars from Action.Obligation
fpds$Action.Obligation <- gsub("[^0-9.]","",fpds$Action.Obligation) %>% 
    as.numeric()

#write the frame to a file
save(fpds, file = 'gsaSource_15APR16.Rda')
