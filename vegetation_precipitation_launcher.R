####
####  Set stratified randomized datasets from Command Line Arguments
####

args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
do_id <- as.numeric(myargument)