# Consolidation script to combine the subject data (raw matlab files) with fMRI liquid feedback 
# in the the runkey for fMRI and behavioral analyses

# packages
# Note, the packages must be installed for the script to work
library(dplyr)
library(tidyr)

# paths
path.subjectdata<-"/Trimmed/subject_data.txt"
path.runkey<-"runkey_withbaseline.csv"
path.output<-"/Users/debbieyee/Dropbox/MacAir-Transfer/2019-April/Trimmed/"

# Read in the data and format 
subjectdata<-read.csv(subjectdata.path, sep = "\t")
runkey<-read.csv(path.runkey)

# Merge data and the runkey, to include liquid info with the data
subject_data_combined <- subjectdata %>% left_join(runkey, by = c("subID","runID"))

# code to check that the two dataframes merged correctly:
table(runkey$liquid)
table(runkey$subID)
table(subset(subject_data_combined, subID==51)$liquid)

# write new dataframe
write.csv(x = subject_data_combined, file = paste0(out.path,"subject_data_full.csv"))
