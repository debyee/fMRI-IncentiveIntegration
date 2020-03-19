# Script for Creating Stimulus onsets for fMRI Liquid Feedback
# Last updated 7/18/18
# Last updated 02/14/19
# Last updated 06/01/19

# Loading Packages
library(readr)
library(dplyr)
library(tidyr)


## =====READING IN AND FORMATTING TRIAL AND SUBJECT DATA=====

# Read in the stimulus onset data
stim_present_data <- read_delim("/Users/debbieyee/Box/CCPLab_Liquid_Studies_ACTIVE/Aging Motivation (SRNDNA)/Data/Trimmed/stim_present_data.txt",
"\t", escape_double = FALSE, trim_ws = TRUE)

# Read in the trial data
subject_data <- read_delim("/Users/debbieyee/Box/CCPLab_Liquid_Studies_ACTIVE/Aging Motivation (SRNDNA)/Data/Trimmed/subject_data.txt",
"\t", escape_double = FALSE, trim_ws = TRUE)
subject_data.trim<-subject_data %>% select(subID,runID,trial,moneyreward,ACC,Feedback,task)

# Read in runkey
runkey <- read.csv("/Users/debbieyee/Box/CCPLab_Liquid_Studies_ACTIVE/Aging Motivation (SRNDNA)/Data/Trimmed/runkey.csv")
runkey.liquid<-select(runkey,subID,runID,liquid)

# formatting the stimulus data to include money and liquid conditions
trial_data <- stim_present_data %>% 
  filter(stim_present=="cue"|stim_present=="ITI"|stim_present=="trialstart") %>%
  left_join(y = runkey.liquid, by = c("subID","runID")) %>%
  mutate(liquid=as.character(liquid)) %>%
  replace_na(list(liquid = "baseline")) %>%
  left_join(y = subject_data.trim, by = c("subID","runID","trial")) %>%
  mutate(condition=ifelse(Feedback=="liquidfeedback" | Feedback=="liquid feedback",liquid,"noreward"))

# Set output directory for where onset text files with be written
path.onsets<-"/Users/debbieyee/Box/CCPLab_Liquid_Studies_ACTIVE/Aging Motivation (SRNDNA)/Data/onsets/"

# ===== AIM 1: Extracting GLMs for Univariate Analysis =====
# Sustained/Block Files: BASE, JUICE, NEUT, SALT
# Transient/Event-Related Files: Juice_1,Juice_2,Juice_4,Neut_1,Neut_2,Neut_4,Salt_1,Salt_2,Salt_4,No_Reward

## LOOPING OVER SUBJECTS
sub.ids<-unique(trial_data$subID)
for (sid in sub.ids) { # sid<-sub.ids[1]

  # filter for single subject data
  data.sub<-filter(trial_data,subID==sid)
  
  # BLOCK: BASELINE onsets
  data.base<-filter(data.sub,liquid=="baseline",trial==1|trial==16|trial==17|trial==32|trial==33|trial==48)
  runs<-unique(data.base$runID)
  if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=3
  onset_base=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
  for (rid in 1:length(runs)) { # rid<-1
    onset_base<-(data.base %>% filter(runID==runs[rid],stim_present=="trialstart", 
                                      trial==1|trial==17|trial==33) %>% select(onset_exp))[[1]]
    offset_base<-(data.base %>% filter(runID==runs[rid],stim_present=="ITI", 
                                       trial==16|trial==32|trial==48) %>% select(onset_exp))[[1]]
    b1<-paste0(round(onset_base[1]),':',round(offset_base[1]-onset_base[1]))
    b2<-paste0(round(onset_base[2]),':',round(offset_base[2]-onset_base[2]))
    b3<-paste0(round(onset_base[3]),':',round(offset_base[3]-onset_base[3]))
    onset<-cbind(b1,b2,b3) # Combine the onset and duration
    if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
    onset_base[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
    onset_base[rid_ix,which(is.na(onset_base[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
  } # end for loop over baseline runs
  onset_base[which(onset_base[,1]=="*"),2:max.length]="" # Remove extra asterisks 
  
  # BLOCK: JUICE onsets
  data.juice<-filter(data.sub,liquid=="juice",trial==1|trial==16|trial==17|trial==32|trial==33|trial==48)
  runs<-unique(data.juice$runID)
  if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=3
  onset_block_juice=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
  for (rid in 1:length(runs)) { # rid<-1
    onset_juice<-(data.juice %>% filter(runID==runs[rid],stim_present=="trialstart", 
                                      trial==1|trial==17|trial==33) %>% select(onset_exp))[[1]]
    offset_juice<-(data.juice %>% filter(runID==runs[rid],stim_present=="ITI", 
                                       trial==16|trial==32|trial==48) %>% select(onset_exp))[[1]]
    b1<-paste0(round(onset_juice[1]),':',round(offset_juice[1]-onset_juice[1]))
    b2<-paste0(round(onset_juice[2]),':',round(offset_juice[2]-onset_juice[2]))
    b3<-paste0(round(onset_juice[3]),':',round(offset_juice[3]-onset_juice[3]))
    onset<-cbind(b1,b2,b3)
    if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
    onset_block_juice[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
    onset_block_juice[rid_ix,which(is.na(onset_block_juice[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
  }# end for loop over juice runs
  onset_block_juice[which(onset_block_juice[,1]=="*"),2:max.length]="" # Remove extra asterisks
  
  # BLOCK: NEUTRAL onsets
  data.neutral<-filter(data.sub,liquid=="neutral",trial==1|trial==16|trial==17|trial==32|trial==33|trial==48)
  runs<-unique(data.neutral$runID)
  if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=3
  onset_block_neutral=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
  for (rid in 1:length(runs)) { # rid<-1
    onset_neutral<-(data.neutral %>% filter(runID==runs[rid],stim_present=="trialstart", 
                                        trial==1|trial==17|trial==33) %>% select(onset_exp))[[1]]
    offset_neutral<-(data.neutral %>% filter(runID==runs[rid],stim_present=="ITI", 
                                         trial==16|trial==32|trial==48) %>% select(onset_exp))[[1]]
    b1<-paste0(round(onset_neutral[1]),':',round(offset_neutral[1]-onset_neutral[1]))
    b2<-paste0(round(onset_neutral[2]),':',round(offset_neutral[2]-onset_neutral[2]))
    b3<-paste0(round(onset_neutral[3]),':',round(offset_neutral[3]-onset_neutral[3]))
    onset<-cbind(b1,b2,b3)
    if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
    onset_block_neutral[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
    onset_block_neutral[rid_ix,which(is.na(onset_block_neutral[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
  }# end for loop over neutral runs
  onset_block_neutral[which(onset_block_neutral[,1]=="*"),2:max.length]="" # Remove extra asterisks
  
  # BLOCK: SALTWATER onsets
  data.salt<-filter(data.sub,liquid=="saltwater",trial==1|trial==16|trial==17|trial==32|trial==33|trial==48)
  runs<-unique(data.salt$runID)
  if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=3
  onset_block_salt=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
  for (rid in 1:length(runs)) { # rid<-1
    onset_salt<-(data.salt %>% filter(runID==runs[rid],stim_present=="trialstart", 
                                            trial==1|trial==17|trial==33) %>% select(onset_exp))[[1]]
    offset_salt<-(data.salt %>% filter(runID==runs[rid],stim_present=="ITI", 
                                             trial==16|trial==32|trial==48) %>% select(onset_exp))[[1]]
    b1<-paste0(round(onset_salt[1]),':',round(offset_salt[1]-onset_salt[1]))
    b2<-paste0(round(onset_salt[2]),':',round(offset_salt[2]-onset_salt[2]))
    b3<-paste0(round(onset_salt[3]),':',round(offset_salt[3]-onset_salt[3]))
    onset<-cbind(b1,b2,b3)
    if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
    onset_block_salt[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
    onset_block_salt[rid_ix,which(is.na(onset_block_salt[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
  }# end for loop over saltwater runs  
  onset_block_salt[which(onset_block_salt[,1]=="*"),2:max.length]="" # Remove extra asterisks

  # Write text files for onsets
  if (sid < 10) {
    # BLOCK onsets (BASELINE,JUICE,NEUTRAL,SALTWATER)
    write.table(onset_block_base, file=paste0(path.onsets,'dmblock/s0',sid,'_BLOCK_BASE.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_juice, file=paste0(path.onsets,'dmblock/s0',sid,'_BLOCK_JUICE.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_neutral, file=paste0(path.onsets,'dmblock/s0',sid,'_BLOCK_NEUT.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_salt, file=paste0(path.onsets,'dmblock/s0',sid,'_BLOCK_SALT.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
  } else {
    # BLOCK onsets (BASELINE,JUICE,NEUTRAL,SALTWATER)
    write.table(onset_block_base, file=paste0(path.onsets,'dmblock/s',sid,'_BLOCK_BASE.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_juice, file=paste0(path.onsets,'dmblock/s',sid,'_BLOCK_JUICE.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_neutral, file=paste0(path.onsets,'dmblock/s',sid,'_BLOCK_NEUT.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
    write.table(onset_block_salt, file=paste0(path.onsets,'dmblock/s',sid,'_BLOCK_SALT.txt'),
                row.names=FALSE,col.names=FALSE, quote = FALSE)
  }    
    
  # EVENT-RELATED: onsets for rewarded trials
  cond.liq<-c("juice","neutral","saltwater")
  cond.rew<-c(1,2,4)
  for (liq in cond.liq) { 
    for (rew in cond.rew) {
      # subset data based on conditions
      data.ER<-filter(data.sub,liquid==liq,moneyreward==rew,condition==liq,stim_present=="cue")
      runs<-unique(data.ER$runID)
      # iterate over each run for the liquid & money combination
      if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=16
      onset_event=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
      for (rid in 1:length(runs)) { # rid<-1
        #onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
        #onset_event[rid,]=c(onset,rep(NA,max.length-length(onset)))
        onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
        if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
        onset_event[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
        onset_event[rid_ix,which(is.na(onset_event[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
      } # end loop over runs
      onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks 
      #onset_event=onset_event[,-which(is.na(onset_event[rid_ix,]))] # Remove extra columns
      # Write text files for onsets
      if (sid < 10) {
        write.table(onset_event, file=paste0(path.onsets,'event_related/s0',sid,'_EV_',liq,'_',rew,'.txt'),
                    row.names=FALSE,col.names= FALSE,quote=FALSE)
      } else {
        write.table(onset_event, file=paste0(path.onsets,'event_related/s',sid,'_EV_',liq,'_',rew,'.txt'),
                    row.names=FALSE,col.names=FALSE,quote = FALSE)
      }
      
    } # end loop over monetary reward levels
  } # end loop over liquids
  
  # EVENT-RELATED: onsets for baseline trials (for MVPA analysis)
  cond.liq<-c("baseline")
  cond.rew<-c(1,2,4)
  for (rew in cond.rew) { # rew<-cond.rew[1]
    # subset data based on conditions
    data.ER<-filter(data.sub,liquid==cond.liq,moneyreward==rew,condition=="noreward",stim_present=="cue")
    runs<-unique(data.ER$runID)
    # iterate over each run for the liquid & money combination
    if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=16
    onset_event=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
    for (rid in 1:length(runs)) {
      onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
      onset_event[rid,]=c(onset,rep(NA,max.length-length(onset)))
    } # end loop over runs
    onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks  
    # Write text files for onsets
    if (sid < 10) {
      write.table(onset_event, file=paste0(path.onsets,'event_related_baseline/s0',sid,'_EV_',cond.liq,'_',rew,'.txt'),
                  row.names=FALSE,col.names= FALSE,quote=FALSE)
    } else {
      write.table(onset_event, file=paste0(path.onsets,'event_related_baseline/s',sid,'_EV_',cond.liq,'_',rew,'.txt'),
                  row.names=FALSE,col.names=FALSE,quote = FALSE)
    }
    
    } # end loop over money levels
  
  # EVENT-RELATED: onsets for not-rewarded trials
  data.sub.inc<-filter(data.sub,runType=="incentive")
  runs<-unique(data.sub.inc$runID)
  if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=50
  onset_event=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
  for (rid in 1:length(runs)) { # rid<-1
    data.ER<-filter(data.sub,runID==runs[rid],condition=="noreward",stim_present=="cue")
    #onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
    #onset_event[rid,]=c(onset,rep(NA,max.length-length(onset)))
    onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
    if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
    onset_event[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
    onset_event[rid_ix,which(is.na(onset_event[rid_ix,]))]="" # Replace NAs at end of onsets with empty spaces
  } #end loop over runs
  onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks
  # Write text files for onsets
  if (sid < 10) {
    write.table(onset_event, file=paste0(path.onsets,'event_related/s0',sid,'_EV_noreward.txt'),
                row.names=FALSE,col.names= FALSE,quote=FALSE)
  } else {
    write.table(onset_event, file=paste0(path.onsets,'event_related/s',sid,'_EV_noreward.txt'),
                row.names=FALSE,col.names=FALSE,quote = FALSE)
  }  

  # For Motion Plots 
  run.ids<-unique(data.sub$runID)
  TR = 2 #TR = 2 seconds
  for (rid in 1:length(run.ids)) {
    # subset run
    data.run<-filter(data.sub, runID==run.ids[rid], stim_present=="cue") %>% 
      select(onset_exp) %>%
      mutate(onset_exp_TR=onset_exp/TR) %>%
      select(onset_exp_TR)
    
    # Write text file for onsets for each run
    if (sid < 10) {
      write.table(data.run, file=paste0(path.onsets,'event_related_TR/s0',sid,'_run_',rid,'.txt'),
                  row.names=FALSE,col.names= FALSE,quote=FALSE)
    } else {
      write.table(data.run, file=paste0(path.onsets,'event_related_TR/s',sid,'_run_',rid,'.txt'),
                  row.names=FALSE,col.names=FALSE,quote = FALSE)
    }
  } # end for loop over runs
  
}# end for loop over subjects

# THIS MAY BE REDUNDANT CODE, CHECK THE FORLOOP ABOVE
# # Extract the event-related trials for baseline (still Aim 1)
# for (sid in sub.ids) {
#   # filter for single subject data
#   data.sub<-filter(trial_data,subID==sid)
#   
#   # baseline runs
#   cond.liq<-c("baseline")
#   cond.rew<-c(1,2,4)
#   for (rew in cond.rew) {
#     # subset data based on conditions
#     data.ER<-filter(data.sub,liquid==cond.liq,moneyreward==rew,condition=="noreward",stim_present=="cue")
#     runs<-unique(data.ER$runID)
#     # iterate over each run for the liquid & money combination
#     rm(onset_event)
#     max.length=16
#     onset_event=matrix(data = NA, nrow = length(runs), ncol = max.length)
#     for (rid in 1:length(runs)) {
#       onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
#       onset_event[rid,]=c(onset,rep(NA,max.length-length(onset)))
#     } # end loop over runs
#     onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks
#     # Write text files for onsets
#     if (sid < 10) {
#       write.table(onset_event, file=paste0(path.onsets,'event_related_baseline/s0',sid,'_EV_',cond.liq,'_',rew,'.txt'),
#                   row.names=FALSE,col.names= FALSE,quote=FALSE)
#     } else {
#       write.table(onset_event, file=paste0(path.onsets,'event_related_baseline/s',sid,'_EV_',cond.liq,'_',rew,'.txt'),
#                   row.names=FALSE,col.names=FALSE,quote = FALSE)
#     }
#     
#   } # end loop over monetary reward levels
# } # end loop over liquids



# ===== AIM 2: Extracting GLMs for Pattern Similarity Analysis (Multivariate) =====
# Sustained/Block Files: BASE, JUICE, NEUT, SALT (already written from loop above, dont need to do again)
# Transient/Event-Related Files:  Juice_1_run1,Juice_1_run2,Juice_2_run1,Juice_2_run2,Juice_4_run1,Juice_4_run2
#                                 Neut_1_run1,Neut_1_run_2,Neut_2_run1,Neut_2_run2,Neut_4_run1,Neut_4_run2,
#                                 Salt_1_run1,Salt_1_run2,Salt_2_run1,Salt_2_run2,Salt_4_Run1,Salt_4_run2
# NOTE: ignoring the accuracy here (not sure if we'll want to fix this for later)

## LOOPING OVER SUBJECTS
sub.ids<-unique(trial_data$subID)
for (sid in sub.ids) { # sid<-sub.ids[1]
  
  # filter for single subject data
  data.sub<-filter(trial_data,subID==sid)
  
  # EVENT-RELATED INCENTIVE: onsets for reward conditions (including both rewarded and unrewarded trials)
  cond.liq<-c("juice","neutral","saltwater")
  cond.rew<-c(1,2,4)
  for (liq in cond.liq) { # liq<-cond.liq[1]
    for (rew in cond.rew) { # rew<-cond.rew[1]
      # subset data based on conditions
      data.ER<-filter(data.sub,liquid==liq,moneyreward==rew,stim_present=="cue") # IGNORE ACCURACY HERE
      #data.ER<-filter(data.sub,liquid==liq,moneyreward==rew,condition==liq,stim_present=="cue")
      # iterate over each run for the liquid & money combination
      runs<-unique(data.ER$runID)
      if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=16
      for (rid in 1:length(runs)) { # rid<-1
        onset_event=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
        onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
        if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
        onset_event[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assign onset values to matrix
        onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks 
        # Write text files for onsets
        if (sid < 10) {
          write.table(onset_event, file=paste0(path.onsets,'event_related_byrun/s0',sid,'_EV_',liq,'_',rew,'_run',rid,'.txt'),
                      row.names=FALSE,col.names= FALSE,quote=FALSE)
        } else {
          write.table(onset_event, file=paste0(path.onsets,'event_related_byrun/s',sid,'_EV_',liq,'_',rew,'_run',rid,'.txt'),
                      row.names=FALSE,col.names=FALSE,quote = FALSE)
          } # end if-else statement
      } # end loop over runs
    } # end loop over monetary reward levels
  } # end loop over liquids
  
  # EVENT-RELATED TASK: onsets for different task conditions
  cond.liq<-c("juice","neutral","saltwater")
  cond.rew<-c(1,2,4)
  cond.task<-c("letter","digit")
  for (liq in cond.liq) { # liq<-cond.liq[1]
    for (rew in cond.rew) { # rew<-cond.rew[1]
      for (tid in cond.task) { # tid<-cond.task[1]
      # subset data based on conditions
      data.ER<-filter(data.sub,liquid==liq,moneyreward==rew,task==tid,stim_present=="cue") # IGNORE ACCURACY HERE
      #data.ER<-filter(data.sub,liquid==liq,moneyreward==rew,condition==liq,stim_present=="cue")
      # iterate over each run for the liquid & money combination
      runs<-unique(data.ER$runID)
      if (sid < 18) {num.runs.towrite=7} else {num.runs.towrite=8}; max.length=16
      for (rid in 1:length(runs)) { # rid<-1
        onset_event=matrix(data = "*", nrow = num.runs.towrite, ncol = max.length)
        onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
        if (sid < 18) {rid_ix=runs[rid]-7} else {rid_ix=runs[rid]-6} # Define run index for assignment in matrix
        onset_event[rid_ix,]=c(onset,rep(NA,max.length-length(onset)))  # Assigns onset values to matrix
        onset_event[which(onset_event[,1]=="*"),2:max.length]="" # Remove extra asterisks 
        onset_event=onset_event[,-which(is.na(onset_event[rid_ix,]))] # Remove extra columns
        # Write text files for onsets
        if (sid < 10) {
          write.table(onset_event, file=paste0(path.onsets,'event_related_byrun_TASK/s0',sid,'_EV_',liq,'_',rew,'_run',rid,'_',tid,'.txt'),
                      row.names=FALSE,col.names= FALSE,quote=FALSE)
        } else {
          write.table(onset_event, file=paste0(path.onsets,'event_related_byrun_TASK/s',sid,'_EV_',liq,'_',rew,'_run',rid,'_',tid,'.txt'),
                      row.names=FALSE,col.names=FALSE,quote = FALSE)
          } # end if-else statement
        } # end loop over runs
      } # end loop over tasks 
    } # end loop over monetary reward levels
  } # end loop over liquids  
  
  
  # # EVENT-RELATED: onsets for not-rewarded trials
  # data.sub.inc<-filter(data.sub,runType=="incentive")
  # runs<-unique(data.sub.inc$runID)
  # rm(onset_event)
  # max.length=50
  # onset_event=matrix(data = NA, nrow = length(runs), ncol = max.length)
  # for (rid in 1:length(runs)) {
  #   data.ER<-filter(data.sub,runID==runs[rid],condition=="noreward",stim_present=="cue")
  #   onset<-round((data.ER %>% filter(runID==runs[rid]) %>% select(onset_exp))[[1]],1)
  #   onset_event[rid,]=c(onset,rep(NA,max.length-length(onset)))
  # } #end loop over runs
  # 
  # # Write text files for onsets
  # if (sid < 10) {
  #   write.table(onset_event, file=paste0(path.onsets,'event_related/s0',sid,'_EV_noreward.txt'),
  #               row.names=FALSE,col.names= FALSE,quote=FALSE)
  # } else {
  #   write.table(onset_event, file=paste0(path.onsets,'event_related/s',sid,'_EV_noreward.txt'),
  #               row.names=FALSE,col.names=FALSE,quote = FALSE)
  # }  
  
} # end for loop over subjects

