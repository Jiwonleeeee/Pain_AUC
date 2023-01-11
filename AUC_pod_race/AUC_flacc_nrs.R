rm(list=ls())
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

raw_data <- read_excel("/Users/wonny/Downloads/CHMC/AUC pod race/DATA/pain scores flacc and nrs_12152022.xlsx") %>% as.data.frame
add_data <- read_excel("/Users/wonny/Downloads/CHMC/AUC pod race/DATA/pain scores flacc and nrs_01052023.xlsx") %>% as.data.frame


# unique(add_data[,c("PAT_ID","SURGERY_DATE")]) %>% nrow
# > unique(add_data[,c("PAT_ID","SURGERY_DATE")]) %>% nrow
# [1] 2136
# > n
# [1] 2123

IDmat <- unique(raw_data[,c("PAT_ID","SURGERY_DATE")])
n <- nrow(IDmat) 

newID <- numeric(n)
for(i in 1:n){
  newID[i] <- paste0("ID",i)
}
IDmat <- cbind(IDmat, newID)

## new raw data
raw_data <- inner_join(raw_data, IDmat, by=c("PAT_ID","SURGERY_DATE"))
n <- length(unique(raw_data$newID))
ID <- unique(raw_data$newID)


# compare raw_data vs add_data
unique(raw_data$PAT_ID) %>% length
unique(add_data$PAT_ID) %>% length


## count # of observations in each period
# tabulate does not count pod2 if there's no pod2
# manual count
count_ftn <- function(id_input, data_input){
  
  pod_vector <- subset(data_input, PAT_ID==id_input)$POD
  
  count0 <- sum(pod_vector==0)
  count1 <- sum(pod_vector==1)
  count2 <- sum(pod_vector==2)
  count012 <- c(count0, count1, count2)
  
  return(count012)
}

count_list <- lapply(ID, count_ftn, data_input = raw_data)

# count matrix: Npod0, Npod1, Npod2
count_matrix <- matrix(0, nrow = length(ID), ncol = 3)
for(i in 1:length(ID)){
  count_matrix[i,] <- count_list[[i]]
}


# check if there are multiple surgeries
# multiOP_indi <- numeric(n)
# for(i in 1:n){
#   temp_data <- subset(raw_data, PAT_ID==ID[i])
#   if(length(unique(temp_data$TIME_IN_OOR_2))!=1) multiOP_indi[i] <- 1
#   
# }
# table(multiOP_indi)
# > table(multiOP_indi)
# multiOP_indi
# 0    1 
# 1746  168 
# which(multiOP_indi==1)

# n = 1914
# nrow(unique(raw_data[c("PAT_ID","SURGERY_DATE")])) = 2123
# n should be this nrow
# (It was not multiple surgery case, it was just a matter of defining unique ID)

# There some patients whose surgery carried over midnight, 
# which means their surgery start date and the end date are not the same
# The POD for these cases are recorded as POD1, but this should be POD0.

corrected_indi <- numeric(n)
for(i in 1:n){
  temp_data <- subset(raw_data, newID==ID[i])
  
  sur_startdate <- temp_data$SURGERY_DATE[1]
  sur_enddate<- as.Date(ymd_hms(temp_data$TIME_IN_OOR_2[1]))
  
  if(is.na(sur_enddate)){
    sur_enddate <- temp_data$TIME_IN_OOR_2[1]
  }
  
  if(sur_startdate!=sur_enddate){
    temp_data$POD <- temp_data$POD-1
    corrected_indi[i] <- 1
  }
  
  raw_data[raw_data$newID==ID[i],] <- temp_data
  
  print(i)
}
table(corrected_indi)
# > table(corrected_indi)
# corrected_indi
# 0    1 
# 2064   59 


combined_data <- subset(raw_data, newID==ID[1])
## bring the data from add_data for those whose corrected_indi == 1
for(i in 2:n){
  
  raw_temp_data <- subset(raw_data, newID==ID[i])
  
  if(corrected_indi[i]==1){
    add_temp_data <- subset(add_data, PAT_ID==raw_temp_data$PAT_ID[1] & SURGERY_DATE==raw_temp_data$SURGERY_DATE[1])
    add_temp_data$newID <- rep(paste0("ID",i), nrow(add_temp_data))
    combined_data <- rbind(combined_data, add_temp_data)
  }else{
    combined_data <- rbind(combined_data, raw_temp_data)
  }
  
  print(i)
}
table(combined_data$POD)
# > table(combined_data$POD)
# 
# 0    1    2    3 
# 8301 5310 1890   59 


raw_data <- combined_data
# do POD - 1 again
for(i in 1:n){
  temp_data <- subset(raw_data, newID==ID[i])
  
  sur_startdate <- temp_data$SURGERY_DATE[1]
  sur_enddate<- as.Date(ymd_hms(temp_data$TIME_IN_OOR_2[1]))
  
  if(is.na(sur_enddate)){
    sur_enddate <- temp_data$TIME_IN_OOR_2[1]
  }
  
  if(sur_startdate!=sur_enddate){
    temp_data$POD <- temp_data$POD-1
  }
  
  raw_data[raw_data$newID==ID[i],] <- temp_data
  
  print(i)
}
# save(raw_data, file="/Users/wonny/Downloads/CHMC/Pain AUC/pod_race_result/raw_data_flacc_nrs.rda")

# > table(raw_data$POD)
# 
# 0    1    2 
# 8822 4988 1750 


# no recorded end "time", only date is available
no_time <- numeric(n)
for(i in 1:n){
  temp_data <- subset(raw_data, newID==ID[i])
  sur_enddate<- as.Date(ymd_hms(temp_data$TIME_IN_OOR_2[1]))
  if(is.na(sur_enddate)){
    no_time[i] <- 1
  }
}

table(no_time)

which(no_time==1)
View(subset(raw_data, newID=="ID2025"))

# set AUC0, duration0 missing for this one later


AUC_ftn <- function(data_input, count_input){
  
  # imputation function
  imput_ftn <- function(p1_input, p2_input, acc1_input, acc2_input, pod_input, pod0_duration_input){
    pod_indicator <- pod_input + 1
    cuttime <- switch (pod_indicator,
                       pod0_duration_input,
                       pod0_duration_input + 24,
                       pod0_duration_input + 48
    )
    acc1_input <- as.numeric(acc1_input);acc2_input <- as.numeric(acc2_input)
    if(p2_input >= p1_input){
      x <- (p2_input-p1_input)*(cuttime-acc1_input)/(acc2_input-acc1_input) + p1_input
    }else{
      x <- (p1_input-p2_input)*(acc2_input-cuttime)/(acc2_input-acc1_input) + p2_input
    }
    
    return(as.numeric(x))
  }
  
  sur_endtime <- data_input$TIME_IN_OOR_2[1]
  pod0_duration <- difftime(data_input$SURGERY_DATE[1]+3600*24, sur_endtime, units = "hours")
  
  if(pod0_duration<0){
    pod0_duration <- difftime(as.Date(ymd_hms(sur_endtime)), sur_endtime, units = "hours")+24
  }
  
  
  pod1_duration <- pod0_duration + 24
  pod2_duration <- pod0_duration + 48   ## accumulative, so this is the total duration
  
  pod <- data_input$POD # pod
  time_vec <- data_input$RECORDED_TIME2 # pain recorded time
  ps_vec <- data_input$pain_score # pain score
  
  
  # order
  temp_order <- data.frame(ps=ps_vec, ts=time_vec, pod=pod)
  temp_order <- temp_order[order(temp_order$ts),]
  ps_vec <- temp_order$ps; time_vec <- temp_order$ts; pod <- temp_order$pod
  
  # remove the pain scores and time < surgery end time
  temp <- difftime(sur_endtime, time_vec)
  if(sum(temp>0)>=1){
    time_vec <- time_vec[-which(temp>0)]
    ps_vec <- ps_vec[-which(temp>0)]
    pod <- pod[-which(temp>0)]
  }
  
  ps0 <- ps_vec[pod==0]
  ps1 <- ps_vec[pod==1]
  ps2 <- ps_vec[pod==2]
  
  time0 <- time_vec[pod==0]
  time1 <- time_vec[pod==1]
  time2 <- time_vec[pod==2]
  
  
  
  auc0 <- 0
  auc1 <- 0
  auc2 <- 0
  
  
  Npod0 <- sum(pod==0)
  Npod1 <- sum(pod==1)
  Npod2 <- sum(pod==2)
  
  # print(c(Npod0,Npod1,Npod2))
  
  
  auc_save <- 0
  acc_time <- 0
  missing_indicator <- numeric(3)
  # POD0
  if(Npod0!=0){ # if 0 -> auc_work = 0
    
    # first one
    lag_time <- difftime(time0[1], sur_endtime, units="hours")
    auc_work <- ps0[1] * lag_time
    
    
    auc_save <- auc_save + auc_work
    acc_time <- acc_time + lag_time
    
    
    # if only one ps
    if(Npod0==1){
      
      # this should be the last one
      if(Npod1==0){
        lag_time <- pod0_duration - difftime(time0[1], sur_endtime, units="hours")
        auc_work <- ps0[1] * lag_time
        
        
      }else{ # imputation
        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time1[1], time0[Npod0], units="hours")
        
        imputed_score <- imput_ftn(ps0[Npod0], ps1[1], acc1, acc2, 0, pod0_duration)
        auc_work <-  (ps0[Npod0] + imputed_score) * lag_time /2
        
      }
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      
    }else{
      # else -> we need a middle part
      
      for(i in 1:(Npod0-1)){
        
        lag_time <- difftime(time0[i+1], time0[i], units="hours")
        auc_work <- (ps0[i+1]+ps0[i]) * lag_time / 2
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
      
      # last : imputation if Npod1!=0, not if Npod1==0
      if(Npod1!=0){
        
        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time1[1], time0[Npod0], units = "hours")
        
        imputed_score <- imput_ftn(ps0[Npod0], ps1[1], acc1, acc2, 0, pod0_duration)
        auc_work <-  (ps0[Npod0] + imputed_score) * lag_time /2
        
        
        
      }else{
        
        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        auc_work <- ps0[Npod0] * lag_time
        
        
      }
      
      auc_save <- auc_save + auc_work
      acc_time <- pod0_duration
    }
    
    
    auc0 <- auc_save
    
  }else{
    acc_time <- pod0_duration
    missing_indicator[1] <- 1
  }# POD0 end ################################################
  
  # POD1 start ###################################################
  if(Npod1!=0){
    
    # first: Npod0=0 -> no imputation
    if(Npod0==0){
      lag_time <- difftime(time1[1], sur_endtime, units = "hours") - acc_time
      auc_work <- ps1[1] * lag_time
      
      
    }else{ # imputation
      lag_time <- difftime(time1[1], sur_endtime, units = "hours") - pod0_duration
      auc_work <- (imputed_score + ps1[1]) * lag_time/2
      
      
    } # first area end
    
    auc_save <- auc_save + auc_work
    acc_time <- acc_time + lag_time
    
    # if there's only one -> check POD2 and this is the last
    if(Npod1==1){
      
      if(Npod2!=0){ # imputation
        
        lag_time <- pod0_duration + 24 - difftime(time1[Npod1], sur_endtime, units = "hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time2[1], time1[Npod1], units = "hours")
        
        imputed_score <- imput_ftn(ps1[Npod1], ps2[1], acc1, acc2, 1, pod0_duration)
        auc_work <-  (ps1[Npod1] + imputed_score) * lag_time /2
        
        
      }else{  # no imputation
        lag_time <- pod1_duration - difftime(time1[Npod1], sur_endtime, units = "hours")
        auc_work <- ps1[Npod1] * lag_time
        
        
      }
      acc_time <- acc_time + lag_time
      auc_save <- auc_save + auc_work
      
    }else{ # if Npod1 >1
      
      for(i in 1:(Npod1-1)){
        lag_time <- difftime(time1[i+1], time1[i], units="hours")
        auc_work <- (ps1[i+1]+ps1[i]) * lag_time / 2
        
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
      
      # last part
      if(Npod2!=0){ # imputation
        
        lag_time <- pod0_duration + 24 - difftime(time1[Npod1], sur_endtime, units = "hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time2[1], time1[Npod1], units = "hours")
        
        imputed_score <- imput_ftn(ps1[Npod1], ps2[1], acc1, acc2, 1, pod0_duration)
        auc_work <-  (ps1[Npod1] + imputed_score) * lag_time /2
        
        
        
      }else{  # no imputation
        lag_time <- pod1_duration - difftime(time1[Npod1], sur_endtime, units = "hours")
        auc_work <- ps1[Npod1] * lag_time
        
        
      }
      acc_time <- acc_time + lag_time
      auc_save <- auc_save + auc_work
      
    }
    
    auc1 <- auc_save - auc0
  }else{
    acc_time <- pod1_duration
    missing_indicator[2] <- 1
  } # POD1 end
  
  ## POD2 start
  if(Npod2!=0){
    if(Npod2==1){
      
      if(Npod1==0){
        auc_work <- ps2[1] * 24
        auc_save <- auc_save + auc_work
      }else{
        # imputation and last one
        lag_time <-  difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
        auc_work <- (imputed_score + ps2[1]) * lag_time/2
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
        
        # last one
        
        # last
        lag_time <- pod2_duration - difftime(time2[Npod2], sur_endtime, units = "hours")
        auc_work <- ps2[Npod2] * lag_time
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
        
      }
      
      
    }else{
      
      # first: Npod1=0 -> no imputation
      if(Npod1==0){
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
        auc_work <- ps2[1] * lag_time
        
        
      }else{ # imputation
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
        auc_work <- (imputed_score + ps2[1]) * lag_time/2
        
      }
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      # middle
      for(i in 1:(Npod2-1)){
        lag_time <- difftime(time2[i+1], time2[i], units="hours")
        auc_work <- (ps2[i+1]+ps2[i]) * lag_time / 2
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
      
      
      # last
      lag_time <- pod2_duration - difftime(time2[Npod2], sur_endtime, units = "hours")
      auc_work <- ps2[Npod2] * lag_time
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      
    }
    auc2 <- auc_save - (auc0 + auc1)
    
  }else{
    missing_indicator[3] <- 1
  }# POD2 end
  
  
  return(list(auc0=auc0, auc1=auc1, auc2=auc2, pod0=pod0_duration, missing=missing_indicator))
} # function end

result <- matrix(0, n, 9)
colnames(result) <- c("AUC0","AUC1","AUC2","Duration0","Duration1","Duration2","Missing0","Missing1","Missing2")

for(i in 1:n){
  
  if(i==2025){
    result[i,1] <- 0
    result[i,2] <- 0
    result[i,3] <- 0
    result[i,4] <- 0
    result[i,7:9] <- c(1,1,1)
  }else{
    temp_data <- subset(raw_data, newID==ID[i])
    temp <- AUC_ftn(temp_data, count_matrix[i,])
    
    result[i,1] <- temp$auc0
    result[i,2] <- temp$auc1
    result[i,3] <- temp$auc2
    result[i,4] <- temp$pod0
    result[i,7:9] <- temp$missing
    
  }
  
  print(i)
}

result[,5] <- 24
result[,6] <- 24

result <- as.data.frame(result)

apply(result[,c(1:3)],2,range)
range(result$Duration0)
# save
save(result, count_matrix, file="race_auc_flacc_nrs.rda" )

## check ##


