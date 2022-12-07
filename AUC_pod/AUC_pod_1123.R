rm(list=ls())
setwd("/Users/wonny/Downloads/CHMC/Pain AUC/DATA")
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

raw_data <- read_excel("auc_use_11172022.xlsx") %>% as.data.frame
ID <- unique(raw_data$record_id)
n <- length(ID)

# example data
# EX <- subset(raw_data, record_id==ID[40])
# EX$pain_score[EX$POD==2]
# 
# ps_vec <- EX$pain_score
# time_vec <- EX$painDT
# time_vec[order(time_vec)][1] < EX$set_new[1]
# pod <- EX$POD
# 
# temp_order <- data.frame(ps=ps_vec, ts=time_vec, pod=pod)
# temp_order <- temp_order[order(temp_order$ts),]
# ps_vec <- temp_order$ps; time_vec <- temp_order$ts; pod <- temp_order$pod
# 
# EX$ps <- ps_vec
# EX$time <- time_vec
# EX$pod <- pod
# 
# View(EX)


# check if there's any patient who has earlier pain score recorded time than the surgery end time
# indicator <- numeric(n)
# for(i in 1:n){
#   temp_data <- subset(raw_data, record_id==ID[i])
#   time_vec <- temp_data$painDT
#   time_vec <- time_vec[order(time_vec)]
#   indicator[i] <- ifelse(time_vec[1] < temp_data$set_new[1], 1, 0)
# }
# 
# table(indicator)  # 12 patients
# which(indicator==1) # 281 301 338 360 364 383 386 391 608 624 629 641
# ID[which(indicator==1)]

# example
# EX <- subset(raw_data, record_id==ID[360])
# View(subset(raw_data, record_id==ID[360]))
# 
# difftime(EX$set_new, EX$painDT) # if this is positive, that time & score should be removed


# extract excel
# Offpatients <- filter(raw_data, record_id %in% ID[which(indicator==1)] )
# sortdata <- subset(Offpatients, record_id==ID[281])
# sortdata <- sortdata[order(sortdata$painDT),]
# 
# for(i in which(indicator==1)[2:12]){
#   temp_set <- subset(Offpatients, record_id==ID[i])
#   temp_set <- temp_set[order(temp_set$painDT),]
#   sortdata <- rbind(sortdata, temp_set)
#   print(i)
# }

# setwd("/Users/wonny/Downloads")
# write.xlsx(sortdata, sheetName="sheet1", file="PatientsInformation.xlsx")


## count # of observations in each period
# tabulate does not count pod2 if there's no pod2
# manual count
count_ftn <- function(id_input, data_input){
  
  pod_vector <- subset(data_input, record_id==id_input)$POD
  
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
  
  sur_endtime <- data_input$set_new[1]
  pod0_duration <- difftime(data_input$dos[1]+3600*24, sur_endtime, units = "hours")
  pod1_duration <- pod0_duration + 24
  pod2_duration <- pod0_duration + 48   ## accumulative, so this is the total duration
  
  pod <- data_input$POD # pod
  time_vec <- data_input$painDT # pain recorded time
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
  temp_data <- subset(raw_data, record_id==ID[i])
  temp <- AUC_ftn(temp_data, count_matrix[i,])
  
  result[i,1] <- temp$auc0
  result[i,2] <- temp$auc1
  result[i,3] <- temp$auc2
  result[i,4] <- temp$pod0
  result[i,7:9] <- temp$missing

  print(i)
}

result[,5] <- 24
result[,6] <- 24

result <- as.data.frame(result)

apply(result[,c(1:3)],2,range)
range(result$Duration0)
# save
# save(result, count_matrix, file="pod_result.rda" )

